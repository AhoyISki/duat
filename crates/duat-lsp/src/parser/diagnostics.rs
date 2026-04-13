use std::{
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use duat_base::widgets::{GutterBuffer, GutterEntryId};
use duat_core::{
    Ns,
    context::{self, Handle},
    data::Pass,
    form,
    hook::{self, ConfigUnloaded},
    storage::{self, bincode},
    text::{Mask, RegexHaystack, Strs, Text},
    txt,
};
use lsp_types::{Diagnostic, DiagnosticSeverity, Uri};

use crate::{Encoding, path_to_uri, server::Server, uri_to_path};

static DIAGNOSTICS: LazyLock<Mutex<Diagnostics>> = LazyLock::new(|| {
    hook::add::<ConfigUnloaded>(|pa, is_quitting| {
        if !is_quitting {
            let diagnostics = std::mem::take(&mut *DIAGNOSTICS.lock().unwrap());
            _ = storage::store(pa, diagnostics);
        }
    });

    Mutex::new(storage::get_if(|_| true).unwrap_or_default())
});

/// Handles a list of diagnostics.
pub fn add(pa: &mut Pass, ns: Ns, uri: Uri, mut list: Vec<Diagnostic>, encoding: Encoding) {
    list.sort_unstable_by(|lhs, rhs| {
        lhs.severity
            .cmp(&rhs.severity)
            .then(lhs.range.start.cmp(&rhs.range.start))
            .then(lhs.range.end.cmp(&rhs.range.end))
    });

    let mut diagnostics = DIAGNOSTICS.lock().unwrap();
    let added = added_for(&mut diagnostics, ns, &uri);
    added.clear();

    let Some(buffer) = context::buffer_from_path(pa, &uri_to_path(uri.clone()))
        .filter(|buffer| buffer.has_gutter(pa))
    else {
        *added = list
            .into_iter()
            .map(|diagnostic| Entry::Diagnostic(None, diagnostic))
            .collect();
        return;
    };

    buffer.remove_gutter_entries(pa, ns);

    for diagnostic in list {
        let text = buffer.text(pa);
        let range = byte_range(text, diagnostic.range, encoding);

        let entry_id = match diagnostic.severity {
            Some(DiagnosticSeverity::INFORMATION) => {
                let mut msg = format_message(&diagnostic.message);
                msg.insert_tag(Ns::basic(), .., Mask("info"));
                buffer.add_hint(pa, ns, range, msg)
            }
            Some(DiagnosticSeverity::HINT) => {
                let mut msg = format_message(&diagnostic.message);
                msg.insert_tag(Ns::basic(), .., Mask("info"));
                buffer.add_hint(pa, ns, range, msg)
            }
            Some(DiagnosticSeverity::WARNING) => {
                let mut msg = format_message(&diagnostic.message);
                msg.insert_tag(Ns::basic(), .., Mask("warn"));
                buffer.add_warning(pa, ns, range, msg)
            }
            Some(DiagnosticSeverity::ERROR) => {
                let mut msg = format_message(&diagnostic.message);
                msg.insert_tag(Ns::basic(), .., Mask("error"));
                buffer.add_error(pa, ns, range, msg)
            }
            None => {
                let msg = format_message(&diagnostic.message);
                buffer.add_hint(pa, ns, range, msg)
            }
            _ => {
                context::error!("Unrecognized severity: [a]{diagnostic.severity:?}");
                continue;
            }
        };

        for related in diagnostic.related_information.iter().flatten() {
            // Avoid adding reduntant entries.
            if related.message == "original diagnostic" {
                continue;
            }

            let added = added_for(&mut diagnostics, ns, &related.location.uri);
            let entry = Entry::Related(None, related.location.range, related.message.clone());

            let entry = if let Some(entry) = added.iter_mut().find(|other| **other == entry) {
                entry
            } else {
                added.push(entry);
                added.last_mut().unwrap()
            };

            let Some(buffer) =
                context::buffer_from_path(pa, &uri_to_path(related.location.uri.clone()))
                    .filter(|buffer| buffer.has_gutter(pa))
            else {
                entry.set_id(None);
                continue;
            };

            let text = buffer.text(pa);
            let mut msg = format_message(&related.message);
            let range = byte_range(text, related.location.range, encoding);

            msg.insert_tag(Ns::basic(), .., Mask("info"));
            entry.set_id(Some(buffer.add_hint(pa, ns, range, msg)));
        }

        added_for(&mut diagnostics, ns, &uri).push(Entry::Diagnostic(Some(entry_id), diagnostic));
    }

    for (_, entries) in diagnostics.0[&ns].iter() {
        for entry in entries.iter() {
            let Entry::Diagnostic(Some(id), diagnostic) = entry else {
                continue;
            };
            let Some(related) = diagnostic
                .related_information
                .as_ref()
                .filter(|list| !list.is_empty())
            else {
                continue;
            };

            let iter = related.iter().filter_map(|related| {
                let entries = diagnostics.0[&ns].get(&related.location.uri)?;
                let entry = entries.iter().find(|entry| {
                    entry.range() == related.location.range && entry.message() == related.message
                })?;

                entry.id()
            });

            id.relate_with_other_entries(iter);
        }
    }
}

/// Adds the initial diagnostics for a [`Buffer`]
///
/// [`Buffer`]: duat_core::buffer::Buffer
pub fn add_initial(pa: &mut Pass, servers: &[Server], buffer: &Handle) {
    let mut diagnostics = DIAGNOSTICS.lock().unwrap();

    let Some(uri) = path_to_uri(&buffer.read(pa).path()) else {
        return;
    };

    let mut to_add = Vec::new();

    for (ns, entries) in diagnostics.0.iter_mut() {
        let encoding = Encoding::new(
            servers
                .iter()
                .find(|server| server.ns() == *ns)
                .unwrap()
                .capabilities()
                .unwrap(),
        );

        let Some(entries) = entries.get_mut(&uri) else {
            continue;
        };

        let list = Vec::from_iter(entries.drain(..).filter_map(|entry| match entry {
            Entry::Diagnostic(_, diagnostic) => Some(diagnostic),
            Entry::Related(..) => None,
        }));

        if !list.is_empty() {
            to_add.push((list, *ns, encoding));
        }
    }

    drop(diagnostics);

    for (list, ns, encoding) in to_add {
        add(pa, ns, uri.clone(), list, encoding);
    }
}

fn format_message(msg: &str) -> Text {
    let tt_ranges = Vec::from_iter(msg.search("`.+?`"));

    let mut msg = txt!("[lsp]{msg}");
    let tt_form = form::id_of!("lsp.tt").to_tag(100);

    for range in tt_ranges.into_iter().rev() {
        msg.replace_range(range.end - 1..range.end, "");
        msg.replace_range(range.start..range.start + 1, "");

        msg.insert_tag(Ns::basic(), range.start..range.end - 2, tt_form);
    }

    msg
}

fn byte_range(strs: &Strs, range: lsp_types::Range, encoding: Encoding) -> Range<usize> {
    let start = crate::point_from_position(strs, range.start, encoding);
    let end = crate::point_from_position(strs, range.end, encoding);

    start.byte()..end.byte()
}

// This is fine, because fluent_uri doesn't actually internally
// mutate.
#[allow(clippy::mutable_key_type)]
fn added_for<'e>(diagnostics: &'e mut Diagnostics, ns: Ns, uri: &Uri) -> &'e mut Vec<Entry> {
    let buffer_diagnostics = diagnostics.0.entry(ns).or_default();
    if buffer_diagnostics.contains_key(uri) {
        buffer_diagnostics.get_mut(uri).unwrap()
    } else {
        buffer_diagnostics.entry(uri.clone()).or_default()
    }
}

type ServerDiagnostics = HashMap<Uri, Vec<Entry>>;

#[derive(Debug, Default)]
struct Diagnostics(HashMap<Ns, ServerDiagnostics>);

impl<Context> bincode::Decode<Context> for Diagnostics {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        let servers = Vec::<(Ns, Vec<(String, Vec<Entry>)>)>::decode(decoder)?;
        Ok(Self(HashMap::from_iter(servers.into_iter().map(
            |(ns, list)| {
                (
                    ns,
                    HashMap::from_iter(
                        list.into_iter()
                            .map(|(uri, entries)| (uri.parse().unwrap(), entries)),
                    ),
                )
            },
        ))))
    }
}

impl bincode::Encode for Diagnostics {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        let servers = Vec::from_iter(self.0.iter().map(|(ns, list)| {
            (
                *ns,
                Vec::from_iter(list.iter().map(|(uri, entries)| (uri.to_string(), entries))),
            )
        }));
        servers.encode(encoder)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq)]
enum Entry {
    Diagnostic(Option<GutterEntryId>, Diagnostic),
    Related(Option<GutterEntryId>, lsp_types::Range, String),
}

impl Entry {
    fn message(&self) -> &str {
        match self {
            Entry::Diagnostic(_, diagnostic) => &diagnostic.message,
            Entry::Related(.., message) => message,
        }
    }

    fn range(&self) -> lsp_types::Range {
        match self {
            Entry::Diagnostic(_, diagnostic) => diagnostic.range,
            Entry::Related(_, range, _) => *range,
        }
    }

    fn id(&self) -> Option<GutterEntryId> {
        match self {
            Entry::Diagnostic(id, _) => *id,
            Entry::Related(id, ..) => *id,
        }
    }

    fn set_id(&mut self, new_value: Option<GutterEntryId>) {
        match self {
            Entry::Diagnostic(id, _) => *id = new_value,
            Entry::Related(id, ..) => *id = new_value,
        }
    }
}

impl<Context> bincode::Decode<Context> for Entry {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        let variant = u8::decode(decoder)?;
        if variant == 0 {
            let diagnostic = String::decode(decoder)?;
            Ok(Self::Diagnostic(
                None,
                serde_json::from_str(&diagnostic).unwrap(),
            ))
        } else {
            let range = String::decode(decoder)?;
            let message = String::decode(decoder)?;
            Ok(Self::Related(
                None,
                serde_json::from_str(&range).unwrap(),
                message,
            ))
        }
    }
}

impl bincode::Encode for Entry {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        match self {
            Entry::Diagnostic(_, diagnostic) => {
                0u8.encode(encoder)?;
                serde_json::to_string(diagnostic).unwrap().encode(encoder)
            }
            Entry::Related(_, range, message) => {
                1u8.encode(encoder)?;
                serde_json::to_string(range).unwrap().encode(encoder)?;
                message.encode(encoder)
            }
        }
    }
}
