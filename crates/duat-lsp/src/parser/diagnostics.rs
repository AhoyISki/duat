use std::{
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use duat_base::widgets::{GutterBuffer, GutterEntryId};
use duat_core::{
    Ns,
    buffer::Moment,
    context::{self, Handle},
    data::Pass,
    form,
    hook::{self, BufferOpened, ConfigUnloaded},
    storage::{self, bincode},
    text::{Mask, RegexHaystack, Strs, Text},
    txt,
};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Uri};

use crate::{Encoding, parser::PARSERS, path_to_uri, server::Server, uri_to_path};

static NS: LazyLock<Ns> = Ns::new_lazy();
static DIAGNOSTICS: LazyLock<Mutex<Diagnostics>> = LazyLock::new(|| {
    hook::add::<BufferOpened>(|pa, buffer| _ = buffer.read(pa).moment_for(*NS));
    hook::add::<ConfigUnloaded>(|pa, is_quitting| {
        if is_quitting {
            return;
        }

        let mut moments = HashMap::new();
        let mut diagnostics = std::mem::take(&mut *DIAGNOSTICS.lock().unwrap());

        let get_parts = |pa: &mut Pass, server_ns, uri| {
            let buffer = context::buffer_from_path(pa, &uri_to_path(uri))?;
            let (parser, _) = PARSERS.write(pa, &buffer)?;
            let encoding = parser
                .servers
                .iter()
                .find(|server| server.ns() == server_ns)
                .and_then(|server| server.capabilities().map(Encoding::new))?;

            Some((buffer, encoding))
        };

        for (&server_ns, (_, buffer_entries)) in diagnostics.0.iter_mut() {
            for (uri, entries) in buffer_entries.iter_mut() {
                // I assume here that, when the Buffer isn't found, it means it hasn't been changed and saved. Otherwise, LSPs like rust-analyzer would've sent new diagnostics.
                let Some((buffer, encoding)) = get_parts(pa, server_ns, uri.clone()) else {
                    continue;
                };

                let ranges = entries
                    .iter_mut()
                    .enumerate()
                    .map(|(i, entry)| (i, entry.range_mut()));
                let buf = buffer.read(pa);

                let moment = moments
                    .entry(buf.path())
                    .or_insert_with(|| buf.moment_for(*NS));

                let to_remove = apply_changes(ranges, moment, buf.text(), encoding);
                for idx in to_remove.into_iter().rev() {
                    entries.remove(idx);
                }

                // Shifting the related entries as well 😒.
                for entry in entries {
                    let Entry::Diagnostic(_, diagnostic, _) = entry else {
                        continue;
                    };
                    let Some(related) = diagnostic.related_information.as_mut() else {
                        continue;
                    };

                    let related_uris = related.iter().fold(Vec::new(), |mut uris, related| {
                        if uris.contains(&related.location.uri) {
                            uris
                        } else {
                            uris.push(related.location.uri.clone());
                            uris
                        }
                    });

                    for uri in related_uris {
                        let Some((buffer, encoding)) = get_parts(pa, server_ns, uri.clone()) else {
                            continue;
                        };
                        let buf = buffer.read(pa);

                        let moment = moments
                            .entry(buf.path())
                            .or_insert_with(|| buf.moment_for(*NS));

                        let ranges = related
                            .iter_mut()
                            .enumerate()
                            .filter(|(_, related)| related.location.uri == uri)
                            .map(|(i, related)| (i, &mut related.location.range));

                        let to_remove = apply_changes(ranges, moment, buf.text(), encoding);
                        for idx in to_remove.into_iter().rev() {
                            related.remove(idx);
                        }
                    }
                }
            }
        }

        _ = storage::store(pa, diagnostics);
    });

    Mutex::new(storage::get_if(|_| true).unwrap_or_default())
});

/// Handles a list of diagnostics.
pub fn add(pa: &mut Pass, server_ns: Ns, uri: Uri, mut list: Vec<Diagnostic>, encoding: Encoding) {
    list.sort_unstable_by(|lhs, rhs| {
        lhs.severity
            .cmp(&rhs.severity)
            .then(lhs.range.start.cmp(&rhs.range.start))
            .then(lhs.range.end.cmp(&rhs.range.end))
    });

    let mut diagnostics = DIAGNOSTICS.lock().unwrap();
    let (ns, added) = added_for(&mut diagnostics, server_ns, &uri);
    added.clear();

    let Some(buffer) = context::buffer_from_path(pa, &uri_to_path(uri.clone()))
        .filter(|buffer| buffer.has_gutter(pa))
    else {
        *added = list
            .into_iter()
            .map(|diagnostic| Entry::Diagnostic(None, diagnostic, None))
            .collect();
        return;
    };

    buffer.remove_gutter_entries(pa, ns);

    for diagnostic in list {
        let text = buffer.text(pa);
        let range = get_byte_range(text, diagnostic.range, encoding);

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

            let (ns, added) = added_for(&mut diagnostics, server_ns, &related.location.uri);
            let entry = Entry::Related(None, related.message.clone(), related.location.range, None);

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
            let range = get_byte_range(text, related.location.range, encoding);

            msg.insert_tag(Ns::basic(), .., Mask("info"));
            entry.set_id(Some(buffer.add_hint(pa, ns, range, msg)));
        }

        let (_, added) = added_for(&mut diagnostics, server_ns, &uri);
        let byte_range = get_byte_range(buffer.text(pa), diagnostic.range, encoding);
        added.push(Entry::Diagnostic(
            Some(entry_id),
            diagnostic,
            Some(byte_range),
        ));
    }

    for (_, entries) in diagnostics.0[&server_ns].1.iter() {
        for entry in entries.iter() {
            let Entry::Diagnostic(Some(id), diagnostic, _) = entry else {
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
                let entries = diagnostics.0[&server_ns].1.get(&related.location.uri)?;
                let entry = entries.iter().find(|entry| {
                    entry.range() == related.location.range && entry.message() == related.message
                })?;

                entry.id()
            });

            id.relate_with_other_entries(iter);
        }
    }

    // Declare that the current entries are correctly shifted.
    let moment = buffer.read(pa).moment_for(*NS);
    if moment.is_empty() || diagnostics.0.len() == 1 {
        return;
    }
    
    let other_servers = Vec::from_iter(
        diagnostics
            .0
            .iter()
            .filter_map(|(other_ns, _)| (*other_ns != server_ns).then_some(*other_ns)),
    );

    let text = buffer.text(pa);
    for server_ns in other_servers {
        let (_, added) = added_for(&mut diagnostics, server_ns, &uri);
        let ranges = added
            .iter_mut()
            .enumerate()
            .map(|(i, entry)| (i, entry.range_mut()));
        let to_remove = apply_changes(ranges, &moment, text, encoding);
        for idx in to_remove.into_iter().rev() {
            added.remove(idx);
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

    for (server_ns, (_, list)) in diagnostics.0.iter_mut() {
        let encoding = Encoding::new(
            servers
                .iter()
                .find(|server| server.ns() == *server_ns)
                .unwrap()
                .capabilities()
                .unwrap(),
        );

        let Some(entries) = list.get_mut(&uri) else {
            continue;
        };

        let list = Vec::from_iter(entries.drain(..).filter_map(|entry| match entry {
            Entry::Diagnostic(_, diagnostic, _) => Some(diagnostic),
            Entry::Related(..) => None,
        }));

        if !list.is_empty() {
            to_add.push((list, *server_ns, encoding));
        }
    }

    drop(diagnostics);

    for (list, server_ns, encoding) in to_add {
        add(pa, server_ns, uri.clone(), list, encoding);
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

fn apply_changes<'d>(
    mut ranges: impl Iterator<Item = (usize, &'d mut lsp_types::Range)>,
    moment: &Moment,
    strs: &Strs,
    encoding: Encoding,
) -> Vec<usize> {
    let sh = |pos: &mut Position, shift: [i32; 2], last_line: u32| {
        if last_line == pos.line {
            pos.character = pos.character.saturating_add_signed(shift[1]);
        }
        pos.line = pos.line.saturating_add_signed(shift[0]);
    };

    let (mut shift, mut last_line) = ([0; 2], 0);
    let mut to_remove = Vec::new();

    for change in moment.iter() {
        let start = encoding.pos_from_point(strs, change.start());
        let taken_end = encoding.taken_end_pos(start, change.taken_end(), change.taken_str());
        let added_end = encoding.pos_from_point(strs, change.added_end());

        let new_shift = [
            added_end.line as i32 - taken_end.line as i32,
            added_end.character as i32 - taken_end.character as i32,
        ];

        let mut is_contained = |i: usize, range: lsp_types::Range| {
            if start <= range.start && range.start < taken_end
                || taken_end <= range.end && range.end < taken_end
            {
                to_remove.push(i);
                true
            } else {
                false
            }
        };

        if let Some((_, range)) = ranges.find_map(|(i, range)| {
            sh(&mut range.start, shift, last_line);
            sh(&mut range.end, shift, last_line);

            (!is_contained(i, *range) && range.end > start).then_some((i, range))
        }) {
            let start_shift = new_shift.map(|val| val * (range.start > start) as i32);

            sh(&mut range.start, start_shift, taken_end.line);
            sh(&mut range.end, new_shift, taken_end.line);
        }

        shift[0] += new_shift[0];
        if taken_end.line == last_line {
            shift[1] += new_shift[1];
        } else {
            shift[1] = new_shift[1];
        }

        last_line = taken_end.line;
    }

    for (_, range) in ranges {
        sh(&mut range.start, shift, last_line);
        sh(&mut range.end, shift, last_line);
    }

    to_remove
}

#[track_caller]
fn get_byte_range(strs: &Strs, range: lsp_types::Range, encoding: Encoding) -> Range<usize> {
    let start = encoding.byte_from_pos(strs, range.start);
    let end = encoding.byte_from_pos(strs, range.end);
    start..end
}

// This is fine, because fluent_uri doesn't actually internally
// mutate.
#[allow(clippy::mutable_key_type)]
fn added_for<'e>(diagnostics: &'e mut Diagnostics, ns: Ns, uri: &Uri) -> (Ns, &'e mut Vec<Entry>) {
    let (ns, buffer_diagnostics) = diagnostics.0.entry(ns).or_default();
    if buffer_diagnostics.contains_key(uri) {
        (*ns, buffer_diagnostics.get_mut(uri).unwrap())
    } else {
        (*ns, buffer_diagnostics.entry(uri.clone()).or_default())
    }
}

type ServerDiagnostics = (Ns, HashMap<Uri, Vec<Entry>>);

#[derive(Debug, Default)]
struct Diagnostics(HashMap<Ns, ServerDiagnostics>);

impl<Context> bincode::Decode<Context> for Diagnostics {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        let servers = Vec::<(Ns, (Ns, Vec<(String, Vec<Entry>)>))>::decode(decoder)?;
        Ok(Self(HashMap::from_iter(servers.into_iter().map(
            |(server_ns, (ns, list))| {
                (
                    server_ns,
                    (
                        ns,
                        HashMap::from_iter(
                            list.into_iter()
                                .map(|(uri, entries)| (uri.parse().unwrap(), entries)),
                        ),
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
        let servers = Vec::from_iter(self.0.iter().map(|(server_ns, (ns, list))| {
            (
                *server_ns,
                (
                    *ns,
                    Vec::from_iter(list.iter().map(|(uri, entries)| (uri.to_string(), entries))),
                ),
            )
        }));
        servers.encode(encoder)
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Eq)]
enum Entry {
    Diagnostic(Option<GutterEntryId>, Diagnostic, Option<Range<usize>>),
    Related(
        Option<GutterEntryId>,
        String,
        lsp_types::Range,
        Option<Range<usize>>,
    ),
}

impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Diagnostic(_, l1, l2), Self::Diagnostic(_, r1, r2)) => l1 == r1 && l2 == r2,
            (Self::Related(_, l1, l2, l3), Self::Related(_, r1, r2, r3)) => {
                l1 == r1 && l2 == r2 && l3 == r3
            }
            _ => false,
        }
    }
}

impl Entry {
    fn message(&self) -> &str {
        match self {
            Entry::Diagnostic(_, diagnostic, _) => &diagnostic.message,
            Entry::Related(_, message, ..) => message,
        }
    }

    fn range(&self) -> lsp_types::Range {
        match self {
            Entry::Diagnostic(.., diagnostic, _) => diagnostic.range,
            Entry::Related(.., range, _) => *range,
        }
    }

    fn range_mut(&mut self) -> &mut lsp_types::Range {
        match self {
            Entry::Diagnostic(.., diagnostic, _) => &mut diagnostic.range,
            Entry::Related(.., old_range, _) => old_range,
        }
    }

    fn id(&self) -> Option<GutterEntryId> {
        match self {
            Entry::Diagnostic(id, ..) => *id,
            Entry::Related(id, ..) => *id,
        }
    }

    fn set_id(&mut self, new_value: Option<GutterEntryId>) {
        match self {
            Entry::Diagnostic(id, ..) => *id = new_value,
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
            let byte_range = Option::decode(decoder)?;
            Ok(Self::Diagnostic(
                None,
                serde_json::from_str(&diagnostic).unwrap(),
                byte_range,
            ))
        } else {
            let message = String::decode(decoder)?;
            let range = String::decode(decoder)?;
            let byte_range = Option::decode(decoder)?;
            Ok(Self::Related(
                None,
                message,
                serde_json::from_str(&range).unwrap(),
                byte_range,
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
            Entry::Diagnostic(_, diagnostic, byte_range) => {
                0u8.encode(encoder)?;
                serde_json::to_string(diagnostic).unwrap().encode(encoder)?;
                byte_range.encode(encoder)
            }
            Entry::Related(_, message, range, byte_range) => {
                1u8.encode(encoder)?;
                message.encode(encoder)?;
                serde_json::to_string(range).unwrap().encode(encoder)?;
                byte_range.encode(encoder)
            }
        }
    }
}
