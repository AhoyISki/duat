use std::{collections::HashSet, ops::Range};

use duat_base::widgets::{GutterBuffer, GutterEntryId};
use duat_core::{
    Ns,
    context::{self},
    data::Pass,
    form,
    text::{Mask, RegexHaystack, Strs, Text},
    txt,
};
use lsp_types::{Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, Uri};

use crate::{Encoding, uri_to_path};

macro_rules! ns_and_buffer {
    ($pa:expr, $uri:expr, $kw:ident) => {{
        let Some(buffer) = context::get_buffer_by_path($pa, &uri_to_path(($uri).clone())) else {
            $kw;
        };
        if !buffer.has_gutter($pa) {
            $kw;
        }

        let Some((parser, _)) = super::PARSERS.write($pa, &buffer) else {
            $kw;
        };

        (parser.diagnostics.ns, buffer)
    }};
}

/// Diagnostics handling struct.
pub struct Diagnostics {
    ns: Ns,
    result_id: Option<String>,
    current: Vec<CurrentDiagnostic>,
}

impl Diagnostics {
    /// Returns a new `Diagnostics`].
    pub fn new() -> Self {
        Self {
            ns: Ns::new(),
            result_id: None,
            current: Vec::new(),
        }
    }

    /// Handles the [`Diagnostic`]s of [`PublishDiagnosticsParams`].
    pub fn handle_published(
        &mut self,
        params: PublishDiagnosticsParams,
        encoding: Encoding,
    ) -> impl FnOnce(&mut Pass) + 'static {
        move |pa| {
            handle_diagnostics(pa, params.uri, params.diagnostics, encoding);
        }
    }
}

/// Handles a list of diagnostics.
pub fn handle_diagnostics(pa: &mut Pass, uri: Uri, mut list: Vec<Diagnostic>, encoding: Encoding) {
    let mut already_added = Vec::new();

    list.sort_unstable_by(|lhs, rhs| {
        lhs.severity
            .cmp(&rhs.severity)
            .then(lhs.range.start.cmp(&rhs.range.start))
            .then(lhs.range.end.cmp(&rhs.range.end))
    });

    let (ns, buffer) = ns_and_buffer!(pa, &uri, return);
    buffer.remove_gutter_entries(pa, ns);

    let mut taken_buffers = HashSet::new();
    taken_buffers.insert(buffer.read(pa).buffer_id());

    for diagnostic in &list {
        let text = buffer.text(pa);
        let range = byte_range(text, diagnostic.range, encoding);

        let mut msg = format_message(&diagnostic.message);

        let entry_id = match diagnostic.severity {
            Some(DiagnosticSeverity::INFORMATION) => {
                msg.insert_tag(Ns::basic(), .., Mask("info"));
                buffer.add_hint(pa, ns, range, msg)
            }
            Some(DiagnosticSeverity::HINT) => {
                msg.insert_tag(Ns::basic(), .., Mask("info"));
                buffer.add_hint(pa, ns, range, msg);
                continue;
            }
            Some(DiagnosticSeverity::WARNING) => {
                msg.insert_tag(Ns::basic(), .., Mask("warn"));
                buffer.add_warning(pa, ns, range, msg)
            }
            Some(DiagnosticSeverity::ERROR) => {
                msg.insert_tag(Ns::basic(), .., Mask("error"));
                buffer.add_error(pa, ns, range, msg)
            }
            None => buffer.add_hint(pa, ns, range, msg),
            _ => {
                context::error!("Unrecognized severity: [a]{diagnostic.severity:?}");
                continue;
            }
        };

        let mut other_entries = Vec::new();
        for related in diagnostic.related_information.iter().flatten() {
            let (ns, buffer) = ns_and_buffer!(pa, &related.location.uri, continue);
            let buffer_id = buffer.read(pa).buffer_id();
            if !taken_buffers.contains(&buffer_id) {
                buffer.remove_gutter_entries(pa, ns);
                taken_buffers.insert(buffer_id);
            }

            let text = buffer.text(pa);
            let mut msg = format_message(&related.message);
            let range = byte_range(text, related.location.range, encoding);

            already_added.push((
                &related.location.uri,
                related.location.range,
                &related.message,
            ));

            msg.insert_tag(Ns::basic(), .., Mask("info"));
            other_entries.push(buffer.add_hint(pa, ns, range, msg));
        }

        entry_id.relate_with_other_entries(other_entries);
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

struct CurrentDiagnostic {
    range: lsp_types::Range,
    message: String,
    id: GutterEntryId,
}
