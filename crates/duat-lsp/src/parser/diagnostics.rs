use duat_base::widgets::GutterBuffer;
use duat_core::{Ns, context, txt};
use lsp_types::{DiagnosticSeverity, PublishDiagnosticsParams};

use crate::uri_to_path;

/// Diagnostics handling struct.
pub struct Diagnostics(Ns);

impl Diagnostics {
    /// Returns a new `Diagnostics`].
    pub fn new() -> Self {
        Self(Ns::new())
    }

    /// Publish a new set of [`Diagnostic`]s for a [`Buffer`].
    ///
    /// [`Diagnostic`]: lsp_types::Diagnostic
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub fn publish_diagnostics(&self, encoding: crate::Encoding, params: PublishDiagnosticsParams) {
        context::debug!("{params:#?}");
        let path = uri_to_path(params.uri);

        let ns = self.0;
        context::queue(move |pa| {
            let Some(buffer) = context::get_buffer_by_path(pa, &path).filter(|b| b.has_gutter(pa))
            else {
                return;
            };

            buffer.remove_gutter_entries(pa, ns);

            for diagnostic in params.diagnostics {
                let text = buffer.text(pa);
                let start = crate::point_from_position(text, diagnostic.range.start, encoding);
                let end = crate::point_from_position(text, diagnostic.range.end, encoding);

                match diagnostic.severity {
                    Some(DiagnosticSeverity::INFORMATION) => {
                        let msg = txt!("[lsp.info]{diagnostic.message}");
                        buffer.add_hint(pa, ns, start..end, msg);
                    }
                    Some(DiagnosticSeverity::HINT) => {
                        let msg = txt!("[lsp.hint]{diagnostic.message}");
                        buffer.add_hint(pa, ns, start..end, msg);
                    }
                    Some(DiagnosticSeverity::WARNING) => {
                        let msg = txt!("[lsp.warn]{diagnostic.message}");
                        buffer.add_warning(pa, ns, start..end, msg);
                    }
                    Some(DiagnosticSeverity::ERROR) => {
                        let msg = txt!("[lsp.error]{diagnostic.message}");
                        buffer.add_error(pa, ns, start..end, msg);
                    }
                    None => _ = buffer.add_hint(pa, ns, start..end, txt!("{diagnostic.message}")),
                    _ => context::error!("Unrecognized severity: [a]{diagnostic.severity:?}"),
                }
            }
        });
    }
}
