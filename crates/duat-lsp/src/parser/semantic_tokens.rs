use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    form,
    text::{FormTag, Tagger, TextParts},
};
use gap_buf::GapBuffer;
use lsp_types::{
    SemanticToken, SemanticTokens, SemanticTokensDelta, SemanticTokensEdit, SemanticTokensLegend,
};

use crate::server::ServerId;

static TOKEN_MAP: LazyLock<Mutex<HashMap<&str, &str>>> = LazyLock::new(|| {
    Mutex::new(HashMap::from_iter([
        ("struct", "type.struct"),
        ("class", "type.class"),
        ("builtinType", "type.builtin"),
        ("enum", "type.enum"),
        ("enumMember", "type.enum.variant"),
        ("method", "function.method"),
        ("unresolvedReference", "unresolved.reference"),
        ("invalidEscapeSequence", "unresolved.escape_seq"),
        ("escapeSequence", "character.escape_seq"),
        ("namespace", "module"),
        ("macro", "function.macro"),
        ("parameter", "variable.parameter"),
    ]))
});

#[derive(Default)]
pub struct BufferTokens {
    tokens_by_server: HashMap<ServerId, ServerTokens>,
}

struct ServerTokens {
    tagger: Tagger,
    applied: GapBuffer<SemanticToken>,
    forms: Vec<FormTag>,
    result_id: Option<String>,
}

impl BufferTokens {
    /// Applies the initial result, fully replacing the
    /// [`SemanticToken`]s within.
    pub fn apply_full(
        &mut self,
        mut parts: TextParts,
        tokens: SemanticTokens,
        id: ServerId,
        legend: &SemanticTokensLegend,
    ) {
        let server_tokens = self.tokens_by_server.entry(id).or_insert_with(|| {
            let map = TOKEN_MAP.lock().unwrap();

            let forms = legend
                .token_types
                .iter()
                .map(|ty| {
                    let ty = ty.as_str();
                    form::id_of_non_static(map.get(ty).copied().unwrap_or(ty)).to_tag(100)
                })
                .collect();

            ServerTokens {
                tagger: Tagger::new(),
                applied: GapBuffer::new(),
                forms,
                result_id: None,
            }
        });

        let (semantic_tokens, result_id) = (tokens.data, tokens.result_id);

        let mut line = 0;
        let mut byte = 0;

        parts.tags.remove(server_tokens.tagger, ..);

        for token in &semantic_tokens {
            (line, byte) = fwd_pos(line, byte, token);

            let tag = server_tokens.forms[token.token_type as usize];
            let start = parts.strs.point_at_coords(line, 0).byte() + byte;
            parts.tags.insert(
                server_tokens.tagger,
                start..start + token.length as usize,
                tag,
            );
        }

        server_tokens.applied = GapBuffer::from(semantic_tokens);
        server_tokens.result_id = result_id;
    }

    /// Apply a delta to the `BufferTokens`
    pub fn apply_delta(
        &mut self,
        mut parts: TextParts,
        mut delta: SemanticTokensDelta,
        id: ServerId,
    ) {
        let server_tokens = self.tokens_by_server.get_mut(&id).unwrap();

        let mut delta_from = 0;
        let mut line = 0;
        let mut byte = 0;

        delta
            .edits
            .sort_unstable_by(|lhs, rhs| lhs.start.cmp(&rhs.start));

        let mut removed_entries = 0;

        for edit in delta.edits.into_iter() {
            let edit = SemanticTokensEdit {
                start: edit.start + removed_entries * 5,
                ..edit
            };

            let start_index = edit.start as usize / 5;
            let end_index = (edit.start + edit.delete_count) as usize / 5;
            let new_tokens_len = edit.data.as_ref().map(Vec::len).unwrap_or(0);
            removed_entries += (end_index - start_index) as u32;

            for token in &server_tokens.applied.range(delta_from..start_index) {
                (line, byte) = fwd_pos(line, byte, token);
            }

            {
                let mut line = line;
                let mut byte = byte;

                let new_tokens = edit.data.into_iter().flatten();
                for token in server_tokens
                    .applied
                    .splice(start_index..end_index, new_tokens)
                {
                    let start = parts.strs.point_at_coords(line, 0).byte() + byte;
                    if start > parts.strs.len() {
                        break;
                    }

                    parts
                        .tags
                        .remove_excl(server_tokens.tagger, start..start + token.length as usize);

                    (line, byte) = fwd_pos(line, byte, &token);
                }
            }

            {
                let mut line = line;
                let mut byte = byte;

                for token in &server_tokens
                    .applied
                    .range(start_index..start_index + new_tokens_len)
                {
                    (line, byte) = fwd_pos(line, byte, token);

                    let tag = server_tokens.forms[token.token_type as usize];
                    let start = parts.strs.point_at_coords(line, 0).byte() + byte;
                    parts.tags.insert(
                        server_tokens.tagger,
                        start..start + token.length as usize,
                        tag,
                    );
                }
            }

            delta_from = start_index;
        }
    }

    /// The previous result id, used for delta calculation.
    pub fn result_id(&self, server_id: ServerId) -> Option<String> {
        self.tokens_by_server.get(&server_id)?.result_id.clone()
    }
}

fn fwd_pos(mut line: usize, mut byte: usize, token: &SemanticToken) -> (usize, usize) {
    line += token.delta_line as usize;
    byte *= (token.delta_line == 0) as usize;
    byte += token.delta_start as usize;

    (line, byte)
}
