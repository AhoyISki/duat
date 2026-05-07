//! A macro made specifically to create the `Text` structs from duat.
//!
//! This macro replaces the `format-like` crate that was depended on
//! previously. It's supposed to compile faster than this older
//! version.
//!
//! This crate also provides a `status!` macro, in order to create
//! `StatusLine`s.
use std::ops::Range;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

/// Creates a `Text` struct from arguments.
#[proc_macro]
pub fn txt(input: TokenStream) -> TokenStream {
    let mut stream = input.into_iter();

    let (str_span, str) = match get_str(stream.next()) {
        Ok(str_and_span) => str_and_span,
        Err(compile_err) => return compile_err,
    };
    let str = str.value();
    let exprs = match get_expresions(stream) {
        Ok(exprs) => exprs,
        Err(compile_err) => return compile_err,
    };

    let mut args = Vec::new();

    let mut arg: Option<CurrentArg> = None;
    let mut unescaped_rhs: Option<(usize, char)> = None;
    let mut push_new_ident = true;
    let mut positional_needed = 0;

    let str_span = |_r: Range<usize>| str_span;

    for (i, char) in str.char_indices() {
        if let Some((j, [lhs, rhs], mut idents, mut modif)) = arg.take() {
            if char == rhs {
                let modif = match modif {
                    Some(range) => unsafe {
                        str::from_utf8_unchecked(&str.as_bytes()[range.clone()])
                    },
                    None => "",
                };

                if idents.is_empty() {
                    if char == ']' {
                        args.push(Arg::Form(j..i + 1, "", modif));
                    } else {
                        positional_needed += 1;
                        args.push(Arg::Positional(j..i + 1, modif));
                    }
                } else if push_new_ident {
                    return compile_err(
                        str_span(i - 1..i),
                        "invalid format string: field access expected an identifier",
                    );
                } else {
                    let mut stream = Vec::new();
                    let len = idents.len();

                    for (i, (range, is_tuple_member)) in idents.into_iter().enumerate() {
                        let str =
                            unsafe { str::from_utf8_unchecked(&str.as_bytes()[range.clone()]) };

                        stream.push(if is_tuple_member {
                            TokenTree::Literal({
                                let mut literal = Literal::usize_unsuffixed(str.parse().unwrap());
                                literal.set_span(str_span(range.clone()));
                                literal
                            })
                        } else {
                            TokenTree::Ident(Ident::new(str, str_span(range.clone())))
                        });

                        if i != len - 1 {
                            stream.push(TokenTree::Punct({
                                let mut punct = Punct::new('.', Spacing::Alone);
                                punct.set_span(str_span(range.end..range.end + 1));
                                punct
                            }));
                        }
                    }

                    if char == ']' {
                        let end = str.as_bytes()[j + 1..i]
                            .iter()
                            .position(|b| *b == b':')
                            .map(|end| end + j + 1)
                            .unwrap_or(i);
                        args.push(Arg::Form(j..i + 1, &str[j + 1..end], modif));
                    } else {
                        args.push(Arg::Inlined(TokenStream::from_iter(stream), modif));
                    }
                }

                continue;
            } else if char == lhs && idents.is_empty() {
                // If arg was empty, that means the delimiter was repeated, so escape
                // it.
                extend_str_arg(&mut args, char, i - 1);
                continue;
            }

            // We might have mismatched delimiters
            if DELIMS.iter().any(|&[lhs, rhs]| char == lhs || char == rhs) {
                return TokenStream::from_iter([
                    compile_err(
                        str_span(i..i + 1),
                        "invalid format string: wrong match for delimiter",
                    ),
                    compile_err(
                        str_span(j..j + 1),
                        format!("from the delimiter {lhs}, expected {rhs}"),
                    ),
                ]);
            } else if char.is_alphanumeric() || char == '_' || modif.is_some() {
                if let Some(modif) = &mut modif {
                    modif.end = i + 1;
                } else if let Some((range, is_tuple_member)) = idents.last_mut()
                    && !push_new_ident
                {
                    *is_tuple_member &= char.is_ascii_digit();
                    range.end = i + 1;
                } else {
                    idents.push((i..i + 1, char.is_ascii_digit()));
                    push_new_ident = false;
                }
            } else if char == '.' {
                if let Some(modif) = &mut modif {
                    modif.end = i + 1;
                } else if push_new_ident {
                    // Can't start an identifier list with '.' or put multiple '.'s in a
                    // row.
                    return compile_err(
                        str_span(i..i + 1),
                        "invalid format string: unexpected '.' here",
                    );
                } else {
                    push_new_ident = true;
                }
            } else if char == ':' {
                if let Some(modif) = &mut modif {
                    modif.end = i + 1;
                } else {
                    modif = Some(i + 1..i + 1);
                }
            } else {
                return compile_err(
                    str_span(i..i + 1),
                    format!("invalid format string: unexpected {char} here"),
                );
            }

            arg = Some((j, [lhs, rhs], idents, modif));
        } else if let Some([lhs, rhs]) = DELIMS
            .into_iter()
            .find(|del| char == del[0] || char == del[1])
        {
            // If the char is a left delimiter, begin an argument.
            // If it is a right delimiter, handle dangling right parameter
            // scenarios.
            if char == lhs {
                push_new_ident = true;
                arg = Some((i, [lhs, rhs], Vec::new(), None));
            } else if let Some((j, unescaped)) = unescaped_rhs {
                // Double delimiters are escaped.
                if char == unescaped {
                    unescaped_rhs = None;
                    extend_str_arg(&mut args, char, i);
                } else {
                    return compile_err(
                        str_span(j..j + 1),
                        format!("invalid format string: unmatched {unescaped} found"),
                    );
                }
            } else {
                unescaped_rhs = Some((i, char));
            }
        } else if let Some((j, unescaped)) = unescaped_rhs {
            return compile_err(
                str_span(j..j + 1),
                format!("invalid format string: unmatched {unescaped} found"),
            );
        } else {
            extend_str_arg(&mut args, char, i);
        }
    }

    if let Some((i, unescaped)) = unescaped_rhs {
        return compile_err(
            str_span(i..i + 1),
            format!("invalid format string: unmatched {unescaped} found"),
        );
    }

    let positional_provided = exprs.len();
    let mut exprs = exprs.into_iter();

    let mut tokens = {
        let mut tokens = vec![ident("use")];
        extend_with_path(&mut tokens, false, &["duat", "text", "AsBuilderPart"]);

        tokens.extend([
            punct(';'),
            ident("let"),
            ident("mut"),
            ident("builder"),
            punct('='),
        ]);

        extend_with_path(&mut tokens, false, &["duat", "text", "Text", "builder"]);

        tokens.extend([fn_args([]), punct(';')]);

        tokens
    };

    for arg in args {
        match arg {
            Arg::Str(string, range) => {
                let mut str = Literal::string(&string);
                str.set_span(str_span(range));

                tokens.extend([
                    ident("builder"),
                    punct('.'),
                    ident("push_str"),
                    fn_args([TokenTree::Literal(str)]),
                    punct(';'),
                ]);
            }
            Arg::Positional(range, modif) => {
                if let Some(expr) = exprs.next() {
                    extend_with_expr(&mut tokens, modif, expr);
                } else {
                    let npl = if positional_needed == 1 { "" } else { "s" };
                    let pverb = if positional_provided == 1 {
                        "is"
                    } else {
                        "are"
                    };
                    let ppl = if positional_provided == 1 { "" } else { "s" };

                    return compile_err(
                        str_span(range),
                        format!(
                            "{positional_needed} positional argument{npl} in format string, but there {pverb} {positional_provided} argument{ppl}"
                        ),
                    );
                }
            }
            Arg::Inlined(idents, modif) => extend_with_expr(&mut tokens, modif, idents),
            Arg::Form(range, form_name, modif) => tokens.extend([
                ident("builder"),
                punct('.'),
                ident("push"),
                fn_args([block({
                    let mut tokens = vec![ident("const"), ident("PRIORITY"), punct(':')];
                    extend_with_path(&mut tokens, true, &["core", "primitive", "u8"]);
                    tokens.push(punct('='));

                    extend_with_path(&mut tokens, false, &["duat", "priority"]);
                    tokens.extend([
                        fn_args([literal(modif)]),
                        punct(';'),
                        ident("let"),
                        ident("id"),
                        punct('='),
                    ]);

                    if form_name.is_empty() {
                        extend_with_path(&mut tokens, false, &["duat", "form", "DEFAULT_ID"]);
                    } else if form_name == "a" {
                        extend_with_path(&mut tokens, false, &["duat", "form", "ACCENT_ID"]);
                    } else {
                        extend_with_path(&mut tokens, false, &["duat", "form", "id_of"]);
                        tokens.extend([
                            punct('!'),
                            fn_args([TokenTree::Literal({
                                let mut literal = Literal::string(form_name);
                                literal.set_span(str_span(range));
                                literal
                            })]),
                        ]);
                    }

                    tokens.extend([
                        punct(';'),
                        ident("id"),
                        punct('.'),
                        ident("to_tag"),
                        fn_args([ident("PRIORITY")]),
                    ]);

                    tokens
                })]),
                punct(';'),
            ]),
        }
    }

    // There should be no positional arguments left.
    if let Some(expr) = exprs.next() {
        return compile_err(
            expr.into_iter().next().unwrap().span(),
            "argument never used",
        );
    }

    TokenStream::from_iter([block(tokens.into_iter().chain([
        ident("builder"),
        punct('.'),
        ident("build"),
        fn_args([]),
    ]))])
}

fn extend_with_expr(tokens: &mut Vec<TokenTree>, modif: &str, expr: TokenStream) {
    if modif.is_empty() {
        tokens.extend([
            ident("builder"),
            punct('.'),
            ident("push_builder_part"),
            fn_args([
                fn_args([punct('&')].into_iter().chain(expr)),
                punct('.'),
                ident("as_builder_part"),
                fn_args([]),
            ]),
            punct(';'),
        ]);
    } else {
        let modif: &str = modif;
        tokens.extend([
            ident("builder"),
            punct('.'),
            ident("push_str"),
            fn_args([
                ident("format"),
                punct('!'),
                fn_args(
                    [literal(&format!("{{:{modif}}}")), punct(',')]
                        .into_iter()
                        .chain(expr),
                ),
            ]),
            punct(';'),
        ]);
    }
}

fn get_expresions(
    mut stream: proc_macro::token_stream::IntoIter,
) -> Result<Vec<TokenStream>, TokenStream> {
    Ok(match stream.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
            let mut exprs = Vec::new();

            let mut tokens = Vec::new();
            let mut on_closure_args = false;

            for token in stream {
                if let TokenTree::Punct(punct) = &token
                    && (punct.as_char() == ',' && !on_closure_args)
                {
                    if !tokens.is_empty() {
                        exprs.push(TokenStream::from_iter(tokens.drain(..)));
                    }
                } else {
                    tokens.push(token);
                    on_closure_args = match tokens.as_slice() {
                        [before @ .., TokenTree::Punct(punct)] if punct.as_char() == '|' => {
                            match before {
                                [] => true,
                                [.., TokenTree::Punct(punct)] => punct.as_char() != '|',
                                [.., TokenTree::Ident(ident)]
                                    if ["move", "async"].contains(&ident.to_string().as_str()) =>
                                {
                                    true
                                }
                                _ => false,
                            }
                        }
                        _ => on_closure_args,
                    }
                }
            }

            if !tokens.is_empty() {
                exprs.push(TokenStream::from_iter(tokens));
            }

            exprs
        }
        Some(other) => return Err(compile_err(other.span(), "expected a comma")),
        None => Vec::new(),
    })
}

fn get_str(token: Option<TokenTree>) -> Result<(Span, litrs::StringLit<String>), TokenStream> {
    let (str, str_span) = match token {
        Some(TokenTree::Literal(literal)) => match litrs::StringLit::parse(literal.to_string()) {
            Ok(str) => (str, literal.span()),
            Err(err) => return Err(compile_err(literal.span(), format!("{err}"))),
        },
        Some(other) => return Err(compile_err(other.span(), "expected a string literal")),
        None => return Err(compile_err(Span::mixed_site(), "expected a string literal")),
    };
    Ok((str_span, str))
}

enum Arg<'s> {
    Str(String, Range<usize>),
    Positional(Range<usize>, &'s str),
    Inlined(TokenStream, &'s str),
    Form(Range<usize>, &'s str, &'s str),
}

fn extend_str_arg(args: &mut Vec<Arg>, char: char, i: usize) {
    if let Some(Arg::Str(string, range)) = args.last_mut() {
        string.push(char);
        range.end = i + char.len_utf8();
    } else {
        args.push(Arg::Str(char.to_string(), i..i + char.len_utf8()))
    }
}

fn compile_err(span: Span, msg: impl std::fmt::Display) -> TokenStream {
    let (start, end) = (span.start(), span.end());

    TokenStream::from_iter([
        TokenTree::Punct({
            let mut punct = Punct::new(':', Spacing::Joint);
            punct.set_span(start);
            punct
        }),
        TokenTree::Punct({
            let mut punct = Punct::new(':', Spacing::Alone);
            punct.set_span(start);
            punct
        }),
        TokenTree::Ident(Ident::new("core", start)),
        TokenTree::Punct({
            let mut punct = Punct::new(':', Spacing::Joint);
            punct.set_span(start);
            punct
        }),
        TokenTree::Punct({
            let mut punct = Punct::new(':', Spacing::Alone);
            punct.set_span(start);
            punct
        }),
        TokenTree::Ident({
            let mut ident = Ident::new("compile_error", start);
            ident.set_span(start);
            ident
        }),
        TokenTree::Punct({
            let mut punct = Punct::new('!', Spacing::Alone);
            punct.set_span(start);
            punct
        }),
        TokenTree::Group({
            let mut group = Group::new(Delimiter::Brace, {
                TokenStream::from_iter([TokenTree::Literal({
                    let mut string = Literal::string(&msg.to_string());
                    string.set_span(end);
                    string
                })])
            });
            group.set_span(end);
            group
        }),
    ])
}

fn extend_with_path(tokens: &mut Vec<TokenTree>, is_abs: bool, idents: &[&'static str]) {
    let colon = TokenTree::Punct(Punct::new(':', Spacing::Joint));

    for (i, ident) in idents.iter().enumerate() {
        if i > 0 || is_abs {
            tokens.push(colon.clone());
            tokens.push(colon.clone())
        }

        tokens.push(TokenTree::Ident(Ident::new(ident, Span::mixed_site())));
    }
}

fn ident(ident: &'static str) -> TokenTree {
    TokenTree::Ident(Ident::new(ident, Span::mixed_site()))
}

fn punct(char: char) -> TokenTree {
    TokenTree::Punct(Punct::new(char, Spacing::Alone))
}

fn literal(literal: &str) -> TokenTree {
    TokenTree::Literal({
        let mut literal = Literal::string(literal);
        literal.set_span(Span::mixed_site());
        literal
    })
}

fn fn_args(tokens: impl IntoIterator<Item = TokenTree>) -> TokenTree {
    TokenTree::Group(Group::new(
        Delimiter::Parenthesis,
        tokens.into_iter().collect(),
    ))
}

fn block(tokens: impl IntoIterator<Item = TokenTree>) -> TokenTree {
    TokenTree::Group(Group::new(Delimiter::Brace, tokens.into_iter().collect()))
}

type CurrentArg = (
    usize,
    [char; 2],
    Vec<(Range<usize>, bool)>,
    Option<Range<usize>>,
);

const DELIMS: [[char; 2]; 2] = [['{', '}'], ['[', ']']];
