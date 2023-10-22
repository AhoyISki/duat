#![feature(return_position_impl_trait_in_trait, decl_macro)]

mod remapper;
mod widgets;

#[cfg(feature = "term-ui")]
type Ui = parsec_term::Ui;

fn main() {
}
