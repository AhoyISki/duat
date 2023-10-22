use parsec_core::SessionCfg;

mod remapper;
mod widgets;

#[cfg(feature = "term-ui")]
type Ui = parsec_term::Ui;
