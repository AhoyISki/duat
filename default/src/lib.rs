use parsec::prelude::*;

#[patch]
pub fn config() -> SessionStarter {
    print::forms::set("File", Form::new().red());
    finish()
}
