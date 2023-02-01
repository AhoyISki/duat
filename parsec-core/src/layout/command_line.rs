/// A command that returns a `String` as a result.
///
/// The command takes in two vectors of `String`s, the first one is the "flags" passed on to the
/// command. The second one is a list of arguments passed on to the command.
pub struct OutputCommand<C>
where
    C: Commandable + ?Sized, {
    function: Box<dyn FnMut(&mut C, Vec<String>, Vec<String>) -> Result<String, String>>,
    /// A list of `String`s that act as callers for this `Command`.
    callers: Vec<String>
}

/// A command that doesn't returns a `String` as a result.
///
/// The command takes in two vectors of `String`s, the first one is the "flags" passed on to the
/// command. The second one is a list of arguments passed on to the command.
pub struct Command<C>
where
    C: Commandable + ?Sized, {
    function: Box<dyn FnMut(&mut C, Vec<String>, Vec<String>) -> Result<(), String>>,
    /// A list of `String`s that act as callers for this `Command`.
    callers: Vec<String>
}

/// A trait representing a struct that can be mutated through the command line.
pub trait Commandable {
    fn output_commands(&self) -> Vec<OutputCommand<Self>>;

    fn commands(&self) -> Vec<Command<Self>>;
}
