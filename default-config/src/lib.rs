// These two will unfortunately be necessary until this feature
// is eventually stabilized.
#![feature(generic_const_exprs)]
#![allow(incomplete_features)]

// The prelude contains all of the commonly used commands.
use parsec::prelude::{ui::VertRule, *};
// An `InputMethod` for `File`s.
use parsec_kak::KeyMap;

// This macro expands to the function that Parsec understands.
// Will be replaced by a proc-macro at some point.
run! {
    // Resets all existant hooks related to the opening of
    // `File`s.
    hook::reset_file_fn();
    // Sets new hooks for that.
    hook::on_file_open(|builder| {
        // Push some widgets to the edge of the file, in order,
        // from inner to outer.
        //
        // By default, these specific widgets go on the left,
        // but you can override that:
        //
        // // *Uncomment me* //
        // let cfg = VertRule::cfg().on_the_right();
        // builder.push_cfg(cfg);
        // let cfg = LineNumbers::cfg()
        //     .on_the_right()
        //     .align_right()
        //     .align_main_left();
        // builder.push_cfg(cfg);
        // //
        builder.push::<VertRule>();
        builder.push::<LineNumbers>();
    });

    // Resets all existant hooks related to the opening of
    // windows.
    hook::reset_window_fn();
    // Sets new hooks for that.
    hook::on_window_open(|builder| {
        // A macro to format a StatusLine.
        let status = status!(
            // A `Form` is enclosed in `[` `]` pairs, and sets
            // the text formatting.
            // The "Default" form is abbreviated as `[]`.
            [File]
            // A function that reads from a `File`. Anything
            // that isn't a `Form` is treated as an expression.
            { File::name }
            // Any type that implements `Into<Text>` works.
            " "
            // You can also use closures. This one reads from a
            // `parsec_kak::KeyMap`.
            { |map: &KeyMap| KeyMap::mode_fmt(map) } " "
            // Finally, these two functions return `Text`s.
            // Any function that returns `Text` should be have
            // the `_fmt` suffix, for consistency.
            selections_fmt " " main_fmt
        );
        // Push a widget to a specific are, in this case, the
        // new area housing the `StatusLine`.
        let (child, _) = builder.push_cfg(status);
        builder.push_cfg_to(CommandLine::cfg().left_with_percent(30), child);
    });

    // Sets a new `InputMethod for `File`s.
    setup::input(KeyMap::new());

	// Modifies the "Mode" `Form`, created by `KeyMap::new`
	// method.
    print::forms::set("Mode", Form::new().dark_magenta());
}
