# `form`: How text is colored

In duat, the way text is styled is through `Form`s. The `Form` struct, 
alongside the `form` module, are imported by the prelude:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // Setting by Form
    form::set("punctuation.bracket", Form::red());
    form::set("default", Form::with("#575279").on("#faf4ed"));
    form::set("matched_pair", Form::blue().underlined()); 
    
    // Setting by reference
    form::set("accent.debug", "default.debug");
}
```

The main function that you will use from this module is `form::set`. This 
function sets the form with the given name on the left to the argument on the 
right. This argument can be of two types:

- A `Form` argument will just modify the named form to be shown like that;
- A `&str` argument will "reference" the form on the right. If the form on the 
  right is altered, so will the one on the left. This reduces the need for 
  setting a ton of forms in things like colorschemes;

## How forms should be named

Every form in duat should be named like this: `[a-z0-9]+(\.[a-z0-9]+)*`. That 
way, inheritance of forms becomes very predictable, and it's much easier for 
plugin writers to depend on that feature.

There is one exception to this rule however, that being the `defaut` form. The 
`default` form, unlike other forms, can have `Widget` specific implementations, 
like `default.StatusLine`, which will change the `default` form _only_ on 
`StatusLine`s, and is set by default.

## Colorschemes

The other main function that you will use from this module is the 
`form::set_colorscheme` function. This function will change the colorscheme to 
a previously named one:

```rust
setup_duat!(setup);
use duat::prelude::*;
use catppuccin::Catppuccin;

fn setup() {
    // Adds four colorschemes, "catppuccin-latte" among them.
    plug!(Catppuccin::new());
    
    form::set_colorscheme("catppuccin-latte");
}
```

## `Form` inheritance

Another aspect of duat's forms that can save a lot of typing is the concept of `Form` inheritance. In Duat, forms follow the following structure:

- If `form.subform` is unset, it will reference `form`;
- If `form.subform` is set to `Form::green()`, it won't be changed when 
  `form` changes, staying at `Form::green()`;
- If `form.subform` is set to reference `other_form`, changing 
  `other_form` will also change `form.subform`, but changing `form` 
  won't;

As a consequence of this, for example, if you were to set the `markup` form to 
something, every form with a name like `markup.*` _that isn't already set_, 
would follow the change to the `markup` form.

Additionally, if the form `f0.f1.f2` is set to something, the forms `f0` and 
`f1.f2` would also be set, although they will reference the `default` form in 
that situation, not whatever `f0.f1.f2` was set to.

## Masks

A mask is essentially the opposite of the inheritance concept. Instead of the longer form inheriting from the shorter forms, the shorter forms will be mapped for longer ones.

It works like this: Say I have a `File` widget, and in it, there are instances 
of the `function` form, used to highlight function identifiers. If there is a 
`function.error` form, and I tell the `File` to use the `error` mask, instead 
of using the `function` form, Duat will use `function.error`.

In duat, by default there are four masks: `error`, `warning`, `info`, and `inactive`. The first three are used primarily to show color coded notifications. The last one is unused, but you can use it to change how unfocused files should be displayed.

You can also add more masks through `form::enable_mask`. If you want to learn 
more about masks and how to use them, you should check out the [masks 
chapter](masks.md)
