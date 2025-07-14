# `form`: How text is colored

In duat, the way text is styled is through `Form`s. The `Form` struct, 
alongside the `form` module, are imported in the prelude:

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

## Colorschemes

The other main function that you will use from this module is the 
`form::set_colorscheme` function. This function will change the colorscheme to 
a previously named one:

```rust
setup_duat!(setup);
use duat::prelude::*;
use catppuccin::Catppuccin;

fn setup() {
    plug!(Catppuccin::new());
    
    form::set_colorscheme("catppuccin-latte");
}
```

## `Form` inheritance

Another aspect of duat's forms that can save a lot of typing is the concept of `Form` inheritance. In Duat, forms follow the following structure:

- If `my_form.my_subform` is unset, it will reference `my_form`;
- If `my_form.my_subform` is set to `Form::green()`, it won't be changed when 
  `my_form` changes, staying at `Form::green()`;
- If `my_form.my_subform` is set to reference `my_other_form`, changing 
  `my_other_form` will also change `my_form.my_subform`, but changing `my_form` 
  won't;


