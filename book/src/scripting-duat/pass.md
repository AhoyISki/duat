# The Pass and Duat's global state

The most important concept when configuring duat is the sharing model of 
memory. Every piece of state in Duat is behind some type that requires the 
`Pass` struct.

The `Pass` struct is how you access duat's global state. It works through 
Rust's borrow checker, giving you safe access to the variables of the global 
state. It works via mutable and immutable references, that is, whenever you 
mutably borrow a `Pass`, you get mutable access to the global state of duat, 
and whenever you borrow it normally, you get non mutable access to said state:

```rust
use duat::prelude::*;

// As a shorthand, you should use `pa` as the variable's name.
// You don't need to write the types of `pa` and `handle`, they're
// just here for tutorial reasons.
hook::add::<BufferOpened>(|pa: &mut Pass, handle: &Handle<Buffer>| {
    // This function will mutably borrow the `Pass`, preventing other 
    // uses of it, thus following Rust's mutability XOR aliasing rule.
    let buf_mut: &mut Buffer = handle.write(pa);
    
    // Given the mutable reference, you can change the `Buffer`.
    buf_mut.opts.wrap_lines = false;
    buf_mut.opts.tabstop = 2;
    
    // This function will immutably borrow the `Pass`, which means you
    // can do other immutable borrows, letting you have as many
    // immutable references as you want.
    // Note that all function available from an immutable borrow are also
    // available with a mutable one.
    let buf: &Buffer = handle.read(pa);
    
    // The `Buffer` may not have any filetype if it can't be inferred.
    let Some(filetype) = buf.filetype() else {
        return;
    };
    
    // Calling this now would cause a compile time error, since you would
    // have reused a mutable reference (`&mut Buffer`) after getting another
    // one (`&Buffer`).
    // buf_mut.opts.scrolloff.y = 5;
    
    // The context module will have all the types representing duat's state
    // This function retrieves `Handle`s to all `Buffer`s in duat.
    let mut other_buffers = context::buffers(pa);
    other_buffers.retain(|other| other != handle);
    
    if other_buffers.iter().all(|other| other.filetype(pa) != Some(filetype)) {
        context::info!("Opened the first buffer of filetype [a]{filetype}");
        context::info!("The buffer is called [a]{}", buf.name());
    }
});
```

The above example showcases the two types of borrowing. The first type is the 
mutable borrow, which can't be reused after acquiring other references, and the 
second one is the immutable borrow, which can coexist with any number of other 
immutable references.

These two borrows govern all of the configuration of duat, and they can only 
bedone on the main thread, given the non `Send` nature of `&Pass` and `&mut 
Pass`. Additionally, you won't always have a `&mut Pass`. In some APIs, you 
only get access to a `&Pass`, which grants only reading access to global state, 
making for APIs that are harder to mess up.

> [!TIP]
> While editing with rust-analyzer, on functions that require a `&Pass` or 
> `&mut Pass`, it might suggest a completion like `&pa`, or `&mut pa`.
> You should ignore those suggestions and just type `pa`, since rust will
> automatically convert a `&mut Pass` to a `&Pass` given the context, so `pa`
> should be enough for every single situation.

## Global state types

Above, you saw a `Handle<Buffer>`. This is a handle to a `Widget` of type 
`Buffer`. You can have a `Handle<W>` for any widget of type `W`, which lets you 
change them, including the `Text` that they display.

Internally, a `Handle<W>` is backed by two datatypes, an `RwData<W>` and a 
`RwArea`. The `RwData` type is duat's global state primitive, every kind of 
global access will eventually go through an `RwData`.

### The `RwData` type

This type serves two primary purposes:

- To hold a value, which can be accessed with a `Pass`.
- To tell others if the value has been updated.

Lets explore how this happens. Below is a code snippet that shows on the 
`StatusLine` wether a value has changed:

```rust
use duat::prelude::*;

fn setup() {
    let value = RwData::new(0);
    let value_clone = value.clone();
   
    opts::fmt_status(move |_| {
        let value_changed = {
            // One copy for this function
            let value = value_clone.clone();
            move || {
                if value.has_changed() {
                    "value changed to "
                } else {
                    ""
                }
            }
        };
        
        // And one copy for the `StatusLine`.
        // This is done so the `StatusLine` updates automatically.
        let value = value_clone.clone();
        status!("{name_txt} {value_changed}{value}{Spacer}{main_txt}")
    });
    
    cmd::add("change-value", move |pa: &mut Pass, new: usize| {
        *value.write(pa) = new;
        Ok(None)
    });
}
```

In the snippet above, the `value` variable will be displayed in the 
`StatusLine`, and every time it changes, the text `"value changed to" will be 
shown beside it.

The value inside an `RwData` is deemed as "changed" if the `RwData::write` 
function was called in any other copy of the same `RwData`:

```rust
use duat::prelude::*;

fn test(pa: &mut Pass) {
    let value = RwData::new("hello");
    let value_clone = value.clone();
    
    // Since `RwData::read` hasn't been called, both are considered
    // to have changed.
    assert!(value.has_changed() == true);
    assert!(value_clone.has_changed() == true);
    
    // Further calls to `RwData::has_changed` will return `true` until
    // you call `RwData::read`
    assert!(value.has_changed() == true);
    
    _ = value_clone.read(pa);
    
    *value.write(pa) = "bye";
    
    // Since `value` was written to, copies of `value` will be notified
    // that the data within has changed.
    assert!(value_clone.has_changed() == true);
    
    // An `RwData::write` also assumes that you have read the value, so
    // `value.has_changed()` will return `false`.
    assert!(value.has_changed() == false);
    
    // If you don't have a Pass available (rare), you can also say that
    // you don't care if the data has changed by calling this function.
    value_clone.declare_as_read();
    
    assert!(value_clone.has_changed() == false);
    
    // Without a Pass, you can also tell other copies that a value has
    // been changed, even if nothing changed at all
    value_clone.declare_written();
    
    assert!(value.has_changed() == true);
}
```

### Widget `Handle`s

A `Handle<W: Widget>` is a wrapper over two types:

- An `RwData<W>`, which holds a widget
- An `RwArea`

This type is responsible for displaying everything on screen, by taking the 
`&Text` out ofevery widget and printing it on their respective `RwArea`s.

The most common `Handle` you will encounter is the `Handle<Buffer>`, 
shorthanded to just `Handle`. You are able to freely modify it whenever you 
have access to an `&mut Pass`:

```rust
use duat::prelude::*;
use std::collections::HashSet;

fn setup() {
    cmd::add("dedup-selections", |pa: &mut Pass| {
        let buf: Handle<Buffer> = context::current_buffer(pa);
        let mut dupes = Vec::new();
        
        // Handle::edit_all will apply an editing function to all cursors
        // Because it edits the cursors of the `Buffer`, it needs
        // mutable access to it, hence the `&mut Pass`.
        buf.edit_all(pa, |c| {
            if dupes.iter().any(|dupe| *dupe == c.selection()) {
                c.destroy()
            } else {
                dupes.push(c.selection().to_string());
            }
        });
    
        Ok(None)
    })
    .doc(txt!("Removes selections with duplicate text content"), None);
}
```

The `raison d'être` of `Handle`s is to keep both the `RwArea` and `RwData<W>` 
in the same struct, so you can use methods that require reading from both:

```rust
use duat::prelude::*;

fn setup() {
    opts::fmt_status(|_| {
        status!("{name_txt} {}{Spacer}{main_txt}{cursors_on_screen}", mode_txt())
    });
}

fn cursors_on_screen(pa: &Pass, handle: &Handle) -> Text {
    /// In order to get the range of the `Text` that is on screen,
    /// you need both the `Buffer` and the `RwArea` at the same time.
    let range = handle.full_printed_range(pa);
    
    let total = handle.selections(pa).iter_within(range).count();
    
    txt!("On screen[separator]:[] [coord]{total}")
}
```

### The type erased `RwArea`

In Duat, you may create your own Ui by implementing the `RawUi` trait, which 
also requires an area type that implements the `RawArea` trait.

The `RwArea` type is a [type erased] wrapper over this `RawArea`. When you call 
`RwArea::write`, you will get a `&mut Area` value, which is also type erased, 
but grants direct access to the methods defined in the `RawArea` trait, without 
the need for a `Pass`. 

When you have a `Handle`, you can access the `Area` value in two ways:

```rust
use duat::{prelude::*, ui::RwArea};

fn test<W: Widget>(pa: &mut Pass, handle: &Handle<W>) {
    let rw_area: &RwArea = handle.area();
    let area: &mut Area = rw_area.write(pa);
    
    area.set_width(5.0).unwrap();
    
    // Normally, calling `Handle::write` returns only `W`, for
    // convenience reasons and because you mostly don't need to
    // access the `Area` value.
    // You can do this if you need mutable access to both.
    let (widget, area): (&mut W, &mut Area) = handle.write_with_area(pa);
    
    area.set_height(10.0).unwrap();
    widget.text_mut().replace_range(.., "lmao");
}
```

If required, you can also access the unerased version of the `Area` by calling 
`RwArea::write_as` or `RwArea::read_as`, which takes in a type argument `A` and 
returns `Some(area)` if the area is actually of type `A` (i.e., duat was 
compiled with that `RawArea`'s `RawUi`).

### `DataMap` and `MutDataMap`

On the earlier example which shows wether a value has changed, you might have 
thought that the code looked a little awkward, since we had to put two objects
on the `StatusLine` just to get it to update properly.

Well, there is actually an existing solution to that, which can be achieved by calling the `RwData::map` function:

```rust
use duat::prelude::*;

fn setup() {
    let value = RwData::new(0);
    let value_clone = value.clone();
   
    opts::fmt_status(move |_| {
        // Clone to check if it has changed from within the function,
        // you never really need to do this.
        let clone = value_clone.clone();
        let show_value = value_clone.map(move |value| {
            if clone.has_changed() {
                clone.declare_as_read();
                format!("value changed to {value}")
            } else {
                format!("{value}")
            }
        });
        
        status!("{name_txt} {show_value}{Spacer}{main_txt}")
    });
    
    cmd::add("change-value", move |pa: &mut Pass, new: usize| {
        *value.write(pa) = new;
        Ok(None)
    });
}
```

This returns a `DataMap<usize, String>`, which works very similarly to an 
`RwData`, except for the fact that it can only be read, and reading is 
accompanied by a call to the mapping function, returning its value, as opposed 
to `usize`.

This type also has the property of automatically updating the `StatusLine` 
whenever the value changes, which means that you no longer need to place a 
`{value}` in order to update the `StatusLine` automatically.

The `MutDataMap` serves a similar purpose, but it can also write to the inner 
value, and is created through `RwData::map_mut`.

### `BulkDataWriter` for parallelism

One problem with all of the previously mentioned types is that, in order to 
write to them, you _need_ to have a `&mut Pass` with you. Since this type can 
only be accessed from the main thread, you _cannot_ update values from other 
threads.

This type does not change that fact, however, it lets you "push" updating 
functions via the `BulkDataWriter::mutate` method. This method, which 
doesn'ttake a `Pass`, will send an `impl FnOnce` function to update the inner 
value,and the next time someone tries to call `BulkDataWriter::write`, which 
does takea `Pass`, that function will be called and will update the value 
before returning the `&mut T.

This makes it possible to update a value from another thread:

```rust
use duat::{data::BulkDataWriter, prelude::*};
use std::time::Duration;

// You will mostly want this type to be a static variable
static DATA: BulkDataWriter<Duration> = BulkDataWriter::new();

fn setup() {
    std::thread::spawn(|| {
        let one_sec = Duration::from_secs(1);
        while !context::will_reload_or_quit() {
            std::thread::sleep(one_sec);
            DATA.mutate(move |value| *value += one_sec);
        }
    });
    
    cmd::add("uptime", |pa: &mut Pass| {
        context::info!("The uptime is {:?}", DATA.write(pa));
        Ok(None)
    });
}
```

This type is very focused on building APIs, in particular, it is good at 
permitting functions that don't take a `Pass` to be used. For example, in duat, 
the `cmd::add` function makes use of a `BulkDataWriter` in order to be called 
without a `Pass`, only actually adding the function when calling `cmd::call` or 
other functions that need access to the list of commands.

Another API that makes use of this is the one backing the `map` and `alias` 
functions.

## Multiple simultaneous writes

One limitation that you might have perceived up to this point is that you can't 
write to two `RwData`-like structures at the same time, since they would both 
need to borrow from the `&mut Pass`, breaking Rust's aliasing XOR mutability 
rule.

There is, however, one way to do this, which is through the `Pass:write_many` 
method. This method takes in a group of `RwData`-like structures and gets 
mutable references to _all_ of them at the same time.

```rust
use duat::prelude::*;

fn test(pa: &mut Pass, lhs: [&RwData<u32>; 2], rhs: &RwData<u32>) {
    // You can pass in any tuple (up to 12 elements) or any array
    // of `RwData`-like structures and this function will give you a
    // mutable reference to all of them.
    let ([l0, l1], r): ([&mut u32; 2], &mut u32) = pa.write_many((lhs, rhs));
    
    let trouble = [lhs[0], lhs[0]];
    // Note here that I'm writing to the same variable twice.
    // Instead of returning two mutable references to the same value, this
    // call will just panic, returning control to Duat and cancelling
    // whatever you were doing.
    let [oops0, oops1] = pa.write_many(trouble);
    
    // If you think this might happen, you can call this instead.
    let handled = pa.try_write_many(trouble);
    
    // This function will return an `Err(text)` in the case of failure.
    assert!(handled.is_err());
}
```

[type erased]: https://en.wikipedia.org/wiki/Type_erasure
