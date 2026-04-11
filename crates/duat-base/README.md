# duat-base ![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue) [![duat-base on crates.io](https://img.shields.io/crates/v/duat-base)](https://crates.io/crates/duat-base) [![duat-base on docs.rs](https://docs.rs/duat-base/badge.svg)](https://docs.rs/duat-base) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-base)

Standard complementary additions to Duat

This crate essentially consists of the standard bits that pretty
much every `config` crate will want to use, but aren’t strictly
speaking necessary for Duat to function. This split is mostly to
improve compile times, but semantically, if a crate doesn’t need
all of these extra things, it is nice to separate them out.

The crate has the following elements:

* 8 [`widgets`][__link0]:
  
  * [`LineNumbers`][__link1] shows the numbers on a [`Buffer`][__link2] (for now),
    and you can configure their alignment, relativeness, etc.
  * The [`PromptLine`][__link3] lets you run commands and do other things,
    like incremental search
  * The [`StatusLine`][__link4] lets you display information that gets
    updated automatically, it can show information from
    [`RwData`][__link5]s, mapping functions, static elements, and every bit
    of Duat. It’s syntax, in [`status!`][__link6] is the same as the
    [`txt!`][__link7] macro.
  * [`Notifications`][__link8] shows things that have been logged to the
    [`Logs`][__link9] of Duat, through the [`error!`][__link10], [`warn!`][__link11] and
    [`info!`][__link12] macros.
  * [`LogBook`][__link13] is a log of everything that has been notified to
    Duat. It is usually more admissive than `Notifications`, and
    is most commonly scrolled by the [`Pager`][__link14] [`Mode`][__link15].
  * [`Completions`][__link16] is Duat’s completion widget, it provides an
    extensible completions list, which allows you to format the
    entries and add new providers via the [`CompletionsProvider`][__link17]
    trait. Right now, the only `CompletionsProvider` is the words
    provider.
  * [`Gutter`][__link18] Sits on the side of each `Buffer`, showing
    diagnostic information about each line of the `Buffer`.
  * [`WhichKey`][__link19] shows what each key will do. It shows up
    automatically as you are typing and multi key sequences are
    expected (e.g. Vim’s `s`, `d`, `f` and others).
  * [`Info`][__link20] just shows static information, resizing itself to
    properly show as much of it as possible.
* 2 [`modes`][__link21]:
  
  * [`Prompt`][__link22] is a multitool that can serve many purposes,
    through the [`PromptMode`][__link23] trait, which allows one to act on
    the `PromptLine` while abstracting over less important
    elements of the `Widget`.
  * [`Pager`][__link24] is a simple, read only `Mode`, designed for
    scrolling and searching through `Widget`s, most commonly the
    `LogBook`.
* For the [`PromptLine`][__link25], there are 4 [`PromptMode`][__link26]s:
  
  * [`RunCommands`][__link27] will interpret and run Duat commands, with
    syntax highlighting for correctness, defined by the
    [`Parameter`][__link28] trait.
  * [`PipeSelections`][__link29] will pipe each selection on the current
    `Buffer`, replacing them with the return value from a shell
    command.
  * [`IncSearch`][__link30] is a specialized mode used for incremental
    search, which can abstract over what the search actually does
    with the [`IncSearcher`][__link31] trait.
* For [`IncSearch`][__link32], there are 4 `IncSearcher`s:
  
  * [`SearchFwd`][__link33] will move each [`SelectionMut`][__link34] to the next match.
  * [`SearchRev`][__link35] will move each `SelectionMut` to the previous match.
  * [`ExtendFwd`][__link36] will extend each `SelectionMut`’s selections to the
    next match.
  * [`ExtendRev`][__link37] will extend each `SelectionMut`’s selections to the
    previous match.

Note that the [`IncSearcher`][__link38] trait can be used for many more
interesting things, like in [`duat-kak`][__link39] for example, where its
implementors allow for splitting selections, selecting everything
within a range, and many more such things in the future.

* There are also two [`hooks`][__link40]:
  * [`SearchUpdated`][__link41] for when an `IncSearch` is updated.
  * [`SearchPerformed`][__link42] for when an `IncSearch` is finished.

And finally, there is the [`state`][__link43] module, which contains a bunch
of [`StatusLine`][__link44] parts for you to customize the `StatusLine`
with.

I would consider this crate essential for all `config`s of Duat
out there, since it defines primitives that are not only hard to
replace, but might also be very extensible by plugins in the
ecosystem.


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG9jDgQBMKFFRG_1r2JJ7ItOlG8pI2-ZQxwnVGxhoFYcMO5tsYWSCg2lkdWF0LWJhc2VlMC45LjBpZHVhdF9iYXNlgmlkdWF0X2NvcmVlMC45LjA
 [__link0]: https://docs.rs/duat-base/0.9.0/duat_base/widgets/index.html
 [__link1]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::LineNumbers
 [__link10]: https://docs.rs/duat_core/0.9.0/duat_core/?search=context::error
 [__link11]: https://docs.rs/duat_core/0.9.0/duat_core/?search=context::warn
 [__link12]: https://docs.rs/duat_core/0.9.0/duat_core/?search=context::info
 [__link13]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::LogBook
 [__link14]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::Pager
 [__link15]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::Mode
 [__link16]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::Completions
 [__link17]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::CompletionsProvider
 [__link18]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::Gutter
 [__link19]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::WhichKey
 [__link2]: https://docs.rs/duat_core/0.9.0/duat_core/?search=buffer::Buffer
 [__link20]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::Info
 [__link21]: https://docs.rs/duat-base/0.9.0/duat_base/modes/index.html
 [__link22]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::Prompt
 [__link23]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::PromptMode
 [__link24]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::Pager
 [__link25]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::PromptLine
 [__link26]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::PromptMode
 [__link27]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::RunCommands
 [__link28]: https://docs.rs/duat_core/0.9.0/duat_core/?search=cmd::Parameter
 [__link29]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::PipeSelections
 [__link3]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::PromptLine
 [__link30]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::IncSearch
 [__link31]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::IncSearcher
 [__link32]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::IncSearch
 [__link33]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::SearchFwd
 [__link34]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::SelectionMut
 [__link35]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::SearchRev
 [__link36]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::ExtendFwd
 [__link37]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::ExtendRev
 [__link38]: https://docs.rs/duat-base/0.9.0/duat_base/?search=modes::IncSearcher
 [__link39]: https://docs.rs/duat-kak/latest/duat_kak
 [__link4]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::StatusLine
 [__link40]: https://docs.rs/duat-base/0.9.0/duat_base/hooks/index.html
 [__link41]: https://docs.rs/duat-base/0.9.0/duat_base/?search=hooks::SearchUpdated
 [__link42]: https://docs.rs/duat-base/0.9.0/duat_base/?search=hooks::SearchPerformed
 [__link43]: https://docs.rs/duat-base/0.9.0/duat_base/state/index.html
 [__link44]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::StatusLine
 [__link5]: https://docs.rs/duat_core/0.9.0/duat_core/?search=data::RwData
 [__link6]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::status
 [__link7]: https://docs.rs/duat_core/0.9.0/duat_core/?search=text::txt
 [__link8]: https://docs.rs/duat-base/0.9.0/duat_base/?search=widgets::Notifications
 [__link9]: https://docs.rs/duat_core/0.9.0/duat_core/?search=context::Logs
