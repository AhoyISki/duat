# duat-utils ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-utils on crates.io](https://img.shields.io/crates/v/duat-utils)](https://crates.io/crates/duat-utils) [![duat-utils on docs.rs](https://docs.rs/duat-utils/badge.svg)](https://docs.rs/duat-utils) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-utils)

Standard complementary additions to Duat

This crate essentially consists of the standard bits that pretty
much every `config` crate will want to use, but aren’t strictly
speaking necessary for Duat to function. This split is mostly to
improve compile times, but semantically, if a crate doesn’t need
all of these extra things, it is nice to separate them out.

The crate has the following elements:

* 4 [`widgets`][__link0]:
  
  * [`LineNumbers`][__link1] shows the numbers on a [`File`][__link2] (for now), and
    you can configure their alignment, relativeness, etc.
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
* 2 [`modes`][__link13]:
  
  * [`Regular`][__link14] is essentially the standard [`Mode`][__link15] that text
    editors use. Sort of like VSCode.
  * [`Prompt`][__link16] is a multitool that can serve many purposes,
    through the [`PromptMode`][__link17] trait, which allows one to act on
    the [`PromptLine`][__link18] while abstracting over less important
    elements of the [`Widget`][__link19].
* For the [`PromptLine`][__link20], there are 3 [`PromptMode`][__link21]s:
  
  * [`RunCommands`][__link22] will interpret and run Duat commands, with
    syntax highlighting for correctness, defined by the
    [`Parameter`][__link23] trait.
  * [`PipeSelections`][__link24] will pipe each selection on the current
    [`File`][__link25], replacing them with the return value from a shell
    command.
  * [`IncSearch`][__link26] is a specialized mode used for incremental
    search, which can abstract over what the search actually does
    with the [`IncSearcher`][__link27] trait.
* For [`IncSearch`][__link28], there are 4 [`IncSearcher`][__link29]s:
  
  * [`SearchFwd`][__link30] will move each [`Cursor`][__link31] to the next match.
  * [`SearchRev`][__link32] will move each [`Cursor`][__link33] to the previous match.
  * [`ExtendFwd`][__link34] will extend each [`Cursor`][__link35]’s selections to the
    next match.
  * [`ExtendRev`][__link36] will extend each [`Cursor`][__link37]’s selections to the
    previous match.

Note that the [`IncSearcher`][__link38] trait can be used for many more
interesting things, like in [`duat-kak`][__link39] for example, where its
implementors allow for splitting selections, selecting everything
within a range, and many more such things in the future.

* There are also two [`hooks`][__link40]:
  * [`SearchUpdated`][__link41] for when an [`IncSearch`][__link42] is updated.
  * [`SearchPerformed`][__link43] for when an [`IncSearch`][__link44] is finished.

And finally, there is the [`state`][__link45] module, which contains a bunch
of [`StatusLine`][__link46] parts for you to customize the [`StatusLine`][__link47]
with.

I would consider this crate essential for all `config`s of Duat
out there, since it defines primitives that are not only hard to
replace, but might also be very extensible by plugins in the
ecosystem.


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG1uw2aMfIt36G0GmVyKwc3vtG1n1EyTMfRRkG4a2tS0sDzFyYWSCg2pkdWF0LXV0aWxzZTAuMS4wamR1YXRfdXRpbHOCaWR1YXRfY29yZWUwLjQuMA
 [__link0]: https://docs.rs/duat-utils/0.1.0/duat_utils/widgets/index.html
 [__link1]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::LineNumbers
 [__link10]: https://docs.rs/duat_core/0.4.0/duat_core/?search=context::error
 [__link11]: https://docs.rs/duat_core/0.4.0/duat_core/?search=context::warn
 [__link12]: https://docs.rs/duat_core/0.4.0/duat_core/?search=context::info
 [__link13]: https://docs.rs/duat-utils/0.1.0/duat_utils/modes/index.html
 [__link14]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::Regular
 [__link15]: https://docs.rs/duat_core/0.4.0/duat_core/?search=mode::Mode
 [__link16]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::Prompt
 [__link17]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::PromptMode
 [__link18]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::PromptLine
 [__link19]: https://docs.rs/duat_core/0.4.0/duat_core/?search=widget::Widget
 [__link2]: https://docs.rs/duat_core/0.4.0/duat_core/?search=file::File
 [__link20]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::PromptLine
 [__link21]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::PromptMode
 [__link22]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::RunCommands
 [__link23]: https://docs.rs/duat_core/0.4.0/duat_core/?search=cmd::Parameter
 [__link24]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::PipeSelections
 [__link25]: https://docs.rs/duat_core/0.4.0/duat_core/?search=file::File
 [__link26]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::IncSearch
 [__link27]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::IncSearcher
 [__link28]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::IncSearch
 [__link29]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::IncSearcher
 [__link3]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::PromptLine
 [__link30]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::SearchFwd
 [__link31]: https://docs.rs/duat_core/0.4.0/duat_core/?search=mode::Cursor
 [__link32]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::SearchRev
 [__link33]: https://docs.rs/duat_core/0.4.0/duat_core/?search=mode::Cursor
 [__link34]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::ExtendFwd
 [__link35]: https://docs.rs/duat_core/0.4.0/duat_core/?search=mode::Cursor
 [__link36]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::ExtendRev
 [__link37]: https://docs.rs/duat_core/0.4.0/duat_core/?search=mode::Cursor
 [__link38]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::IncSearcher
 [__link39]: https://docs.rs/duat-kak/latest/duat_kak
 [__link4]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::StatusLine
 [__link40]: https://docs.rs/duat-utils/0.1.0/duat_utils/hooks/index.html
 [__link41]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=hooks::SearchUpdated
 [__link42]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::IncSearch
 [__link43]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=hooks::SearchPerformed
 [__link44]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=modes::IncSearch
 [__link45]: https://docs.rs/duat-utils/0.1.0/duat_utils/state/index.html
 [__link46]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::StatusLine
 [__link47]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::StatusLine
 [__link5]: https://docs.rs/duat_core/0.4.0/duat_core/?search=data::RwData
 [__link6]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::status
 [__link7]: https://docs.rs/duat_core/0.4.0/duat_core/?search=text::txt
 [__link8]: https://docs.rs/duat-utils/0.1.0/duat_utils/?search=widgets::Notifications
 [__link9]: https://docs.rs/duat_core/0.4.0/duat_core/?search=context::Logs
