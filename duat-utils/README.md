# duat-utils ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-utils on crates.io](https://img.shields.io/crates/v/duat-utils)](https://crates.io/crates/duat-utils) [![duat-utils on docs.rs](https://docs.rs/duat-utils/badge.svg)](https://docs.rs/duat-utils) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-utils)

Standard complementary additions to Duat

This crate essentially consists of the standard bits that pretty
much every `config` crate will want to use, but aren’t strictly
speaking necessary for Duat to function. This split is mostly to
improve compile times, but semantically, if a crate doesn’t need
all of these extra things, it is nice to separate them out.

The crate has the following elements:

* 5 [`widgets`][__link0]:
  
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
  * [`LogBook`][__link13] is a log of everything that has been notified to
    Duat. It is usually more admissive than [`Notifications`][__link14], and
    is most commonly scrolled by the [`Pager`][__link15] [`Mode`][__link16].
* 3 [`modes`][__link17]:
  
  * [`Regular`][__link18] is essentially the standard [`Mode`][__link19] that text
    editors use. Sort of like VSCode.
  * [`Prompt`][__link20] is a multitool that can serve many purposes,
    through the [`PromptMode`][__link21] trait, which allows one to act on
    the [`PromptLine`][__link22] while abstracting over less important
    elements of the [`Widget`][__link23].
  * [`Pager`][__link24] is a simple, read only [`Mode`][__link25], designed for
    scrolling and searching through [`Widget`][__link26]s, most commonly the
    [`LogBook`][__link27].
* For the [`PromptLine`][__link28], there are 4 [`PromptMode`][__link29]s:
  
  * [`RunCommands`][__link30] will interpret and run Duat commands, with
    syntax highlighting for correctness, defined by the
    [`Parameter`][__link31] trait.
  * [`PipeSelections`][__link32] will pipe each selection on the current
    [`File`][__link33], replacing them with the return value from a shell
    command.
  * [`IncSearch`][__link34] is a specialized mode used for incremental
    search, which can abstract over what the search actually does
    with the [`IncSearcher`][__link35] trait.
* For [`IncSearch`][__link36], there are 4 [`IncSearcher`][__link37]s:
  
  * [`SearchFwd`][__link38] will move each [`Cursor`][__link39] to the next match.
  * [`SearchRev`][__link40] will move each [`Cursor`][__link41] to the previous match.
  * [`ExtendFwd`][__link42] will extend each [`Cursor`][__link43]’s selections to the
    next match.
  * [`ExtendRev`][__link44] will extend each [`Cursor`][__link45]’s selections to the
    previous match.

Note that the [`IncSearcher`][__link46] trait can be used for many more
interesting things, like in [`duat-kak`][__link47] for example, where its
implementors allow for splitting selections, selecting everything
within a range, and many more such things in the future.

* There are also two [`hooks`][__link48]:
  * [`SearchUpdated`][__link49] for when an [`IncSearch`][__link50] is updated.
  * [`SearchPerformed`][__link51] for when an [`IncSearch`][__link52] is finished.

And finally, there is the [`state`][__link53] module, which contains a bunch
of [`StatusLine`][__link54] parts for you to customize the [`StatusLine`][__link55]
with.

I would consider this crate essential for all `config`s of Duat
out there, since it defines primitives that are not only hard to
replace, but might also be very extensible by plugins in the
ecosystem.


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG55weKOEYG5TG0BMZre0i-rkG6_GS5qbVWo4G4keDqkWztr_YWSCg2pkdWF0LXV0aWxzZTAuMi4xamR1YXRfdXRpbHOCaWR1YXRfY29yZWUwLjUuMw
 [__link0]: https://docs.rs/duat-utils/0.2.1/duat_utils/widgets/index.html
 [__link1]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::LineNumbers
 [__link10]: https://docs.rs/duat_core/0.5.3/duat_core/?search=context::error
 [__link11]: https://docs.rs/duat_core/0.5.3/duat_core/?search=context::warn
 [__link12]: https://docs.rs/duat_core/0.5.3/duat_core/?search=context::info
 [__link13]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::LogBook
 [__link14]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::Notifications
 [__link15]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::Pager
 [__link16]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Mode
 [__link17]: https://docs.rs/duat-utils/0.2.1/duat_utils/modes/index.html
 [__link18]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::Regular
 [__link19]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Mode
 [__link2]: https://docs.rs/duat_core/0.5.3/duat_core/?search=file::File
 [__link20]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::Prompt
 [__link21]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::PromptMode
 [__link22]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::PromptLine
 [__link23]: https://docs.rs/duat_core/0.5.3/duat_core/?search=ui::Widget
 [__link24]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::Pager
 [__link25]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Mode
 [__link26]: https://docs.rs/duat_core/0.5.3/duat_core/?search=ui::Widget
 [__link27]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::LogBook
 [__link28]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::PromptLine
 [__link29]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::PromptMode
 [__link3]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::PromptLine
 [__link30]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::RunCommands
 [__link31]: https://docs.rs/duat_core/0.5.3/duat_core/?search=cmd::Parameter
 [__link32]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::PipeSelections
 [__link33]: https://docs.rs/duat_core/0.5.3/duat_core/?search=file::File
 [__link34]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::IncSearch
 [__link35]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::IncSearcher
 [__link36]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::IncSearch
 [__link37]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::IncSearcher
 [__link38]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::SearchFwd
 [__link39]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link4]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::StatusLine
 [__link40]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::SearchRev
 [__link41]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link42]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::ExtendFwd
 [__link43]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link44]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::ExtendRev
 [__link45]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link46]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::IncSearcher
 [__link47]: https://docs.rs/duat-kak/latest/duat_kak
 [__link48]: https://docs.rs/duat-utils/0.2.1/duat_utils/hooks/index.html
 [__link49]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=hooks::SearchUpdated
 [__link5]: https://docs.rs/duat_core/0.5.3/duat_core/?search=data::RwData
 [__link50]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::IncSearch
 [__link51]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=hooks::SearchPerformed
 [__link52]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=modes::IncSearch
 [__link53]: https://docs.rs/duat-utils/0.2.1/duat_utils/state/index.html
 [__link54]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::StatusLine
 [__link55]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::StatusLine
 [__link6]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::status
 [__link7]: https://docs.rs/duat_core/0.5.3/duat_core/?search=text::txt
 [__link8]: https://docs.rs/duat-utils/0.2.1/duat_utils/?search=widgets::Notifications
 [__link9]: https://docs.rs/duat_core/0.5.3/duat_core/?search=context::Logs
