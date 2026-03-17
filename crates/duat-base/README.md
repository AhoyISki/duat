# duat-base ![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue) [![duat-base on crates.io](https://img.shields.io/crates/v/duat-base)](https://crates.io/crates/duat-base) [![duat-base on docs.rs](https://docs.rs/duat-base/badge.svg)](https://docs.rs/duat-base) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-base)

Standard complementary additions to Duat

This crate essentially consists of the standard bits that pretty
much every `config` crate will want to use, but aren’t strictly
speaking necessary for Duat to function. This split is mostly to
improve compile times, but semantically, if a crate doesn’t need
all of these extra things, it is nice to separate them out.

The crate has the following elements:

* 5 [`widgets`][__link0]:
  
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
    Duat. It is usually more admissive than [`Notifications`][__link14], and
    is most commonly scrolled by the [`Pager`][__link15] [`Mode`][__link16].
* 3 [`modes`][__link17]:
  
  * [`Prompt`][__link18] is a multitool that can serve many purposes,
    through the [`PromptMode`][__link19] trait, which allows one to act on
    the [`PromptLine`][__link20] while abstracting over less important
    elements of the [`Widget`][__link21].
  * [`Pager`][__link22] is a simple, read only [`Mode`][__link23], designed for
    scrolling and searching through [`Widget`][__link24]s, most commonly the
    [`LogBook`][__link25].
* For the [`PromptLine`][__link26], there are 4 [`PromptMode`][__link27]s:
  
  * [`RunCommands`][__link28] will interpret and run Duat commands, with
    syntax highlighting for correctness, defined by the
    [`Parameter`][__link29] trait.
  * [`PipeSelections`][__link30] will pipe each selection on the current
    [`Buffer`][__link31], replacing them with the return value from a shell
    command.
  * [`IncSearch`][__link32] is a specialized mode used for incremental
    search, which can abstract over what the search actually does
    with the [`IncSearcher`][__link33] trait.
* For [`IncSearch`][__link34], there are 4 [`IncSearcher`][__link35]s:
  
  * [`SearchFwd`][__link36] will move each [`Cursor`][__link37] to the next match.
  * [`SearchRev`][__link38] will move each [`Cursor`][__link39] to the previous match.
  * [`ExtendFwd`][__link40] will extend each [`Cursor`][__link41]’s selections to the
    next match.
  * [`ExtendRev`][__link42] will extend each [`Cursor`][__link43]’s selections to the
    previous match.

Note that the [`IncSearcher`][__link44] trait can be used for many more
interesting things, like in [`duat-kak`][__link45] for example, where its
implementors allow for splitting selections, selecting everything
within a range, and many more such things in the future.

* There are also two [`hooks`][__link46]:
  * [`SearchUpdated`][__link47] for when an [`IncSearch`][__link48] is updated.
  * [`SearchPerformed`][__link49] for when an [`IncSearch`][__link50] is finished.

And finally, there is the [`state`][__link51] module, which contains a bunch
of [`StatusLine`][__link52] parts for you to customize the [`StatusLine`][__link53]
with.

I would consider this crate essential for all `config`s of Duat
out there, since it defines primitives that are not only hard to
replace, but might also be very extensible by plugins in the
ecosystem.


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG97msBIz5InVG_00XP7-_bP0GwJdnHzdQAOxG8ktLnaMqZyrYWSCg2lkdWF0LWJhc2VlMC43LjBpZHVhdF9iYXNlgmlkdWF0X2NvcmVlMC43LjA
 [__link0]: https://docs.rs/duat-base/0.7.0/duat_base/widgets/index.html
 [__link1]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::LineNumbers
 [__link10]: https://docs.rs/duat_core/0.7.0/duat_core/?search=context::error
 [__link11]: https://docs.rs/duat_core/0.7.0/duat_core/?search=context::warn
 [__link12]: https://docs.rs/duat_core/0.7.0/duat_core/?search=context::info
 [__link13]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::LogBook
 [__link14]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::Notifications
 [__link15]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::Pager
 [__link16]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Mode
 [__link17]: https://docs.rs/duat-base/0.7.0/duat_base/modes/index.html
 [__link18]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::Prompt
 [__link19]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::PromptMode
 [__link2]: https://docs.rs/duat_core/0.7.0/duat_core/?search=buffer::Buffer
 [__link20]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::PromptLine
 [__link21]: https://docs.rs/duat_core/0.7.0/duat_core/?search=ui::Widget
 [__link22]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::Pager
 [__link23]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Mode
 [__link24]: https://docs.rs/duat_core/0.7.0/duat_core/?search=ui::Widget
 [__link25]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::LogBook
 [__link26]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::PromptLine
 [__link27]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::PromptMode
 [__link28]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::RunCommands
 [__link29]: https://docs.rs/duat_core/0.7.0/duat_core/?search=cmd::Parameter
 [__link3]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::PromptLine
 [__link30]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::PipeSelections
 [__link31]: https://docs.rs/duat_core/0.7.0/duat_core/?search=buffer::Buffer
 [__link32]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::IncSearch
 [__link33]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::IncSearcher
 [__link34]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::IncSearch
 [__link35]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::IncSearcher
 [__link36]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::SearchFwd
 [__link37]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor
 [__link38]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::SearchRev
 [__link39]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor
 [__link4]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::StatusLine
 [__link40]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::ExtendFwd
 [__link41]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor
 [__link42]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::ExtendRev
 [__link43]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor
 [__link44]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::IncSearcher
 [__link45]: https://docs.rs/duat-kak/latest/duat_kak
 [__link46]: https://docs.rs/duat-base/0.7.0/duat_base/hooks/index.html
 [__link47]: https://docs.rs/duat-base/0.7.0/duat_base/?search=hooks::SearchUpdated
 [__link48]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::IncSearch
 [__link49]: https://docs.rs/duat-base/0.7.0/duat_base/?search=hooks::SearchPerformed
 [__link5]: https://docs.rs/duat_core/0.7.0/duat_core/?search=data::RwData
 [__link50]: https://docs.rs/duat-base/0.7.0/duat_base/?search=modes::IncSearch
 [__link51]: https://docs.rs/duat-base/0.7.0/duat_base/state/index.html
 [__link52]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::StatusLine
 [__link53]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::StatusLine
 [__link6]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::status
 [__link7]: https://docs.rs/duat_core/0.7.0/duat_core/?search=text::txt
 [__link8]: https://docs.rs/duat-base/0.7.0/duat_base/?search=widgets::Notifications
 [__link9]: https://docs.rs/duat_core/0.7.0/duat_core/?search=context::Logs
