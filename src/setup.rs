//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurBuffer`] and [`CurWidget`]
//! variables are not set in the start of the program, since they
//! require a [`Ui`], which cannot be defined in static time.
use std::{
    any::TypeId,
    path::Path,
    sync::{LazyLock, Mutex},
};

use duat_base::{
    modes::Pager,
    widgets::{FooterWidgets, Gutter, LogBook, WhichKey, status},
};
use duat_core::{
    Ns,
    buffer::{BufferOpts, History, PathKind},
    context::{self, cache},
    data::Pass,
    hook::{BufferOpened, KeyTyped, ModeSwitched},
    notify::{FromDuat, Watcher},
    text::txt,
    ui::{DynSpawnSpecs, Orientation, Ui},
};
use duat_filetype::FileType;
#[cfg(feature = "term-ui")]
use duat_term::VertRule;

use crate::{
    form,
    hook::{self, BufferClosed, BufferUnloaded, WindowOpened},
    mode,
    opts::{OPTS, Opts},
    prelude::BufferSaved,
};

#[doc(hidden)]
#[inline(never)]
pub fn full_setup(setup: fn(&mut Opts)) -> (Ui, Vec<TypeId>, BufferOpts) {
    // Check this in here, in order to not give warnings to crates that
    // depend on duat without this feature.
    if !cfg!(feature = "term-ui") {
        panic!("No ui for running Duat has been chosen!");
    }

    mode::set_default(Pager::<LogBook>::new());
    mode::map::<mode::User>("L", |pa: &mut _| mode::set(pa, Pager::<LogBook>::new()))
        .doc(txt!("Open [mode]Logs"));

    // Cache hooks

    #[cfg(feature = "treesitter")]
    {
        use duat_core::Plugins;

        Plugins::_new().require::<duat_match_pairs::MatchPairs>();
    }

    duat_core::Plugins::_new().require::<duatmode::DuatMode>();
    let duat_base_ns = Ns::new();
    crate::prelude::plug(duat_base::DuatBase(duat_base_ns));

    crate::colorscheme::add_default();

    #[cfg(feature = "term-ui")]
    let ui = duat_core::ui::Ui::new::<duat_term::Ui>();

    form::enable_mask("error");
    form::enable_mask("warn");
    form::enable_mask("info");
    form::enable_mask("inactive");
    form::enable_mask("current_line");

    let mut opts = OPTS.lock().unwrap();
    duat_core::utils::catch_panic(|| setup(&mut opts));

    if !opts.enabled_hooks.default_opts_parser {
        hook::remove(duat_base_ns);
    }

    enable_whichkey_hooks(&opts);
    enable_layout_hooks(&mut opts);
    enable_buffer_hooks(&opts);

    // Layout hooks

    let default_buffer_opts = {
        BufferOpts {
            highlight_current_line: opts.highlight_current_line,
            wrap_lines: opts.wrap_lines,
            wrap_on_word: opts.wrap_on_word,
            wrapping_cap: opts.wrapping_cap,
            indent_wraps: opts.indent_wraps,
            tabstop: opts.tabstop,
            scrolloff: opts.scrolloff,
            force_scrolloff: opts.force_scrolloff,
            extra_word_chars: opts.extra_word_chars,
            indent_str: opts.indent_str,
            indent_str_on_empty: opts.indent_str_on_empty,
            indent_tab_str: opts.indent_tab_str,
            space_char: opts.space_char,
            newline: opts.newline,
            newline_on_empty: opts.newline_on_empty,
            newline_trailing: opts.newline_trailing,
        }
    };

    let already_plugged = std::mem::take(&mut *ALREADY_PLUGGED.lock().unwrap());

    (ui, already_plugged, default_buffer_opts)
}
fn enable_layout_hooks(opts: &mut Opts) {
    if opts.enabled_hooks.default_buffer_widgets {
        hook::add::<BufferOpened>(|pa, buffer| {
            #[cfg(feature = "term-ui")]
            VertRule::builder().push_on(pa, buffer);
            OPTS.lock().unwrap().line_numbers.push_on(pa, buffer);
            #[cfg(feature = "term-ui")]
            VertRule::builder().push_on(pa, buffer);
            Gutter::builder().push_on(pa, buffer);
        });
    }

    if opts.enabled_hooks.default_footer_widgets {
        let one_line_footer = opts.one_line_footer;
        let footer_on_top = opts.footer_on_top;
        let mut status_fmt_fn = opts.status_fmt_fn.take();
        let notifications = opts.notifications.clone();
        hook::add::<WindowOpened>(move |pa, window| {
            use crate::{state::*, text::Spacer};

            let status = match &mut status_fmt_fn {
                Some(status_fn) => status_fn(pa),
                None if one_line_footer => {
                    let mode_txt = mode_txt();
                    let duat_param_txt = duat_param_txt();
                    status!("{Spacer}{name_txt} {mode_txt} {sels_txt} {duat_param_txt} {main_txt}")
                }
                None => {
                    let mode_txt = mode_txt();
                    let duat_param_txt = duat_param_txt();
                    status!("{mode_txt} {name_txt}{Spacer}{sels_txt} {duat_param_txt} {main_txt}")
                }
            };

            let mut footer = FooterWidgets::new(status).notifs(notifications.clone());

            if footer_on_top {
                footer = footer.above();
            }

            if one_line_footer {
                footer = footer.one_line();
            }

            footer.push_on(pa, window);
        });
    }

    let mut logs = opts.logs;
    hook::add::<WindowOpened>(move |pa, window| {
        use duat_core::ui::Side::*;

        let too_tall = logs.height > window.height();
        let too_wide = logs.width > window.width();
        logs.hidden = match (logs.side, too_tall, too_wide) {
            (Above | Below, true, _) => true,
            (Above | Below, false, _) => logs.hidden,
            (Right | Left, _, true) => true,
            (Right | Left, _, false) => logs.hidden,
        };

        logs.push_on(pa, window);
    });
}

fn enable_whichkey_hooks(opts: &Opts) {
    if !opts.enabled_hooks.show_whichkey {
        return;
    }

    // WhichKey hooks
    let wk_specs = DynSpawnSpecs {
        orientation: Orientation::VerRightBelow,
        width: None,
        height: Some(20.0),
        hidden: false,
        inside: true,
    };

    let show_whichkey = move |pa: &mut Pass| {
        let mut wk_specs = wk_specs;
        let opts = OPTS.lock().unwrap();
        wk_specs.orientation = opts.whichkey.orientation;
        WhichKey::open(
            pa,
            opts.whichkey.fmt_getter.as_ref().map(|fg| fg()),
            wk_specs,
        );
    };

    let cur_seq = mode::current_sequence();
    hook::add::<KeyTyped>(move |pa, key_event| {
        if cur_seq.call(pa).0.is_empty() && OPTS.lock().unwrap().help_key == Some(key_event) {
            show_whichkey(pa);
        }
    });

    let cur_seq = mode::current_sequence();
    hook::add::<KeyTyped>(move |pa, _| {
        let opts = OPTS.lock().unwrap();
        let current_ty = mode::current_type_id();
        let cur_seq = cur_seq.call(pa).0;
        if !cur_seq.is_empty() || opts.whichkey.always_shown_modes.contains(&current_ty) {
            drop(opts);
            show_whichkey(pa);
        }
    });

    hook::add::<ModeSwitched>(move |pa, _| {
        let opts = OPTS.lock().unwrap();
        if opts
            .whichkey
            .always_shown_modes
            .contains(&mode::current_type_id())
        {
            drop(opts);
            show_whichkey(pa);
        } else {
            for handle in context::windows().handles_of::<WhichKey>(pa) {
                let _ = handle.close(pa);
            }
        }
    });
}

fn enable_buffer_hooks(opts: &Opts) {
    hook::add::<BufferClosed>(|pa, handle| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        cache::delete_for::<History>(&path);
        if !buffer.exists() || buffer.text().has_unsaved_changes() {
            cache::delete(path);
        }
    });

    hook::add::<BufferUnloaded>(|pa, handle| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        buffer.text_mut().new_moment();

        if let Some(main) = buffer.selections().get_main()
            && let Err(err) = cache::store(&path, main.clone())
        {
            context::error!("{err}");
        }

        duat_core::try_or_log_err! {
            handle.area().store_cache(pa, &path)?;
        }
    });

    if opts.enabled_hooks.cache_cursor_position {
        hook::add::<BufferClosed>(|pa, handle| {
            let buffer = handle.write(pa);

            let path = buffer.path();
            buffer.text_mut().new_moment();

            if let Some("gitcommit") = path.filetype() {
                cache::delete(path);
                return;
            }

            if let Some(main) = buffer.selections().get_main()
                && let Err(err) = cache::store(&path, main.clone())
            {
                context::error!("{err}");
            }

            duat_core::try_or_log_err! {
                handle.area().store_cache(pa, &path)?;
            }
        });
    }

    if opts.enabled_hooks.reload_on_save {
        hook::add::<BufferSaved>(|pa, (handle, is_closing)| {
            let path = handle.read(pa).path();
            let path = Path::new(&path);
            if !is_closing
                && let Ok(crate_dir) = crate::utils::crate_dir()
                && path.starts_with(crate_dir)
            {
                crate::prelude::cmd::queue("reload");
            }
        });
    }

    if opts.enabled_hooks.auto_reload_buffers {
        hook::add::<BufferOpened>(|pa, handle| {
            if let PathKind::SetExists(path) = handle.read(pa).path_kind()
                && let Err(err) = BUFFER_WATCHER.watch(&path)
            {
                context::warn!("{err}");
            }
        });
    }
}

// Setup statics.
pub static ALREADY_PLUGGED: Mutex<Vec<TypeId>> = Mutex::new(Vec::new());
static BUFFER_WATCHER: LazyLock<Watcher> = LazyLock::new(|| {
    Watcher::new(|event, from_duat| {
        use dissimilar::Chunk::*;
        use duat_core::notify::event::*;

        if let (Ok(Event { kind, paths, .. }), FromDuat::No) = (event, from_duat)
            && let EventKind::Access(AccessKind::Close(AccessMode::Write)) = kind
        {
            context::queue(move |pa| {
                for path in paths {
                    if let Some(buffer) = context::get_buffer_by_path(pa, &path)
                        && let Ok(new_string) = std::fs::read_to_string(path)
                    {
                        let old_string = buffer.text(pa).to_string();
                        let diffs = dissimilar::diff(&old_string, &new_string);
                        if diffs.is_empty() {
                            return;
                        }

                        context::info!("{} reloaded", buffer.read(pa).name_txt());

                        let mut text = buffer.text_mut(pa);
                        text.new_moment();

                        let mut start_byte = 0;

                        for diff in diffs {
                            match diff {
                                Equal(eq) => start_byte += eq.len(),
                                Delete(del) => {
                                    text.replace_range(start_byte..start_byte + del.len(), "")
                                }
                                Insert(ins) => {
                                    text.replace_range(start_byte..start_byte, ins);
                                    start_byte += ins.len();
                                }
                            }
                        }
                    }
                }
            });
        }
    })
    .unwrap()
});
