//! A command to show which [`Form`]s are applied at any given hovered
//! position.
//!
//! This command can be _very_ useful for people to understand which
//! forms they need to alter in order to achieve some result.

use duat_core::{
    Ns, cmd,
    context::{self, Handle},
    data::Pass,
    form::{self, FormId, FormKind, MaskId},
    hook::{self, KeyTyped, OnMouseEvent},
    mode::MouseEventKind,
    text::{Text, TextPart, TwoPoints},
    txt,
    ui::{DynSpawnSpecs, Orientation},
};

use crate::widgets::Sections;

/// Adds the `inspect-forms` command.
pub fn add_command() {
    let mut is_toggled = false;
    let forms_ns = Ns::new();
    let masks_ns = Ns::new();

    cmd::add("inspect-forms", move |pa: &mut Pass| {
        if is_toggled {
            is_toggled = false;
            hook::remove(forms_ns);

            for sections in context::windows().handles_of::<Sections>(pa) {
                Sections::remove_section(pa, &sections, forms_ns);
                Sections::remove_section(pa, &sections, masks_ns);
            }

            for widget in Vec::from_iter(context::windows().handles(pa)) {
                widget.text_mut(pa).remove_tags(forms_ns, ..);
            }
        } else {
            is_toggled = true;

            let mut sections: Option<Handle<Sections>> = None;
            let specs = DynSpawnSpecs {
                orientation: Orientation::HorTopRight,
                inside: true,
                ..Default::default()
            };

            hook::add::<OnMouseEvent>(move |pa, mouse_event| {
                if let Some(sections) = sections.take() {
                    _ = sections.close(pa);
                }

                let Some(tpp) = mouse_event.points else {
                    return;
                };

                let buffer = context::current_buffer(pa);

                if let MouseEventKind::Moved = mouse_event.kind {
                    let (forms, masks) =
                        get_forms_and_masks(mouse_event.handle.text(pa), tpp.points());
                    let mut builder = Text::builder();

                    if !forms.is_empty() {
                        for (id, priority) in forms.into_iter().rev() {
                            builder.push(format_form(id, Some(priority), &masks));
                        }
                    } else {
                        builder.push("No forms are applied");
                    }

                    sections = {
                        let sections = Sections::new(
                            forms_ns,
                            builder.build(),
                            Some("Applied forms".to_string()),
                            1,
                        );

                        buffer.spawn_on_widget(pa, sections, specs)
                    };

                    if let Some(sections) = sections.clone() {
                        if !masks.is_empty() {
                            let builder =
                                masks
                                    .into_iter()
                                    .fold(Text::builder(), |mut builder, mask| {
                                        builder.push(mask.name());
                                        builder.push("\n");
                                        builder
                                    });

                            Sections::set_section(
                                pa,
                                &sections,
                                masks_ns,
                                builder.build(),
                                Some("Applied masks".to_string()),
                                0,
                            );
                        }

                        hook::add_once::<KeyTyped>(move |pa, _| _ = sections.close(pa));
                    }
                }
            })
            .grouped(forms_ns)
            .lateness(usize::MAX);
        }

        Ok(None)
    })
    .doc(
        txt!("Toggle inspection of which [a]Forms[] are being applied at any given point"),
        Some(txt!(
            "This command will toggle a widget that will follow the cursor around, showing\nwhich \
             [a]Forms[] and [a]masks[] are being used on the position under the cursor.\n\nThe \
             arrows indicate which forms inherit from who, while [punctuation.inspect_forms]=>[] \
             indicates when a\nform gets mapped to another because of a mask."
        )),
    );
}

fn format_form(id: FormId, priority: Option<u8>, masks: &[MaskId]) -> Text {
    use FormKind::*;
    let form = form::from_id(id);
    let punct = form::id_of!("punctuation.inspect_forms").to_tag(250);

    let (maps_txt, mapped_ids) = {
        let mapped_ids = Vec::from_iter(
            masks
                .iter()
                .map(|mask| mask.mapping_for(id))
                .filter(|mapped_id| *mapped_id != id),
        );

        if mapped_ids.is_empty() {
            (Text::new(), mapped_ids)
        } else {
            let mut builder = Text::builder();

            builder.push(txt!(" {punct}=>[] "));

            for (idx, id) in mapped_ids.iter().copied().enumerate() {
                builder.push(txt!("{id}{}", id.name()));
                if idx == 0 && mapped_ids.len() > 1 {
                    builder.push(txt!("{punct}|"));
                }
            }

            (builder.build(), mapped_ids)
        }
    };

    let priority_txt = if let Some(priority) = priority {
        txt!("{punct},[] priority [a]{priority}")
    } else {
        Text::new()
    };

    let mut builder = Text::builder();

    builder.push(match form.kind() {
        Normal | Weak => txt!("{id}{}[]{priority_txt}{maps_txt}\n", id.name()),
        Ref(..) | WeakRef(..) if !mapped_ids.is_empty() => {
            txt!("{id}{}[]{priority_txt}{maps_txt}\n", id.name())
        }
        Ref(refed, _) | WeakRef(refed, _) => {
            let mut text = format_form(refed, None, masks);
            let nl_ranges = Vec::from_iter(text.matches("\n").map(|strs| strs.byte_range()));

            for range in nl_ranges.into_iter().rev() {
                if range.end < text.len() {
                    text.replace_range(range, "\n  ");
                }
            }

            txt!(
                "{id}{}[]{priority_txt}{maps_txt}\n {punct}↳[]{text}",
                id.name()
            )
        }
    });

    for id in mapped_ids
        .into_iter()
        .filter(|id| matches!(form::from_id(*id).kind(), Ref(..) | WeakRef(..)))
    {
        let mut text = format_form(id, None, masks);
        let nl_ranges = Vec::from_iter(text.matches("\n").map(|strs| strs.byte_range()));

        for range in nl_ranges.into_iter().rev() {
            if range.end < text.len() {
                text.replace_range(range.clone(), "\n│");
                text.insert_tag(Ns::basic(), range.end..range.end + 2, punct);
            }
        }

        text.replace_range(0..0, "├");
        text.insert_tag(Ns::basic(), 0..2, punct);

        builder.push(text);
    }

    builder.build()
}

fn get_forms_and_masks(text: &Text, points: TwoPoints) -> (Vec<(FormId, u8)>, Vec<MaskId>) {
    let mut forms = Vec::new();
    let mut masks = Vec::new();

    for place in text
        .iter_fwd(points)
        .take_while(|place| place.points() <= points)
    {
        match place.part {
            TextPart::PushForm(id, priority) => forms.push((id, priority)),
            TextPart::PopForm(id) => {
                if let Some(idx) = forms.iter().position(|(l, _)| *l == id) {
                    forms.remove(idx);
                }
            }
            TextPart::PushMask(id) => masks.push(id),
            TextPart::PopMask(id) => {
                if let Some(idx) = masks.iter().position(|l| *l == id) {
                    masks.remove(idx);
                }
            }
            TextPart::Overlay(text) => {
                let (o_forms, o_masks) = get_forms_and_masks(text, TwoPoints::default());
                forms.extend(o_forms);
                masks.extend(o_masks);
            }
            _ => {}
        }
    }

    (forms, masks)
}
