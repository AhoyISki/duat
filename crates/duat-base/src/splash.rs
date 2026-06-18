use std::sync::{
    LazyLock, Mutex,
    atomic::{AtomicBool, Ordering::Relaxed},
};

use duat_core::{
    Ns,
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    hook::{self, BufferUpdated, ConfigLoaded},
    mode::{self, KeyCode, KeyEvent, KeyMod},
    text::{Inlay, Link, Mask, Spacer, Text},
    txt,
    ui::PrintInfo,
};

/// Wether to center the splash [`Text`] vertically.
pub fn center_splash_vertically(yes: bool) {
    CENTER_VERT.store(yes, Relaxed);
}

/// Defines the splashscreen function.
pub fn set_splashscreen_fn(func: impl FnMut(&Pass) -> Text + Send + 'static) {
    *SPLASH_FN.lock().unwrap() = Some(Box::new(func));
}

/// Adds the hooks for creating a splash screen, if there is only one
/// `Buffer` and it's a scratch `Buffer`.
pub fn add_splash_hook() {
    hook::add::<ConfigLoaded>(|pa, _| {
        let buffers = context::buffers(pa);
        if let Some(buffer) = buffers.first().cloned()
            && buffers.len() == 1
            && let buf = buffer.read(pa)
            && buf.is_scratch()
        {
            let ns = Ns::new();

            hook::add::<BufferUpdated>(move |pa, buffer| {
                let buf = buffer.read(pa);
                let text = buf.text();
                if context::buffers(pa).len() != 1 || !(text == "\n" || text == "\r\n") {
                    buffer.text_mut(pa).remove_tags(ns, ..);
                    hook::remove(ns);
                    return;
                }

                add_splash(pa, buffer, ns);

                buffer.area().set_print_info(pa, PrintInfo::default());
            })
            .filter(buffer.clone())
            .grouped(ns)
            .lateness(usize::MAX);
        }
    })
    .lateness(usize::MAX);
}

fn add_splash(pa: &mut Pass, buffer: &Handle<Buffer>, ns: Ns) {
    let splash_fn_text = {
        let mut splash_fn = SPLASH_FN.lock().unwrap();
        splash_fn.as_mut().map(|func| func(pa))
    };
    let splash = splash_fn_text.as_ref().unwrap_or(&SPLASH);

    buffer.text_mut(pa).remove_tags(ns, ..);

    let width = buffer.area().width(pa);
    let height = buffer.area().height(pa);

    let size = match buffer.area().size_of_text(pa, buffer.opts(pa), splash) {
        Err(_) => return,
        Ok(size) if size.x > width || size.y > height => return,
        Ok(size) => size,
    };

    let mask = Mask("active");
    let nls = if CENTER_VERT.load(Relaxed) {
        // +1 because of the initial '\n' from the scratch buffer.
        "\n".repeat(((height - (size.y + 1.0)) / 2.0) as usize)
    } else {
        String::new()
    };

    let mut text = buffer.text_mut(pa);
    text.insert_tag(ns, 1, Inlay::new(txt!("{mask}{nls}{splash}")));
}

static SPLASH: LazyLock<Text> = LazyLock::new(|| {
    const VERSION: &str = env!("CARGO_PKG_VERSION");
    let quit_keys = mode::keys_to_text(&mode::str_to_keys(":q<Enter>"));
    let keys_keys = mode::keys_to_text(&[KeyEvent::new(KeyCode::Char('k'), KeyMod::CONTROL)]);

    let book_link = Link::new("https://ahoyiski.github.io/duat");
    let kofi_link = Link::new("https://ko-fi.com/ahoyiski");
    let gh_sponsors_link = Link::new("https://github.com/sponsors/ahoyiski");

    let f = duat_core::form::id_of!("splash.title");

    let mut text = txt!(
        r#"{Spacer}   {f}..[]                                       {f}s[]       {Spacer}
{Spacer}  {f}dF[]                                        {f}:8[]        {Spacer}
{Spacer} {f}'88bu.[]         {f}x.[]    {f}.[]                    {f}.88[]        {Spacer}
{Spacer} {f}'*88888bu[]    {f}.@88k[]  {f}z88u[]         {f}u[]       {f}:888ooo[]     {Spacer}
{Spacer}   {f}^"*8888N[]  {f}~"8888[] {f}^8888[]      {f}us888u.[]  {f}-*8888888[]     {Spacer}
{Spacer}  {f}beWE[] {f}"888L[]   {f}8888[]  {f}888R[]   {f}.@88[] {f}"8888"[]   {f}8888[]        {Spacer}
{Spacer}  {f}888E[]  {f}888E[]   {f}8888[]  {f}888R[]   {f}9888[]  {f}9888[]    {f}8888[]        {Spacer}
{Spacer}  {f}888E[]  {f}888E[]   {f}8888[]  {f}888R[]   {f}9888[]  {f}9888[]    {f}8888[]        {Spacer}
{Spacer}  {f}888E[]  {f}888F[]   {f}8888[] {f},888B[] {f}.[] {f}9888[]  {f}9888[]   {f}.8888Lu=[]     {Spacer}
{Spacer} {f}.888N..888[]   {f}"8888Y[] {f}8888"[]  {f}9888[]  {f}9888[]   {f}^%888*[]       {Spacer}
{Spacer}  {f}`"888*""[]     {f}`Y"[]   {f}'YP[]    {f}"888*""888"[]    {f}'Y"[]        {Spacer}
{Spacer}     {f}""[]                      {f}^Y"[]   {f}^Y'[]                {Spacer}

{Spacer} [splash.version]duat v{VERSION}[]     {Spacer}
{Spacer}[splash.divider]────────────────────────────────────────────────[]    {Spacer}
{Spacer}Welcome to duat!           Read the book 📖         {Spacer}
{Spacer}Consider donating!         On ko-fi ☕              {Spacer}
{Spacer}                           On github sponsors 🐙    {Spacer}
{Spacer}[splash.divider]────────────────────────────────────────────────[]    {Spacer}
{Spacer}type   {quit_keys}           to quit duat             {Spacer}
{Spacer}type   {keys_keys}               for keybindings          {Spacer}
"#
    );

    text.insert_tag(Ns::basic(), 854..872, book_link);
    text.insert_tag(Ns::basic(), 909..921, kofi_link);
    text.insert_tag(Ns::basic(), 963..986, gh_sponsors_link);

    text
});

static CENTER_VERT: AtomicBool = AtomicBool::new(true);
#[allow(clippy::type_complexity)]
static SPLASH_FN: Mutex<Option<Box<dyn FnMut(&Pass) -> Text + Send>>> = Mutex::new(None);
