use std::{
    collections::HashMap,
    io::BufReader,
    path::{Path, PathBuf},
    process::Child,
    sync::{LazyLock, Mutex, OnceLock, mpsc},
};

use duat::context::cache::bincode::{self, config};
use duat_core::{
    session::ipc::{FinalState, MsgFromChild, MsgFromParent},
    ui::traits::RawUi,
};
use interprocess::local_socket::{
    GenericFilePath, GenericNamespaced, ListenerOptions, Name, prelude::*,
};

use crate::UiImpl;
pub use crate::ipc::clipboard::*;

static CHILD_INPUT: OnceLock<Mutex<ChildInput>> = OnceLock::new();
static FINAL_CHANNEL: LazyLock<Channel<FinalState>> = LazyLock::new(|| {
    let (tx, rx) = mpsc::channel();
    Channel { tx, rx: Mutex::new(rx) }
});

/// Send a message to the child.
pub fn send(msg: MsgFromParent) -> std::io::Result<()> {
    let mut channel = CHILD_INPUT.get().unwrap().lock().unwrap();
    if let Some(stream) = channel.stream.as_mut()
        && let Ok(_) = bincode::encode_into_std_write(&msg, stream, config::standard())
    {
        Ok(())
    } else {
        channel.stream = None;
        let mut stream = channel.listener.next().unwrap()?;
        bincode::encode_into_std_write(msg, &mut stream, config::standard()).unwrap();
        channel.stream = Some(stream);
        Ok(())
    }
}

/// Receive the [`FinalState`] event.
pub fn recv_final() -> FinalState {
    FINAL_CHANNEL.rx.lock().unwrap().recv().unwrap()
}

pub fn start(
    crate_dir: &'static Path,
    socket_dir: &Path,
    config_tx: mpsc::Sender<(PathBuf, String)>,
) -> std::io::Result<()> {
    let child_input_listener = ListenerOptions::new()
        .name(get_name(socket_dir.join("0"))?)
        .create_sync()?;
    let child_output_listener = ListenerOptions::new()
        .name(get_name(socket_dir.join("1"))?)
        .create_sync()?;

    CHILD_INPUT
        .set(Mutex::new(ChildInput {
            listener: child_input_listener,
            stream: None,
        }))
        .ok()
        .unwrap();

    std::thread::spawn(move || {
        for conn in child_output_listener.incoming() {
            let mut conn = BufReader::new(conn.unwrap());

            while let Ok(msg) = bincode::decode_from_std_read(&mut conn, config::standard()) {
                match msg {
                    MsgFromChild::FinalState(state) => {
                        duat_core::log_to_file!("Received final state");
                        FINAL_CHANNEL.tx.send(state).unwrap()
                    }
                    MsgFromChild::SpawnProcess(request) => todo!(),
                    MsgFromChild::KillProcess(id) => todo!(),
                    MsgFromChild::InterruptWrites(id) => todo!(),
                    MsgFromChild::RequestClipboard => {
                        send(MsgFromParent::ClipboardContent(get_clipboard())).unwrap();
                    }
                    MsgFromChild::UpdateClipboard(content) => set_clipboard(content),
                    MsgFromChild::RequestReload(request) => {
                        super::try_reload(crate_dir, &config_tx, request)
                    }
                    MsgFromChild::Panicked(msg) => {
                        UiImpl::close();
                        println!("{msg}");
                        std::process::exit(1);
                    }
                }
            }
        }
    });

    Ok(())
}

/// Get the name of a [`LocalSocketStream`]
fn get_name(path: PathBuf) -> std::io::Result<Name<'static>> {
    if GenericNamespaced::is_supported() {
        path.to_string_lossy()
            .to_string()
            .to_ns_name::<GenericNamespaced>()
    } else {
        path.to_fs_name::<GenericFilePath>()
    }
}

struct ChildInput {
    listener: LocalSocketListener,
    stream: Option<LocalSocketStream>,
}

/// A simple channel to send stuff over.
struct Channel<T> {
    tx: mpsc::Sender<T>,
    rx: Mutex<mpsc::Receiver<T>>,
}

#[cfg(not(target_os = "android"))]
mod clipboard {
    use std::sync::{LazyLock, Mutex};

    enum ClipboardType {
        Platform(arboard::Clipboard),
        Local(Option<String>),
    }

    static CLIPBOARD: LazyLock<Mutex<ClipboardType>> = LazyLock::new(|| {
        Mutex::new(match arboard::Clipboard::new() {
            Ok(clipb) => ClipboardType::Platform(clipb),
            Err(_) => ClipboardType::Local(None),
        })
    });

    pub fn get_clipboard() -> Option<String> {
        match &mut *CLIPBOARD.lock().unwrap() {
            ClipboardType::Platform(clipb) => clipb.get_text().ok(),
            ClipboardType::Local(text) => text.clone(),
        }
    }

    pub fn set_clipboard(text: String) {
        match &mut *CLIPBOARD.lock().unwrap() {
            ClipboardType::Platform(clipb) => clipb.set_text(text).unwrap(),
            ClipboardType::Local(old) => *old = Some(text),
        }
    }
}

#[cfg(target_os = "android")]
mod clipboard {
    pub fn get_clipboard() -> Option<String> {
        android_clipboard::get_text().ok()
    }

    pub fn set_clipboard(text: String) {
        _ = android_clipboard::set_text(text.to_string());
    }
}

/// Kills all remaining processes.
pub fn kill_remaining_processes() {
    for (_, mut child) in PROCESSES.lock().unwrap().drain() {
        _ = child.kill();
    }
}

static PROCESSES: LazyLock<Mutex<HashMap<String, Child>>> = LazyLock::new(Mutex::default);
