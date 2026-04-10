use std::{
    collections::HashMap,
    io::{BufReader, BufWriter, ErrorKind, Read, Write},
    path::{Path, PathBuf},
    process::Child,
    sync::{Arc, LazyLock, Mutex, OnceLock, mpsc},
};

use duat::context::cache::bincode::{self, config};
use duat_core::{
    process::PersistentSpawnRequest,
    session::ipc::{FinalState, MsgFromChild, MsgFromParent},
};
use interprocess::local_socket::{
    GenericFilePath, GenericNamespaced, ListenerOptions, Name, prelude::*,
};

pub use clipboard::*;

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

static CHILD_INPUT: OnceLock<Mutex<ChildInput>> = OnceLock::new();
static P_OR_F_CHANNEL: LazyLock<Channel<PanicOrFinal>> = LazyLock::new(|| {
    let (tx, rx) = mpsc::channel();
    Channel { tx, rx: Mutex::new(rx) }
});

pub enum PanicOrFinal {
    Final(FinalState),
    Panic(String),
}

/// Send a message to the child.
pub fn send(msg: MsgFromParent) -> std::io::Result<()> {
    let mut channel = CHILD_INPUT.get().unwrap().lock().unwrap();
    if let Some(stream) = channel.stream.as_mut()
        && let Some(_) = (|| {
            bincode::encode_into_std_write(&msg, stream, config::standard()).ok()?;
            stream.flush().ok()
        })()
    {
        Ok(())
    } else {
        channel.stream = None;
        let mut stream =
            BufWriter::with_capacity(BUF_CAP, channel.listener.next().unwrap().unwrap());
        bincode::encode_into_std_write(msg, &mut stream, config::standard()).unwrap();
        stream.flush().unwrap();
        channel.stream = Some(stream);
        Ok(())
    }
}

/// Receive the [`FinalState`] event.
pub fn recv_final() -> PanicOrFinal {
    P_OR_F_CHANNEL.rx.lock().unwrap().recv().unwrap()
}

pub fn start(
    crate_dir: &'static Path,
    socket_dir: &'static Path,
    config_tx: mpsc::Sender<(PathBuf, String)>,
) -> std::io::Result<()> {
    let child_input_listener = listener(socket_dir.join("0"))?;
    let child_output_listener = listener(socket_dir.join("1"))?;

    CHILD_INPUT
        .set(Mutex::new(ChildInput {
            listener: child_input_listener,
            stream: None,
        }))
        .ok()
        .unwrap();

    std::thread::spawn(move || {
        for conn in child_output_listener.incoming() {
            let mut conn = BufReader::with_capacity(256 * 1024, conn.unwrap());
            while let Ok(msg) = bincode::decode_from_std_read(&mut conn, config::standard()) {
                match msg {
                    MsgFromChild::FinalState(state) => {
                        P_OR_F_CHANNEL.tx.send(PanicOrFinal::Final(state)).unwrap();
                    }
                    MsgFromChild::SpawnProcess(request) => spawn_persistent(socket_dir, request),
                    MsgFromChild::KillProcess(id) => {
                        if let Some(mut process) = PROCESSES.lock().unwrap().remove(&id) {
                            _ = process.child.kill();
                        }
                    }
                    MsgFromChild::InterruptWrites(id) => {
                        let processes = PROCESSES.lock().unwrap();
                        let Some(process) = processes.get(&id) else {
                            continue;
                        };

                        for conn in [process.stdout_conn.clone(), process.stderr_conn.clone()] {
                            *conn.stream.lock().unwrap() = None;
                            std::thread::spawn(move || {
                                let mut stream = conn.stream.lock().unwrap();

                                if let Ok(new_stream) = conn.listener.accept() {
                                    *stream = Some(new_stream);
                                };
                            });
                        }
                    }
                    MsgFromChild::RequestClipboard => {
                        send(MsgFromParent::ClipboardContent(get_clipboard())).unwrap();
                    }
                    MsgFromChild::UpdateClipboard(content) => set_clipboard(content),
                    MsgFromChild::RequestReload(request) => {
                        super::try_reload(crate_dir, &config_tx, request)
                    }
                    MsgFromChild::Panicked(msg) => {
                        P_OR_F_CHANNEL.tx.send(PanicOrFinal::Panic(msg)).unwrap();
                    }
                }
            }
        }
    });

    Ok(())
}

fn spawn_persistent(socket_dir: &'static Path, request: PersistentSpawnRequest) {
    use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
    static PROC_ID: AtomicUsize = AtomicUsize::new(0);

    fn read_output(
        stream: Arc<Mutex<Option<LocalSocketStream>>>,
        mut reader: impl Read + Send + 'static,
        id: usize,
        caller: &str,
    ) {
        let caller = caller.to_string();
        std::thread::spawn(move || {
            let mut buf = vec![0; 8 * 1024];
            loop {
                let num_bytes = match reader.read(&mut buf) {
                    Ok(0) => return,
                    Ok(num_bytes) => num_bytes,
                    Err(err) => {
                        send(MsgFromParent::ChildIoError(
                            id,
                            caller.clone(),
                            err.raw_os_error().unwrap(),
                        ))
                        .unwrap();
                        continue;
                    }
                };

                let mut stream = stream.lock().unwrap();
                // This being None means the Listener failed to connect.
                let Some(stream) = stream.as_mut() else {
                    send(MsgFromParent::ChildBrokenPipe(id, caller.clone())).unwrap();
                    break;
                };

                // This isn't supposed to fail.
                _ = stream.write_all(&buf[..num_bytes]);
            }
        });
    }

    let mut processes = PROCESSES.lock().unwrap();
    match request.spawn() {
        Ok((caller, mut child)) => {
            let proc_id = PROC_ID.fetch_add(1, Relaxed);

            let proc_dir = socket_dir.join(format!("proc{proc_id}"));
            let stdin_listener = listener(proc_dir.join("0")).unwrap();
            let stdout_listener = listener(proc_dir.join("1")).unwrap();
            let stderr_listener = listener(proc_dir.join("2")).unwrap();

            let mut stdin = child.stdin.take().unwrap();
            let stdout = child.stdout.take().unwrap();
            let stderr = child.stderr.take().unwrap();

            send(MsgFromParent::SpawnResult(Ok(proc_id))).unwrap();

            std::thread::spawn({
                let caller = caller.clone();
                move || {
                    let mut buf = vec![0; 8 * 1024];
                    for conn in stdin_listener.incoming() {
                        let mut conn = conn.unwrap();
                        while let Ok(num_bytes) = conn.read(&mut buf)
                            && num_bytes > 0
                        {
                            if let Err(err) = stdin.write_all(&buf[..num_bytes]) {
                                if err.kind() == ErrorKind::BrokenPipe {
                                    return;
                                } else {
                                    send(MsgFromParent::ChildIoError(
                                        proc_id,
                                        caller.clone(),
                                        err.raw_os_error().unwrap(),
                                    ))
                                    .unwrap()
                                }
                            }
                        }
                    }
                }
            });

            let stdout_stream = Arc::new(Mutex::new(stdout_listener.accept().ok()));
            let stderr_stream = Arc::new(Mutex::new(stderr_listener.accept().ok()));

            read_output(stdout_stream.clone(), stdout, proc_id, &caller);
            read_output(stderr_stream.clone(), stderr, proc_id, &caller);

            processes.insert(proc_id, Process {
                child,
                stdout_conn: Arc::new(Connector {
                    listener: stdout_listener,
                    stream: stdout_stream,
                }),
                stderr_conn: Arc::new(Connector {
                    listener: stderr_listener,
                    stream: stderr_stream,
                }),
            });
        }
        Err(err) => send(MsgFromParent::SpawnResult(Err(err))).unwrap(),
    }
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
    stream: Option<BufWriter<LocalSocketStream>>,
}

/// A simple channel to send stuff over.
struct Channel<T> {
    tx: mpsc::Sender<T>,
    rx: Mutex<mpsc::Receiver<T>>,
}

/// Kills all remaining processes.
pub fn kill_remaining_processes() {
    for (_, mut process) in PROCESSES.lock().unwrap().drain() {
        _ = process.child.kill();
    }
}

fn listener(path: PathBuf) -> std::io::Result<LocalSocketListener> {
    ListenerOptions::new().name(get_name(path)?).create_sync()
}

static PROCESSES: LazyLock<Mutex<HashMap<usize, Process>>> = LazyLock::new(Mutex::default);
const BUF_CAP: usize = 256 * 1024;

struct Process {
    child: Child,
    stdout_conn: Arc<Connector>,
    stderr_conn: Arc<Connector>,
}

struct Connector {
    listener: LocalSocketListener,
    stream: Arc<Mutex<Option<LocalSocketStream>>>,
}
