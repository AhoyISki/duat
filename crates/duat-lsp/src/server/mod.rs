use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use duat_core::{
    context::{self},
    hook::{self, ConfigUnloaded},
    storage::{
        self,
        bincode::{Decode, Encode},
    },
};
use duat_filetype::FileType;
use lsp_types::{
    ServerCapabilities, ServerInfo,
    notification::{Exit, Notification},
    request::{Initialize, Shutdown},
};

use crate::{
    config::{self, LanguageServerConfig, get_initialize_params},
    server::server_bridge::ServerBridge,
};

mod server_bridge;

static SERVERS: Mutex<Vec<Server>> = Mutex::new(Vec::new());

#[derive(Clone)]
pub struct Server {
    inner: Arc<Mutex<InnerServer>>,
    bridge: ServerBridge,
}

struct InnerServer {
    config: LanguageServerConfig,
    capabilities: Option<ServerCapabilities>,
    info: Option<ServerInfo>,
    offset_encoding: Option<String>,
    roots: Vec<PathBuf>,
}

impl Server {
    /// Wether this `Server` can serve a given [`Path`]
    pub fn can_serve(&self, rootdir: &Path) -> bool {
        let inner = self.inner.lock().unwrap();
        let workspace_folder_support = inner.capabilities.as_ref().is_some_and(|caps| {
            caps.workspace.as_ref().is_some_and(|ws| {
                ws.workspace_folders
                    .as_ref()
                    .is_some_and(|wsf| wsf.supported == Some(true))
            })
        });

        inner.capabilities.is_some()
            && (inner.roots.iter().any(|root| *root == rootdir)
                || inner.config.is_single_instance
                || workspace_folder_support)
    }

    pub fn send_notification<N: Notification>(&self, params: N::Params) {
        self.bridge.send_notification::<N>(params);
    }

	/// The encoding used for positioning by this `Server`
    pub fn position_encoding(&self) -> Encoding {
        let inner = self.inner.lock().unwrap();
        match inner
            .capabilities
            .as_ref()
            .and_then(|cap| cap.position_encoding.as_ref())
            .map(|enc| enc.as_str())
        {
            Some("utf-8") | None => Encoding::Utf8,
            Some("utf-16") => Encoding::Utf16,
            Some("utf-32") => Encoding::Utf32,
            Some(_) => unreachable!(),
        }
    }

    /// Sends the initialization requests for a given [`Path`]
    fn send_initialize_request(&self, path: PathBuf) {
        let inner = self.inner.lock().unwrap();
        let params = get_initialize_params(&path, &inner.config);
        self.bridge.send_request::<Initialize>(params, {
            let server = self.clone();
            move |_, response| {
                server.bridge.declare_initialized();

                let mut inner = server.inner.lock().unwrap();

                inner.capabilities = Some(response.capabilities);
                inner.info = response.server_info;
                inner.offset_encoding = response.offset_encoding;
            }
        });
    }
}

pub fn get_servers_for(path: &Path) -> Option<Vec<Server>> {
    let filetype = path.filetype()?;
    let (config, user_provided) = config::get_for(filetype)?;

    let mut servers = SERVERS.lock().unwrap();

    let servers = config.into_iter().filter_map(|(server_name, config)| {
        let rootdir = config.rootdir_for(path);

        if let Some(server) = servers
            .iter()
            .find(|server| server_name == server.bridge.name())
            && server.can_serve(&rootdir)
            && let mut inner = server.inner.lock().unwrap()
            && inner.config == config
        {
            if !inner.roots.contains(&rootdir) {
                inner.roots.push(rootdir);
            }

            Some(server.clone())
        } else if let Some(parts) = storage::get_if(|parts: &ServerParts| {
            parts.bridge.name() == server_name && parts.roots.contains(&rootdir)
        }) {
            let server = Server {
                inner: Arc::new(Mutex::new(InnerServer {
                    config,
                    capabilities: parts.capabilities,
                    info: parts.info,
                    offset_encoding: parts.offset_encoding,
                    roots: parts.roots,
                })),
                bridge: parts.bridge,
            };

            defer_store(&server);

            Some(server)
        } else {
            let bridge = match ServerBridge::new(server_name, &config) {
                Ok(bridge) => bridge,
                Err(err) if user_provided => {
                    context::error!("{err}");
                    return None;
                }
                Err(_) => return None,
            };

            let roots = vec![config.rootdir_for(path)];
            let server = Server {
                inner: Arc::new(Mutex::new(InnerServer {
                    config,
                    capabilities: None,
                    info: None,
                    offset_encoding: None,
                    roots,
                })),
                bridge,
            };

            if !server.bridge.is_initialized() {
                server.send_initialize_request(path.to_path_buf());
            }
            servers.push(server.clone());

            defer_store(&server);

            Some(server)
        }
    });

    Some(servers.collect())
}

/// Call a function on each [`Server`]
pub fn on_all_servers(mut func: impl FnMut(&Server)) {
    let servers = SERVERS.lock().unwrap();
    for server in servers.iter() {
        func(server)
    }
}

struct ServerParts {
    capabilities: Option<ServerCapabilities>,
    info: Option<ServerInfo>,
    offset_encoding: Option<String>,
    roots: Vec<PathBuf>,
    bridge: ServerBridge,
}

fn defer_store(server: &Server) {
    let server = server.clone();
    hook::add::<ConfigUnloaded>(move |pa, is_quitting| {
        if is_quitting {
            server.bridge.send_request::<Shutdown>((), |_, _| {});
            server.bridge.send_notification::<Exit>(())
        } else {
            let mut inner = server.inner.lock().unwrap();
            _ = storage::store(pa, ServerParts {
                capabilities: inner.capabilities.take(),
                info: inner.info.take(),
                offset_encoding: inner.offset_encoding.take(),
                roots: std::mem::take(&mut inner.roots),
                bridge: server.bridge.clone(),
            });
        }
    });
}

impl<Context> Decode<Context> for ServerParts {
    fn decode<D: storage::bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, storage::bincode::error::DecodeError> {
        let capabilities: Option<String> = Decode::decode(decoder)?;
        let info: Option<String> = Decode::decode(decoder)?;

        Ok(Self {
            capabilities: capabilities.map(|value| serde_json::from_str(&value).unwrap()),
            info: info.map(|value| serde_json::from_str(&value).unwrap()),
            offset_encoding: Decode::decode(decoder)?,
            roots: Decode::decode(decoder)?,
            bridge: Decode::decode(decoder)?,
        })
    }
}

storage::bincode::impl_borrow_decode!(ServerParts);

impl Encode for ServerParts {
    fn encode<E: storage::bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), storage::bincode::error::EncodeError> {
        self.capabilities
            .as_ref()
            .map(|cap| serde_json::to_string(cap).unwrap())
            .encode(encoder)?;
        self.info
            .as_ref()
            .map(|info| serde_json::to_string(info).unwrap())
            .encode(encoder)?;
        self.offset_encoding.encode(encoder)?;
        self.roots.encode(encoder)?;
        self.bridge.encode(encoder)
    }
}

/// An encoding for text positions.
pub enum Encoding {
    Utf8,
    Utf16,
    Utf32,
}

impl Encoding {
    /// The number of bytes occupied by a single code point.
    pub fn num_bytes(&self) -> usize {
        match self {
            Encoding::Utf8 => 1,
            Encoding::Utf16 => 2,
            Encoding::Utf32 => 4,
        }
    }
}
