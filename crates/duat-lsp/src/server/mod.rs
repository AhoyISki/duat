use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex, OnceLock},
};

use duat_core::{
    context::Handle,
    data::Pass,
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
    request::{Initialize, Request, Shutdown},
};

pub use crate::server::bridge::ServerId;
use crate::{
    config::{self, LanguageServerConfig, get_initialize_params},
    parser::Parser,
    server::bridge::ServerBridge,
};

mod bridge;
mod requests;

static SERVERS: Mutex<Vec<Server>> = Mutex::new(Vec::new());

#[derive(Clone)]
pub struct Server {
    config: Arc<LanguageServerConfig>,
    init_parts: Arc<OnceLock<InitParts>>,
    bridge: ServerBridge,
    roots: Arc<Mutex<Vec<PathBuf>>>,
}

struct InitParts {
    capabilities: ServerCapabilities,
    info: Option<ServerInfo>,
    offset_encoding: Option<String>,
}

impl Server {
    /// Wether this `Server` can serve a given [`Path`].
    pub fn can_serve(&self, rootdir: &Path) -> bool {
        let has_root = self.roots.lock().unwrap().iter().any(|r| *r == rootdir);

        let Some(init_parts) = self.init_parts.get() else {
            return has_root;
        };

        let workspace_folder_support =
            init_parts
                .capabilities
                .workspace
                .as_ref()
                .is_some_and(|ws| {
                    ws.workspace_folders
                        .as_ref()
                        .is_some_and(|wsf| wsf.supported == Some(true))
                });

        has_root || self.config.is_single_instance || workspace_folder_support
    }

    pub fn send_notification<N: Notification>(&self, params: N::Params) {
        self.bridge.send_notification::<N>(params);
    }

    /// Sends a request alongside its parameters.
    pub fn send_request<R: Request + 'static>(
        &self,
        params: R::Params,
        callback: impl FnOnce(&mut Pass, R::Result) + Send + 'static,
    ) {
        self.bridge.send_request::<R>(params, callback)
    }

    /// Try to get the [`ServerCapabilities`], will fail if they
    /// haven't been set yet.
    pub fn capabilities(&self) -> Option<&ServerCapabilities> {
        Some(&self.init_parts.get()?.capabilities)
    }

    /// The `ServerId` of the `Server`.
    pub fn id(&self) -> ServerId {
        self.bridge.id()
    }

    /// Send a request to refresh the semantic tokens of a given
    /// [`Handle`].
    pub fn send_semantic_tokens_request(&self, path: PathBuf, handle: &Handle, parser: &Parser) {
        self.bridge
            .send_semantic_tokens_request(path, handle, parser);
    }

    /// Sends the initialization requests for a given [`Path`].
    fn send_initialize_request(&self, path: PathBuf) {
        let params = get_initialize_params(&path, &self.config);
        self.bridge.send_request::<Initialize>(params, {
            let server = self.clone();
            move |_, result| {
                server.bridge.declare_initialized();

                server
                    .init_parts
                    .set(InitParts {
                        capabilities: result.capabilities,
                        info: result.server_info,
                        offset_encoding: result.offset_encoding,
                    })
                    .ok()
                    .unwrap();
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
            && *server.config == config
        {
            let mut roots = server.roots.lock().unwrap();
            if !roots.contains(&rootdir) {
                roots.push(rootdir);
            }

            Some(server.clone())
        } else if let Some(parts) = storage::get_if(|parts: &ServerParts| {
            parts.bridge.name() == server_name && parts.roots.contains(&rootdir)
        }) {
            let server = Server {
                config: Arc::new(config),
                init_parts: Arc::new(OnceLock::new()),
                bridge: parts.bridge,
                roots: Arc::new(Mutex::new(parts.roots)),
            };

            server
                .init_parts
                .set(InitParts {
                    capabilities: parts.capabilities,
                    info: parts.info.clone(),
                    offset_encoding: parts.offset_encoding,
                })
                .ok()
                .unwrap();

            defer_store(&server);

            Some(server)
        } else {
            let bridge = match ServerBridge::new(server_name, &config) {
                Ok(bridge) => bridge,
                Err(err) if user_provided => {
                    duat_core::error!("{err}");
                    return None;
                }
                Err(_) => return None,
            };

            let roots = Arc::new(Mutex::new(vec![config.rootdir_for(path)]));
            let server = Server {
                config: Arc::new(config),
                init_parts: Arc::new(OnceLock::new()),
                bridge,
                roots,
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
    capabilities: ServerCapabilities,
    info: Option<ServerInfo>,
    offset_encoding: Option<String>,
    roots: Vec<PathBuf>,
    bridge: ServerBridge,
}

fn defer_store(server: &Server) {
    let server = server.clone();
    hook::add::<ConfigUnloaded>(move |pa, is_quitting| {
        if !is_quitting
            && let Some(init_parts) = server.init_parts.get()
            && let Ok(_) = storage::store(pa, ServerParts {
                capabilities: init_parts.capabilities.clone(),
                info: init_parts.info.clone(),
                offset_encoding: init_parts.offset_encoding.clone(),
                roots: server.roots.lock().unwrap().clone(),
                bridge: server.bridge.clone(),
            })
        {
        } else {
            server.bridge.send_request::<Shutdown>((), |_, _| {});
            server.bridge.send_notification::<Exit>(())
        }
    });
}

impl<Context> Decode<Context> for ServerParts {
    fn decode<D: storage::bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, storage::bincode::error::DecodeError> {
        let capabilities: String = Decode::decode(decoder)?;
        let info: Option<String> = Decode::decode(decoder)?;

        Ok(Self {
            capabilities: serde_json::from_str(&capabilities).unwrap(),
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
        serde_json::to_string(&self.capabilities)
            .unwrap()
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
