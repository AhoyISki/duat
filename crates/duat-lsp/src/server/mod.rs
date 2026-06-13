use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex, OnceLock},
};

use duat_core::{
    Ns,
    buffer::Buffer,
    context::Handle,
    data::Pass,
    hook::{self, ConfigUnloaded},
    storage::{
        self,
        bincode::{Decode, Encode},
    },
};
use duat_filetype::FileType;
use jsonrpc_lite::Id;
use lsp_types::{
    DidChangeConfigurationParams, ServerCapabilities, ServerInfo,
    notification::{DidChangeConfiguration, Exit, Notification},
    request::{Initialize, Request, Shutdown},
};

use crate::{
    config::{self, LanguageServerConfig, get_initialize_params},
    parser::Parser,
    server::bridge::ServerBridge,
};

mod bridge;
mod handler;

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

    /// Sends a request alongside its parameters.
    ///
    /// This request will be handled immediately, as opposed to
    /// queued for execution with a [`Pass`].
    #[allow(unused)]
    pub fn send_sync_request<R: Request + 'static>(&self, params: R::Params) -> R::Result {
        self.bridge.send_sync_request::<R>(params)
    }

    /// Sends a request alognside its parameters and a custom id.
    pub fn send_request_with_id<R: Request + 'static>(
        &self,
        id: Id,
        params: R::Params,
        callback: impl FnOnce(&mut Pass, R::Result) + Send + 'static,
    ) {
        self.bridge.send_request_with_id::<R>(id, params, callback)
    }

    /// Send a request to refresh the semantic tokens of a given
    /// [`Handle`].
    pub fn send_semantic_tokens_request(
        &self,
        handle: &Handle<Buffer>,
        version: u64,
        parser: &Parser,
    ) {
        if let Some(opts) = self
            .capabilities()
            .as_ref()
            .and_then(|cap| cap.semantic_tokens_provider.clone())
        {
            self.bridge
                .send_semantic_tokens_request(handle, version, parser, opts);
        }
    }

    /// Sends the initialization requests for a given [`Path`].
    fn send_initialize_request(&self, path: PathBuf) {
        let params = get_initialize_params(&path, &self.config);
        self.bridge.send_request::<Initialize>(params, {
            let server = self.clone();
            move |_, result| {
                server.bridge.declare_initialized();
                server
                    .bridge
                    .encoding
                    .set(crate::Encoding::new(&result.capabilities))
                    .ok()
                    .unwrap();

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

        self.send_notification::<DidChangeConfiguration>(DidChangeConfigurationParams {
            settings: serde_json::Value::Object(serde_json::Map::default()),
        });
    }

    /// Try to get the [`ServerCapabilities`], will fail if they
    /// haven't been set yet.
    pub fn capabilities(&self) -> Option<&ServerCapabilities> {
        Some(&self.init_parts.get()?.capabilities)
    }

    /// The name of the language server.
    pub fn name(&self) -> &str {
        self.bridge.name()
    }

    /// The [`Ns`] of this `Server`.
    pub fn ns(&self) -> Ns {
        self.bridge.ns()
    }
}

pub fn on_servers_list(func: impl FnOnce(&[Server])) {
    func(&SERVERS.lock().unwrap());
}

pub fn get_servers_for(buf: &Buffer) -> Option<Vec<Server>> {
    let filetype = buf.filetype()?;
    let path = buf.path();

    let (config, user_provided) = config::get_for(filetype)?;

    let mut servers = SERVERS.lock().unwrap();

    let servers = config.into_iter().filter_map(|(server_name, config)| {
        let rootdir = config.rootdir_for(&path);

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
            servers.push(server.clone());

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

            let roots = Arc::new(Mutex::new(vec![config.rootdir_for(&path)]));
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

/// Call a function on each [`Server`].
pub fn on_all_servers(mut func: impl FnMut(&Server)) {
    let servers = SERVERS.lock().unwrap();
    for server in servers.iter() {
        func(server)
    }
}

/// Call a function on a [`Server`] with the given [`Ns`].
#[track_caller]
pub fn on_ns<Ret>(ns: Ns, func: impl FnOnce(&Server) -> Ret) -> Option<Ret> {
    let servers = SERVERS.lock().unwrap();
    servers.iter().find(|server| server.ns() == ns).map(func)
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

        let capabilities = serde_json::from_str(&capabilities).unwrap();
        let encoding = crate::Encoding::new(&capabilities);

        Ok(Self {
            capabilities,
            info: info.map(|value| serde_json::from_str(&value).unwrap()),
            offset_encoding: Decode::decode(decoder)?,
            roots: Decode::decode(decoder)?,
            bridge: {
                let bridge: ServerBridge = Decode::decode(decoder)?;
                bridge.encoding.set(encoding).ok().unwrap();
                bridge
            },
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

impl std::cmp::Eq for Server {}

impl std::cmp::PartialEq for Server {
    fn eq(&self, other: &Self) -> bool {
        self.ns() == other.ns()
    }
}

impl std::hash::Hash for Server {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ns().hash(state)
    }
}
