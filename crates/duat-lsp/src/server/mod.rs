use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use duat_core::context;
use duat_filetype::FileType;
use lsp_types::{ServerCapabilities, ServerInfo, notification::Notification, request::Initialize};

use crate::{
    config::{self, LanguageServerConfig, get_initialize_params},
    server::server_bridge::ServerBridge,
};

mod server_bridge;

static SERVERS: Mutex<Vec<Server>> = Mutex::new(Vec::new());

#[derive(Clone)]
pub struct Server {
    name: Arc<str>,
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

        if let Some(server) = servers.iter().find(|server| *server_name == *server.name)
            && server.can_serve(&rootdir)
            && let mut inner = server.inner.lock().unwrap()
            && inner.config == config
        {
            if !inner.roots.contains(&rootdir) {
                inner.roots.push(rootdir);
            }

            Some(server.clone())
        } else {
            let bridge = match ServerBridge::new(&server_name, &rootdir, &config) {
                Ok(bridge) => bridge,
                Err(err) if user_provided => {
                    context::error!("{err}");
                    return None;
                }
                Err(_) => return None,
            };

            let roots = vec![config.rootdir_for(path)];
            let server = Server {
                name: server_name.into(),
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
