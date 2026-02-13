use duat_core::{Plugin, hook::{self, BufferOpened}};

use crate::server_bridge::ServerBridge;

mod server_bridge;
mod config;

pub struct DuatLsp;

impl Plugin for DuatLsp {
    fn plug(self, _: &duat_core::Plugins) {
        hook::add::<BufferOpened>(|pa, handle| {
            if handle.read(pa).path() == "/home/mateus/.config/duat/src/lib.rs" {
                ServerBridge::new("rust-analyzer", "rust-analyzer", &[], &[]).unwrap();
                
            }
        });
    }
}
