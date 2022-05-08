use std::env;

use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
