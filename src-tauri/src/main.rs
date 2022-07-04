#![cfg_attr(
  all(not(debug_assertions), target_os = "windows"),
  windows_subsystem = "windows"
)]
use std::thread;
use tauri::{AppHandle, Window};
mod controller;
use simplelog::*;

#[tauri::command]
fn init_tasks(_app: AppHandle, window: Window) -> Result<(), String> {
  log::info!("Spawning backend thread");
  thread::spawn(move || controller::task(window));
  Ok(())
}

fn main() {
  CombinedLogger::init(vec![
    TermLogger::new(
      LevelFilter::Info,
      Config::default(),
      TerminalMode::Mixed,
      ColorChoice::Auto,
    ),
    //WriteLogger::new(LevelFilter::Info, Config::default(), File::create("my_rust_binary.log").unwrap()),
  ])
  .unwrap();

  tauri::Builder::default()
    .invoke_handler(tauri::generate_handler![init_tasks])
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
}
