#![cfg_attr(
  all(not(debug_assertions), target_os = "windows"),
  windows_subsystem = "windows"
)]
use std::thread;
use tauri::{AppHandle, Window};
mod controller;

#[tauri::command]
fn init_tasks(_app: AppHandle, window: Window) -> Result<(), String> {
  thread::spawn(move || controller::backend::task(window));
  Ok(())
}

fn main() {
  tauri::Builder::default()
    .invoke_handler(tauri::generate_handler![init_tasks])
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
}
