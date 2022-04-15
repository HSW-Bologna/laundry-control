use super::discovery;
use super::prefs;
use super::prefs::AppPreferences;
use super::washing_machine as ws;
use super::washing_machine::WashingMachineConnection;
use futures::future::FutureExt;
use log::{error, info, warn};
use serde_json;
use serde_json::json;
use std::sync::mpsc;
use std::time::{Duration, Instant};
use tauri::Window;
use tokio::runtime::Runtime;

#[derive(Clone, serde::Deserialize)]
enum BackEndPortMessage {
  Refresh,
  Preferences(AppPreferences),
  SearchMachines,
  WashingMachineHttpConnect(String),
  SendMachineConfiguration { name: String, bytes: Vec<u8> },
  GetRemoteMachineConfiguration(String),
}

enum Message {
  ConnectTo(String),
  Refresh,
  UdpDiscovery,
  SendMachineConfiguration(String, Vec<u8>),
  GetRemoteMachineConfiguration(String),
}

pub fn task(window: Window) {
  fn emit_update(window: &Window, state: impl serde::Serialize) {
    window.emit("stateUpdate", state).unwrap();
  }

  let rt = Runtime::new().expect("Failed to build pool");
  let mut connection: Option<Box<dyn ws::WashingMachineConnection>> = None;

  if let Some(preferences) = prefs::get_user_preferences() {
    info!(
      "Saved preferences: {} {}",
      preferences.language, preferences.machine
    );
    window.emit("savedPreferences", preferences).ok();
  }

  let timeout = Duration::from_millis(100);
  let (tx, rx) = mpsc::channel::<Message>();

  let tx_clone = tx.clone();
  window.listen("backendPort", move |event| {
    if let Some(str) = event.payload() {
      use BackEndPortMessage::*;
      match serde_json::from_str::<BackEndPortMessage>(str) {
        Ok(message) => match message {
          Refresh => {
            tx_clone.send(Message::Refresh).ok();
          }
          Preferences(prefs) => {
            info!("Saving user preferences: {:?}", prefs);
            prefs::set_usage_preferences(prefs.language, prefs.machine);
          }
          SearchMachines => {
            info!("Searching for machines...");
            tx_clone.send(Message::UdpDiscovery).ok();
          }
          WashingMachineHttpConnect(ip) => {
            tx_clone.send(Message::ConnectTo(ip)).ok();
          }
          SendMachineConfiguration { name, bytes } => {
            tx_clone
              .send(Message::SendMachineConfiguration(
                String::from(name.trim_end_matches(char::from(0))),
                bytes,
              ))
              .ok();
          }

          GetRemoteMachineConfiguration(archive) => {
            tx_clone
              .send(Message::GetRemoteMachineConfiguration(archive))
              .ok();
          }
        },
        Err(e) => panic!("Error while parsing json from port: {}\n--> {:?}", str, e),
      };
    } else {
      panic!("Event without payload!");
    }
  });

  emit_update(&window, json!("null"));
  info!("Starting backend loop");
  let mut update_ts = Instant::now();

  loop {
    use Message::*;
    match rx.recv_timeout(timeout) {
      Ok(ConnectTo(ip)) => {
        info!("connecting...");
        let http_connection = ws::http::WashingMachineHttpConnection::new(ip);
        emit_update(&window, http_connection.get_connection_state());
        connection = Some(Box::new(http_connection));
        update_ts = Instant::now();
      }
      Ok(Refresh) => match connection {
        Some(ref connection) => emit_update(&window, connection.get_connection_state()),
        None => emit_update(&window, json!("null")),
      },
      Ok(UdpDiscovery) => {
        let closure_window = window.clone();
        rt.spawn(discovery::poll().then(|res| async move {
          match res {
            Ok(addresses) => {
              closure_window.emit("ipAddresses", addresses).unwrap();
            }
            Err(e) => {
              warn!("{}", e);
            }
          }
        }));
      }
      Ok(SendMachineConfiguration(name, bytes)) => {
        if let Some(ref mut connection) = connection {
          match connection.send_machine_configuration(name, &bytes[..]) {
            Ok(_) => {
              println!("Tutto okk");
            }
            Err(e) => {
              warn!("Error: {}", e);
              //TODO: send error to the ui
            }
          };
        }
      }
      Ok(GetRemoteMachineConfiguration(archive)) => {
        if let Some(ref mut connection) = connection {
          match connection.get_machine_configuration(archive) {
            Ok(bytes) => {
              window.emit("remoteMachineLoaded", bytes).ok();
            }
            Err(e) => {
              warn!("Error: {}", e);
              //TODO: send error to the ui
            }
          };
        }
      }
      Err(mpsc::RecvTimeoutError::Disconnected) => error!("Disconnected from queue!"),
      Err(mpsc::RecvTimeoutError::Timeout) => (),
    }

    if let Some(ref mut connection) = connection {
      if update_ts.elapsed() > Duration::new(2, 0) {
        connection.refresh_state();
        emit_update(&window, connection.get_connection_state());
        update_ts = Instant::now();
      }
    }
  }
}
