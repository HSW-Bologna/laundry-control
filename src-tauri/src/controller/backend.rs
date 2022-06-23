use super::discovery;
use super::prefs;
use super::things5_api;
use super::washing_machine as ws;
use super::washing_machine::WashingMachineConnection;
use super::Error;
use futures::future::FutureExt;
use log::{debug, info, warn};
use serde_json;
use serde_json::json;
use std::sync::mpsc;
use std::time::{Duration, Instant};
use tauri::Window;
use tokio::runtime::Runtime;

#[derive(Clone, serde::Deserialize)]
enum BackEndPortMessage {
  Refresh,
  Things5Login { username: String, password: String },
  WashingMachineHttpConnect(String),
  WashingMachineThings5Connect { token: String, device_id: String },
  SearchMachines,
  SendCurrentMachineConfiguration(Vec<u8>),
  GetCurrentMachineConfiguration,
  SelectMachineConfiguration(String),
  StartProgram(u16),
  Restart,
  Pause,
  Stop,
  ClearAlarms,
}

pub fn task(window: Window) {
  fn snackbar_message(window: &Window, message: &str) {
    window.emit("notificationMessage", message).unwrap();
  }

  fn report_to_ui<S, E>(window: &Window, res: Result<S, E>) {
    match res {
      Ok(_) => snackbar_message(window, "Successo"),
      Err(_) => snackbar_message(window, "Fallimento"),
    }
  }

  fn emit_update(window: &Window, state: impl serde::Serialize) {
    debug!("update {}", serde_json::ser::to_string(&state).unwrap());
    window.emit("stateUpdate", state).unwrap();
  }

  fn send_state(connection: &Option<Box<dyn ws::WashingMachineConnection>>, window: &Window) {
    match connection {
      Some(ref connection) => emit_update(window, connection.get_connection_state()),
      None => emit_update(&window, json!("null")),
    };
  }

  let rt = Runtime::new().expect("Failed to build pool");
  let mut connection: Option<Box<dyn ws::WashingMachineConnection>> = None;

  let timeout = Duration::from_millis(100);
  let (tx, rx) = mpsc::channel::<BackEndPortMessage>();

  let tx_clone = tx.clone();
  window.listen("backendPort", move |event| {
    if let Some(str) = event.payload() {
      match serde_json::from_str::<BackEndPortMessage>(str) {
        Ok(message) => {
          tx_clone.send(message).ok();
        }
        Err(e) => panic!("Error while parsing json from port: {}\n--> {:?}", str, e),
      };
    } else {
      panic!("Event without payload!");
    }
  });

  if let Some(token) = prefs::get_token() {
    match things5_api::get_devices(&token) {
      Ok(devices) => {
        window.emit("things5Login", token.clone()).unwrap();
        window.emit("things5Devices", devices).unwrap();
      }
      Err(_) => (),
    }
  }

  info!("Starting backend loop");
  let mut update_ts = Instant::now();
  let mut quick_update_ts: Option<Instant> = None;

  loop {
    use BackEndPortMessage::*;
    match rx.recv_timeout(timeout) {
      Ok(WashingMachineHttpConnect(ip)) => {
        info!("connecting...");
        let http_connection = ws::local::Connection::new(ip);
        match http_connection.get_connection_state() {
          ws::ConnectionState::Connected {
            name: _,
            active: _,
            state: _,
            configuration: _,
            stats: _,
          } => {
            snackbar_message(&window, "Connesso");
            connection = Some(Box::new(http_connection));
            send_state(&connection, &window);
            update_ts = Instant::now();
          }
          ws::ConnectionState::Error => snackbar_message(&window, "ConnessioneFallita"),
        }
      }

      Ok(WashingMachineThings5Connect { token, device_id }) => {
        info!("connecting to things5");
        let things5_connection = ws::things5::Connection::new(token, device_id);
        match things5_connection.get_connection_state() {
          ws::ConnectionState::Connected {
            name: _,
            active: _,
            state: _,
            configuration: _,
            stats: _,
          } => {
            snackbar_message(&window, "Connesso");
            connection = Some(Box::new(things5_connection));
            send_state(&connection, &window);
            update_ts = Instant::now();
          }
          ws::ConnectionState::Error => snackbar_message(&window, "ConnessioneFallita"),
        }
      }

      Ok(Things5Login { username, password }) => {
        info!("Login attempt");
        match things5_api::authorize(username.as_str(), password.as_str()) {
          Ok(token) => {
            info!("Login successful!");
            window.emit("things5Login", token.clone()).unwrap();
            if let Ok(devices) = things5_api::get_devices(token.as_str()) {
              window.emit("things5Devices", devices).unwrap();
              prefs::set_token(token);
            }
          }
          Err(Error::Network) | Err(Error::Protocol) => snackbar_message(&window, "ErroreDiRete"),
          Err(Error::Value) => snackbar_message(&window, "CredenzialiNonValide"),
        }
      }

      Ok(Refresh) => send_state(&connection, &window),

      Ok(SearchMachines) => {
        info!("Searching for machines...");
        let closure_window = window.clone();
        rt.spawn(discovery::poll().then(|res| async move {
          match res {
            Ok(addresses) => {
              info!("Found {:?}", addresses);
              closure_window.emit("ipAddresses", addresses).unwrap();
            }
            Err(e) => {
              warn!("{}", e);
            }
          }
        }));
      }

      Ok(SendCurrentMachineConfiguration(bytes)) => {
        report_to_ui(
          &window,
          connection
            .as_ref()
            .unwrap()
            .send_machine_configuration(bytes.into()),
        );
        quick_update_ts = Some(Instant::now());
      }

      Ok(GetCurrentMachineConfiguration) => {
        match connection.as_ref().unwrap().get_machine_configuration() {
          Ok(bytes) => {
            window.emit("remoteMachineLoaded", bytes).ok();
            snackbar_message(&window, "Successo");
          }
          Err(_) => snackbar_message(&window, "Fallimento"),
        };
      }

      Ok(SelectMachineConfiguration(archive)) => {
        match connection
          .as_ref()
          .unwrap()
          .select_machine_configuration(archive)
        {
          Ok(()) => {
            snackbar_message(&window, "Successo");
            quick_update_ts = Some(Instant::now());
          }
          Err(_) => snackbar_message(&window, "Fallimento"),
        };
      }

      Ok(StartProgram(program)) => {
        if let Some(ref mut unwrapped_connection) = connection {
          unwrapped_connection.start_program(program).ok();
          quick_update_ts = Some(Instant::now());
        }
      }

      Ok(Restart) => {
        connection.as_deref_mut().unwrap().restart().ok();
        quick_update_ts = Some(Instant::now());
      }

      Ok(Pause) => {
        connection.as_deref_mut().unwrap().pause().ok();
        quick_update_ts = Some(Instant::now());
      }

      Ok(Stop) => {
        connection.as_deref_mut().unwrap().stop().ok();
        quick_update_ts = Some(Instant::now());
      }

      Ok(ClearAlarms) => {
        connection.as_deref_mut().unwrap().clear_alarms().ok();
        quick_update_ts = Some(Instant::now());
      }

      Err(mpsc::RecvTimeoutError::Disconnected) => panic!("Disconnected from queue!"),
      Err(mpsc::RecvTimeoutError::Timeout) => (),
    }

    if let Some(ref mut unwrapped_connection) = connection {
      if let Some(ts) = quick_update_ts {
        if ts.elapsed() > Duration::from_millis(300) {
          unwrapped_connection.refresh_data();
          send_state(&connection, &window);
          update_ts = Instant::now();
          quick_update_ts = None;
        }
      } else if update_ts.elapsed() > Duration::new(1, 0) {
        unwrapped_connection.refresh_data();
        send_state(&connection, &window);
        update_ts = Instant::now();
      }
    }
  }
}
