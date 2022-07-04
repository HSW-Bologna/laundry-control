mod discovery;
mod prefs;
mod things5_api;
mod washing_machine;

use futures::future::FutureExt;
use serde_json;
use serde_json::json;
use std::sync::mpsc;
use std::time::{Duration, Instant};
use tauri::Window;
use washing_machine as ws;
use washing_machine::WashingMachineConnection;

#[derive(Debug)]
pub enum Error {
  Network(String),
  Json(String),
  Protocol,
  Server(String),
  Value,
}

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

pub struct Context {
  window: Window,
}

pub struct Controller {
  context: Context,
}

impl Controller {
  pub fn new(window: Window) -> Self {
    log::info!("New controller");
    Controller {
      context: Context { window },
    }
  }

  fn window(self: &Self) -> Window {
    self.context.window.clone()
  }

  fn emit(self: &Self, topic: &str, message: impl serde::Serialize) {
    self.window().emit(topic, message).unwrap();
  }

  fn snackbar_message(self: &Self, message: &str) {
    self.emit("notificationMessage", message);
  }

  fn emit_update(self: &Self, state: impl serde::Serialize) {
    log::debug!("update {}", serde_json::ser::to_string(&state).unwrap());
    self.emit("stateUpdate", state);
  }

  fn send_state(self: &Self, connection: &Option<Box<dyn ws::WashingMachineConnection>>) {
    match connection {
      Some(ref connection) => self.emit_update(connection.get_connection_state()),
      None => self.emit_update(json!("null")),
    };
  }
}

pub fn task(window: Window) {
  let rt = tokio::runtime::Runtime::new().expect("Failed to build pool");
  let mut connection: Option<Box<dyn ws::WashingMachineConnection>> = None;
  let controller = Controller::new(window);

  let timeout = Duration::from_millis(100);
  let (tx, rx) = mpsc::channel::<BackEndPortMessage>();

  let tx_clone = tx.clone();
  controller.window().listen("backendPort", move |event| {
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
        controller
          .window()
          .emit("things5Login", token.clone())
          .unwrap();
        controller.window().emit("things5Devices", devices).unwrap();
      }
      Err(_) => (),
    }
  }

  log::info!("Starting backend loop");
  let mut update_ts = Instant::now();
  let mut quick_update_ts: Option<Instant> = None;

  loop {
    use BackEndPortMessage::*;
    match rx.recv_timeout(timeout) {
      Ok(WashingMachineHttpConnect(ip)) => {
        log::info!("connecting...");
        let http_connection = ws::local::Connection::new(ip);
        match http_connection.get_connection_state() {
          ws::ConnectionState::Connected {
            name: _,
            active: _,
            state: _,
            configuration: _,
            stats: _,
          } => {
            controller.snackbar_message("Connesso");
            connection = Some(Box::new(http_connection));
            controller.send_state(&connection);
            update_ts = Instant::now();
          }
          ws::ConnectionState::Error => controller.snackbar_message("ConnessioneFallita"),
        }
      }

      Ok(WashingMachineThings5Connect { token, device_id }) => {
        log::info!("connecting to things5");
        let things5_connection = ws::things5::Connection::new(token, device_id);
        match things5_connection.get_connection_state() {
          ws::ConnectionState::Connected {
            name: _,
            active: _,
            state: _,
            configuration: _,
            stats: _,
          } => {
            controller.snackbar_message("Connesso");
            connection = Some(Box::new(things5_connection));
            controller.send_state(&connection);
            update_ts = Instant::now();
          }
          ws::ConnectionState::Error => controller.snackbar_message("ConnessioneFallita"),
        }
      }

      Ok(Things5Login { username, password }) => {
        log::info!("Login attempt");
        match things5_api::authorize(username.as_str(), password.as_str()) {
          Ok(token) => {
            log::info!("Login successful!");
            controller
              .window()
              .emit("things5Login", token.clone())
              .unwrap();
            if let Ok(devices) = things5_api::get_devices(token.as_str()) {
              controller.window().emit("things5Devices", devices).unwrap();
              prefs::set_token(token);
            }
          }
          Err(Error::Value) => controller.snackbar_message("CredenzialiNonValide"),
          _ => controller.snackbar_message("ErroreDiRete"),
        }
      }

      Ok(Refresh) => controller.send_state(&connection),

      Ok(SearchMachines) => {
        log::info!("Searching for machines...");
        let closure_window = controller.window();
        rt.spawn(discovery::poll().then(|res| async move {
          match res {
            Ok(addresses) => {
              log::info!("Found {:?}", addresses);
              closure_window.emit("ipAddresses", addresses).unwrap();
            }
            Err(e) => {
              log::warn!("{}", e);
            }
          }
        }));
      }

      Ok(SendCurrentMachineConfiguration(bytes)) => {
        match connection
          .as_ref()
          .unwrap()
          .send_machine_configuration(bytes.into())
        {
          Ok(()) => controller.snackbar_message("ConfigurazioneCaricata"),
          Err(e) => {
            log::error!("Unable to put machine config: {:?}", e);
            controller.snackbar_message("NonSonoRiuscitoACaricareLaConfigurazione");
          }
        }
        quick_update_ts = Some(Instant::now());
      }

      Ok(GetCurrentMachineConfiguration) => {
        match connection.as_ref().unwrap().get_machine_configuration() {
          Ok(bytes) => {
            controller.window().emit("remoteMachineLoaded", bytes).ok();
            controller.snackbar_message("ConfigurazioneScaricata");
          }
          Err(e) => {
            log::error!("Unable to get machine config: {:?}", e);
            controller.snackbar_message("NonSonoRiuscitoAScaricareLaConfigurazione");
          }
        };
      }

      Ok(SelectMachineConfiguration(archive)) => {
        match connection
          .as_ref()
          .unwrap()
          .select_machine_configuration(archive)
        {
          Ok(()) => {
            controller.snackbar_message("Successo");
            quick_update_ts = Some(Instant::now());
          }
          Err(_) => controller.snackbar_message("Fallimento"),
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
          controller.send_state(&connection);
          update_ts = Instant::now();
          quick_update_ts = None;
        }
      } else if update_ts.elapsed() > Duration::new(1, 0) {
        unwrapped_connection.refresh_data();
        controller.send_state(&connection);
        update_ts = Instant::now();
      }
    }
  }
}
