use super::discovery;
use super::prefs;
use super::prefs::AppPreferences;
use serde_json;
use std::sync::mpsc;
use std::time::{Duration, Instant};
use tauri::Window;
use futures::future::FutureExt;
use tokio::runtime::Runtime;

#[derive(Clone, serde::Serialize)]
enum FrontEndConnectionState {
  Disconnected,
  Connected(serde_json::Value),
  Error(String),
}

#[derive(Clone, serde::Serialize)]
enum BackEndConnectionState {
  Disconnected,
  ConnectedToLocal(String, serde_json::Value),
  Error(String),
}

impl From<BackEndConnectionState> for FrontEndConnectionState {
  fn from(be: BackEndConnectionState) -> Self {
    match be {
      BackEndConnectionState::Disconnected => FrontEndConnectionState::Disconnected,
      BackEndConnectionState::ConnectedToLocal(_, state) => {
        FrontEndConnectionState::Connected(state)
      }
      BackEndConnectionState::Error(err) => FrontEndConnectionState::Error(err),
    }
  }
}

enum Message {
  ConnectTo(String),
  Disconnect,
  Refresh,
  UdpDiscovery,
}

pub fn task(window: Window) {
  fn emit_update(window: &Window, state: FrontEndConnectionState) {
    println!("Updating state: {}", serde_json::to_string(&state).unwrap());
    window.emit("stateUpdate", state).unwrap();
  }

  fn update_state(agent: &ureq::Agent, ip: &str) -> BackEndConnectionState {
    match get_state(&agent, ip) {
      Ok(value) => BackEndConnectionState::ConnectedToLocal(ip.into(), value),
      Err(error) => BackEndConnectionState::Error(error),
    }
  }

  let rt = Runtime::new().expect("Failed to build pool");

  if let Some(preferences) = prefs::get_user_preferences() {
    println!(
      "Saved preferences: {} {}",
      preferences.language, preferences.machine
    );
    window.emit("savedPreferences", preferences).ok();
  }

  let timeout = Duration::from_millis(100);
  let (tx, rx) = mpsc::channel::<Message>();
  let mut state = BackEndConnectionState::Disconnected;
  let agent: ureq::Agent = ureq::builder()
    .timeout(std::time::Duration::from_secs(4))
    .build();

  window.listen("washingMachineHttpConnect", move |event| {
    println!("{:?}", event);
  });

  let tx_clone = tx.clone();
  window.listen("navigateTo", move |_event| {
    tx_clone.send(Message::Refresh).ok();
  });

  let tx_clone = tx.clone();
  window.listen("searchMachines", move |_event| {
    println!("Searching for machines...");
    tx_clone.send(Message::UdpDiscovery).ok();
  });

  let tx_clone = tx.clone();
  window.listen("washingMachineHttpConnect", move |event| {
    if let Some(str) = event.payload() {
      tx_clone.send(Message::ConnectTo(String::from(str))).ok();
    }
  });

  window.listen("preferences", move |event| {
    println!("Saving user preferences: {:?}", event);
    if let Some(prefs) = event
      .payload()
      .and_then(|json| serde_json::from_str::<AppPreferences>(json).ok())
    {
      prefs::set_usage_preferences(prefs.language, prefs.machine);
    } else {
      panic!("Invalid JSON passed throught port!");
    }
  });

  emit_update(&window, state.clone().into());

  println!("Starting backend loop");

  let mut update_ts = Instant::now();

  loop {
    use Message::*;
    match rx.recv_timeout(timeout) {
      Ok(ConnectTo(ip)) => {
        println!("connecting...");
        state = update_state(&agent, ip.as_str());
        emit_update(&window, state.clone().into());
        update_ts = Instant::now();
      }
      Ok(Disconnect) => {}
      Ok(Refresh) => {
        emit_update(&window, state.clone().into());
      }
      Ok(UdpDiscovery) => {
        let closure_window = window.clone();
        rt.spawn(discovery::poll().then(|res| async move {
          match res {
            Ok(addresses) => {
              closure_window.emit("ipAddresses", addresses).unwrap();
            }
            Err(e) => {
              println!("{}", e);
            }
          }
        }));
      }
      Err(mpsc::RecvTimeoutError::Disconnected) => println!("Disconnected from queue!"),
      Err(mpsc::RecvTimeoutError::Timeout) => (),
    }

    match &state {
      BackEndConnectionState::ConnectedToLocal(ip, _) => {
        if update_ts.elapsed() > Duration::new(2, 0) {
          state = update_state(&agent, ip.as_str());
          emit_update(&window, state.clone().into());
          update_ts = Instant::now();
        }
      }
      _ => (),
    };
  }
}

fn get_state(agent: &ureq::Agent, ip: &str) -> Result<serde_json::Value, String> {
  agent
    .get(format!("http://{}/state", ip).as_str())
    .call()
    .map_err(|e| String::from(format!("{:?}", e)))?
    .into_json::<serde_json::Value>()
    .map_err(|e| String::from(format!("{:?}", e)))
}
