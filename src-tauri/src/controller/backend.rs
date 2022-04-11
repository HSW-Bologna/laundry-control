use super::prefs;
use super::prefs::AppPreferences;
use serde_json;
use std::sync::mpsc;
use std::time::Duration;
use tauri::Window;

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
}

pub fn task(window: Window) {
  fn update(window: &Window, state: FrontEndConnectionState) {
    window.emit("stateUpdate", state).unwrap();
  }

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
    .timeout(std::time::Duration::from_secs(5))
    .build();

  window.listen("washingMachineHttpConnect", move |event| {
    println!("{:?}", event);
  });

  let tx_clone = tx.clone();
  window.listen("navigateTo", move |_event| {
    tx_clone.send(Message::Refresh).ok();
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

  update(&window, state.clone().into());

  println!("Starting controller loop");

  loop {
    use Message::*;
    match rx.recv_timeout(timeout) {
      Ok(ConnectTo(ip)) => {
        println!("connecting...");
        state = match get_state(&agent, ip.as_str()) {
          Ok(value) => BackEndConnectionState::ConnectedToLocal(ip, value),
          Err(error) => BackEndConnectionState::Error(error),
        };
        update(&window, state.clone().into());
      }
      Ok(Disconnect) => {}
      Ok(Refresh) => {
        update(&window, state.clone().into());
      }
      Err(mpsc::RecvTimeoutError::Disconnected) => println!("Disconnected from queue!"),
      Err(mpsc::RecvTimeoutError::Timeout) => (),
    }
  }
}

fn get_state(agent: &ureq::Agent, ip: &str) -> Result<serde_json::Value, String> {
  agent
    .get(format!("http://{}:8080/state", ip).as_str())
    .call()
    .map_err(|e| String::from(format!("{:?}", e)))?
    .into_json::<serde_json::Value>()
    .map_err(|e| String::from(format!("{:?}", e)))
}
