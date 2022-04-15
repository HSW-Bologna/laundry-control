use serde;
pub mod http;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Metadata {
  app_version: String,
  machines: Vec<String>,
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct State {
  alarm_code: u16,
  credit: u16,
  name: String,
  porthole_open: bool,
  state: u16,
  step_code: u16,
}

#[derive(Clone, serde::Serialize)]
pub enum ConnectionState {
  Connected { state: State, metadata: Metadata },
  Error(String),
}

pub trait WashingMachineConnection {
  fn refresh_state(self: &mut Self);
  fn refresh_configuration_archive(self: &mut Self);
  fn send_machine_configuration(self: &Self, archive: String, data: &[u8]) -> Result<(), String>;
  fn get_machine_configuration(self: &Self, archive: String) -> Result<Vec<u8>, String>;
  fn get_connection_state(self: &Self) -> ConnectionState;
}
