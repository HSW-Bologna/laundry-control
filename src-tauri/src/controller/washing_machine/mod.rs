use serde;
pub mod http;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct ProgramPreview {
  name: String,
  wash_type: u16,
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Configuration {
  name: String,
  app_version: String,
  machines: Vec<String>,
  programs: Vec<ProgramPreview>,
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
  Connected {
    state: State,
    configuration: Configuration,
  },
  Error(String),
}

pub trait WashingMachineConnection {
  fn refresh_state(self: &mut Self);
  fn refresh_configuration_archive(self: &mut Self);
  fn send_machine_configuration(self: &Self, archive: String, data: Vec<u8>) -> Result<(), String>;
  fn get_machine_configuration(self: &Self, archive: String) -> Result<Vec<u8>, String>;
  fn select_machine_configuration(self: &Self, archive: String) -> Result<(), String>;
  fn get_connection_state(self: &Self) -> ConnectionState;
  fn start_program(self: &Self, program: u16) -> Result<(), String>;
  fn stop(self: &Self) -> Result<(), String>;
}
