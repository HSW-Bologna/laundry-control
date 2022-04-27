use serde;
pub mod http;

pub enum Error {
  Network,
  Protocol,
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

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
  cycle: u16,
  step_code: u16,
  step_number: u16,
  cycle_remaining: u16,
  step_remaining: u16,
  step_count: u16,
  temperature: u16,
  level: u16,
  speed: u16,
}

#[derive(Clone, serde::Serialize)]
pub enum ConnectionState {
  Connected {
    state: State,
    configuration: Configuration,
  },
  Error,
}

pub trait WashingMachineConnection {
  fn refresh_state(self: &mut Self);
  fn refresh_configuration_archive(self: &mut Self);
  fn send_machine_configuration(self: &Self, archive: String, data: Vec<u8>) -> Result<()>;
  fn get_machine_configuration(self: &Self, archive: String) -> Result<Vec<u8>>;
  fn select_machine_configuration(self: &Self, archive: String) -> Result<()>;
  fn get_connection_state(self: &Self) -> ConnectionState;
  fn start_program(self: &Self, program: u16) -> Result<()>;
  fn restart(self: &Self) -> Result<()>;
  fn pause(self: &Self) -> Result<()>;
  fn stop(self: &Self) -> Result<()>;
}
