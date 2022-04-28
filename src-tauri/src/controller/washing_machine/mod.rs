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

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Statistics {
  cycles: u16,
  interrupted_cycles: u16,
  loop_cycles: u16,
  on_time: u32,
  work_time: u32,
  rotation_time: u32,
  heating_time: u32,
  cold_water_time: u32,
  warm_water_time: u32,
  recovery_water_time: u32,
  flux_water_time: u32,
  porthole_closings: u32,
  porthole_openings: u32,
  soap_times: Vec<u32>,
}

#[derive(Clone, serde::Serialize)]
pub enum ConnectionState {
  Connected {
    state: State,
    configuration: Configuration,
    stats: Statistics,
  },
  Error,
}

pub trait WashingMachineConnection {
  fn refresh_data(self: &mut Self);
  fn refresh_configuration_archive(self: &mut Self);
  fn send_machine_configuration(self: &Self, archive: String, data: Vec<u8>) -> Result<()>;
  fn get_machine_configuration(self: &Self, archive: String) -> Result<Vec<u8>>;
  fn select_machine_configuration(self: &Self, archive: String) -> Result<()>;
  fn get_connection_state(self: &Self) -> ConnectionState;
  fn start_program(self: &Self, program: u16) -> Result<()>;
  fn restart(self: &Self) -> Result<()>;
  fn pause(self: &Self) -> Result<()>;
  fn stop(self: &Self) -> Result<()>;
  fn clear_alarms(self: &Self) -> Result<()>;
}
