use serde;
pub mod local;
pub mod things5;
use super::Error;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, serde::Serialize, serde::Deserialize, Debug)]
pub struct ProgramPreview {
  name: String,
  wash_type: u16,
}

#[derive(Clone, serde::Serialize, serde::Deserialize, Default, Debug)]
pub struct Configuration {
  name: String,
  app_version: String,
  machines: Vec<String>,
  programs: Vec<ProgramPreview>,
}

#[derive(Clone, serde::Serialize, serde::Deserialize, Default, Debug)]
pub struct State {
  pub alarm_code: u16,
  pub credit: u16,
  pub name: String,
  pub porthole_open: bool,
  pub state: u16,
  pub cycle: u16,
  pub step_code: u16,
  pub step_number: u16,
  pub cycle_remaining: u16,
  pub step_remaining: u16,
  pub step_count: u16,
  pub temperature: u16,
  pub level: u16,
  pub speed: u16,
}

#[derive(Clone, serde::Serialize, serde::Deserialize, Default, Debug)]
pub struct Statistics {
  pub cycles: u16,
  pub interrupted_cycles: u16,
  pub loop_cycles: u16,
  pub on_time: u32,
  pub work_time: u32,
  pub rotation_time: u32,
  pub heating_time: u32,
  pub cold_water_time: u32,
  pub warm_water_time: u32,
  pub recovery_water_time: u32,
  pub flux_water_time: u32,
  pub porthole_closings: u32,
  pub porthole_openings: u32,
  pub soap_times: Vec<u32>,
}

#[derive(Clone, serde::Serialize, Debug)]
pub enum ConnectionState {
  Connected {
    active: bool,
    name: String,
    state: State,
    configuration: Configuration,
    stats: Statistics,
  },
  Error,
}

pub trait WashingMachineConnection {
  fn refresh_data(self: &mut Self);
  fn send_machine_configuration(self: &Self, data: Vec<u8>) -> Result<()>;
  fn get_machine_configuration(self: &Self) -> Result<Vec<u8>>;
  fn select_machine_configuration(self: &Self, archive: String) -> Result<()>;
  fn get_connection_state(self: &Self) -> ConnectionState;
  fn start_program(self: &Self, program: u16) -> Result<()>;
  fn restart(self: &Self) -> Result<()>;
  fn pause(self: &Self) -> Result<()>;
  fn stop(self: &Self) -> Result<()>;
  fn clear_alarms(self: &Self) -> Result<()>;
}
