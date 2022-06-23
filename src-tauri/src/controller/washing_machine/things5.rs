use super::super::things5_api;
use super::{Configuration, ConnectionState, Statistics, WashingMachineConnection};
use super::{Error, Result as WSResult};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct StatisticsPair {
  total: Statistics,
}

pub struct Connection {
  token: String,
  device_id: String,
  connection_state: ConnectionState,
}

impl Connection {
  pub fn new(token: String, device_id: String) -> Self {
    let connection_state =
      match things5_api::refresh_data_ingestion(token.as_str(), device_id.as_str()) {
        Ok(()) => Self::first_connection(token.clone(), device_id.clone()),
        Err(e) => {
          log::warn!("Could not refresh data ingestion: {:?}", e);
          ConnectionState::Error
        }
      };

    Self {
      token,
      device_id,
      connection_state,
    }
  }

  fn first_connection(token: String, device_id: String) -> ConnectionState {
    let get_state = || -> Result<ConnectionState, Error> {
      let (name, active) = things5_api::get_device_details(token.as_str(), device_id.as_str())?;
      let (state, stats) =
        things5_api::get_state_and_statistics(token.as_str(), device_id.as_str())?;
      let configuration = Configuration::default(); //TODO:
      Ok(ConnectionState::Connected {
        active,
        name,
        state,
        stats,
        configuration,
      })
    };
    get_state().unwrap_or(ConnectionState::Error)
  }
}

impl WashingMachineConnection for Connection {
  fn refresh_data(self: &mut Self) {
    self.connection_state = Self::first_connection(self.token.clone(), self.device_id.clone());
  }

  fn send_machine_configuration(self: &Self, _data: Vec<u8>) -> WSResult<()> {
    Ok(())
  }

  fn select_machine_configuration(self: &Self, _archive: String) -> WSResult<()> {
    Ok(())
  }

  fn get_machine_configuration(self: &Self) -> WSResult<Vec<u8>> {
    Ok(Vec::new())
  }

  fn get_connection_state(self: &Self) -> ConnectionState {
    self.connection_state.clone()
  }

  fn restart(self: &Self) -> WSResult<()> {
    Ok(())
  }

  fn pause(self: &Self) -> WSResult<()> {
    Ok(())
  }

  fn stop(self: &Self) -> WSResult<()> {
    Ok(())
  }

  fn start_program(self: &Self, _program: u16) -> WSResult<()> {
    Ok(())
  }

  fn clear_alarms(self: &Self) -> WSResult<()> {
    Ok(())
  }
}
