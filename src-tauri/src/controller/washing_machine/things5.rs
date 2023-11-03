use super::super::things5_api;
use super::{ConnectionState, Statistics, WashingMachineConnection};
use super::{Error, Result as WSResult};
use std::time::{Duration, Instant};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct StatisticsPair {
  total: Statistics,
}

pub struct Connection {
  token: String,
  device_id: String,
  connection_state: ConnectionState,
  last_complete_update: Instant,
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
      last_complete_update: Instant::now(),
    }
  }

  fn first_connection(token: String, device_id: String) -> ConnectionState {
    let get_state = || -> Result<ConnectionState, Error> {
      let (name, active) = things5_api::get_device_details(token.as_str(), device_id.as_str())?;
      let (state, stats) =
        things5_api::get_state_and_statistics(token.as_str(), device_id.as_str())?;
      let configuration =
        things5_api::get_preview_configuration(token.as_str(), device_id.as_str())?;
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
  fn suggested_refresh_period(self : &Self) -> Duration {
    Duration::from_secs(5)
  }

  fn refresh_data(self: &mut Self) {
    if Instant::now() - self.last_complete_update > Duration::from_secs(120) {
      self.connection_state =
        match things5_api::refresh_data_ingestion(self.token.as_str(), self.device_id.as_str()) {
          Ok(()) => Self::first_connection(self.token.clone(), self.device_id.clone()),
          Err(e) => {
            log::warn!("Could not refresh data ingestion: {:?}", e);
            ConnectionState::Error
          }
        };
      self.last_complete_update = Instant::now();
    } else {
      match self.connection_state.clone() {
        ConnectionState::Connected {
          active: _,
          name: _,
          state: _,
          stats: _,
          configuration,
        } => {
          let get_state = || -> Result<ConnectionState, Error> {
            let (name, active) =
              things5_api::get_device_details(self.token.as_str(), self.device_id.as_str())?;
            let (state, stats) =
              things5_api::get_state_and_statistics(self.token.as_str(), self.device_id.as_str())?;
            Ok(ConnectionState::Connected {
              active,
              name,
              state,
              stats,
              configuration,
            })
          };

          self.connection_state = get_state().unwrap_or(ConnectionState::Error);
        }
        ConnectionState::Error => {
          self.connection_state =
            Self::first_connection(self.token.clone(), self.device_id.clone());
        }
      }
    }
  }

  fn send_machine_configuration(self: &Self, data: Vec<u8>) -> WSResult<()> {
    things5_api::put_current_machine(self.token.as_str(), self.device_id.as_str(), data)
  }

  fn select_machine_configuration(self: &Self, _archive: String) -> WSResult<()> {
    // permanent stub
    Ok(())
  }

  fn get_machine_configuration(self: &Self) -> WSResult<Vec<u8>> {
    things5_api::get_current_machine(self.token.as_str(), self.device_id.as_str())
  }

  fn get_connection_state(self: &Self) -> ConnectionState {
    self.connection_state.clone()
  }

  fn restart(self: &Self) -> WSResult<()> {
    things5_api::restart(self.token.as_str(), self.device_id.as_str())
  }

  fn pause(self: &Self) -> WSResult<()> {
    things5_api::pause(self.token.as_str(), self.device_id.as_str())
  }

  fn stop(self: &Self) -> WSResult<()> {
    things5_api::stop(self.token.as_str(), self.device_id.as_str())
  }

  fn start_program(self: &Self, program: u16) -> WSResult<()> {
    things5_api::start(self.token.as_str(), self.device_id.as_str(), program)
  }

  fn clear_alarms(self: &Self) -> WSResult<()> {
    things5_api::clear_alarms(self.token.as_str(), self.device_id.as_str())
  }
}
