use super::{Configuration, ConnectionState, State, Statistics, WashingMachineConnection};
use super::{Error, Result as WSResult};
use reqwest;
use reqwest::blocking::{Client, ClientBuilder};
use urlencoding::encode;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct StatisticsPair {
  total: Statistics,
}

pub struct WashingMachineHttpConnection {
  ip: String,
  agent: Client,
  connection_state: ConnectionState,
}

impl WashingMachineHttpConnection {
  pub fn new(ip: String) -> Self {
    let agent: Client = ClientBuilder::new()
      .timeout(std::time::Duration::from_secs(4))
      .build()
      .unwrap();
    let connection_state = Self::first_connection(&ip, &agent);

    Self {
      ip,
      agent,
      connection_state,
    }
  }

  fn first_connection(ip: &String, agent: &Client) -> ConnectionState {
    match json_get(ip, agent, "state")
      .and_then(|state| json_get(ip, agent, "machine").map(|configuration| (state, configuration)))
      .and_then(|(state, configuration)| {
        json_get::<StatisticsPair>(ip, agent, "statistics").map(|stats| {
          ConnectionState::Connected {
            state,
            configuration,
            stats: stats.total,
          }
        })
      }) {
      Ok(state) => {
        log::info!("First connection successful");
        state
      }
      Err(_) => {
        log::warn!("First connection failed");
        ConnectionState::Error
      }
    }
  }

  fn json_get<R: serde::de::DeserializeOwned>(self: &Self, target: &str) -> WSResult<R> {
    json_get(&self.ip, &self.agent, target)
  }

  fn post(self: &Self, target: &str) -> reqwest::Result<()> {
    self
      .agent
      .post(format!("http://{}/{}", self.ip, target).as_str())
      .header("Connection", "close")
      .send()
      .map(|_| ())
  }

  fn post_json<T: serde::Serialize>(self: &Self, target: &str, data: &T) -> reqwest::Result<()> {
    self
      .agent
      .post(format!("http://{}/{}", self.ip, target).as_str())
      .header("Connection", "close")
      .json(data)
      .send()
      .map(|_| ())
  }
}

impl WashingMachineConnection for WashingMachineHttpConnection {
  fn refresh_data(self: &mut Self) {
    use ConnectionState::*;
    self.connection_state = match self.connection_state.clone() {
      Error => Self::first_connection(&self.ip, &self.agent),
      Connected {
        state: _,
        configuration,
        stats: _,
      } => match self.json_get::<State>("state").and_then(|state| {
        self
          .json_get::<StatisticsPair>("statistics")
          .map(|stats| (state, stats.total))
      }) {
        Ok((state, stats)) => Connected {
          state,
          configuration,
          stats,
        },
        Err(_) => Error,
      },
    };
  }

  fn refresh_configuration_archive(self: &mut Self) {
    use ConnectionState::*;
    self.connection_state = match self.connection_state.clone() {
      Error => Self::first_connection(&self.ip, &self.agent),
      Connected {
        state,
        configuration: _,
        stats,
      } => match self.json_get::<Configuration>("machine") {
        Ok(configuration) => Connected {
          state,
          configuration,
          stats,
        },
        Err(_) => Error,
      },
    };
  }

  fn send_machine_configuration(self: &Self, archive: String, data: Vec<u8>) -> WSResult<()> {
    self
      .agent
      .post(format!("http://{}/machine/{}", &self.ip, encode(archive.as_str())).as_str())
      .body(data)
      .send()
      .map_err(|_| Error::Network)
      .map(|_| ())
  }

  fn select_machine_configuration(self: &Self, archive: String) -> WSResult<()> {
    self
      .post(format!("select_machine/{}", encode(archive.as_str())).as_str())
      .map_err(|_| Error::Network)
  }

  fn get_machine_configuration(self: &Self, archive: String) -> WSResult<Vec<u8>> {
    let mut resp = self
      .agent
      .get(format!("http://{}/machine/{}", &self.ip, encode(archive.as_str())).as_str())
      .send()
      .map_err(|_| Error::Network)?;

    if let Some(header) = resp.headers().get("Content-Length") {
      let len = header.len();
      let mut bytes: Vec<u8> = Vec::with_capacity(len);
      resp.copy_to(&mut bytes).map_err(|_| Error::Protocol)?;
      log::info!("Downloaded machine config");
      Ok(bytes)
    } else {
      log::warn!("Failed to download machine config");
      Err(Error::Protocol)
    }
  }

  fn get_connection_state(self: &Self) -> ConnectionState {
    self.connection_state.clone()
  }

  fn restart(self: &Self) -> WSResult<()> {
    self.post("start").map_err(|_| Error::Network)
  }

  fn pause(self: &Self) -> WSResult<()> {
    self.post("pause").map_err(|_| Error::Network)
  }

  fn stop(self: &Self) -> WSResult<()> {
    self.post("stop").map_err(|_| Error::Network)
  }

  fn start_program(self: &Self, program: u16) -> WSResult<()> {
    self
      .post_json("start", &serde_json::json!({ "cycle": program }))
      .map_err(|_| Error::Network)
  }

  fn clear_alarms(self: &Self) -> WSResult<()> {
    self.post("clear_alarms").map_err(|_| Error::Network)
  }
}

fn json_get<R: serde::de::DeserializeOwned>(
  ip: &String,
  agent: &Client,
  target: &str,
) -> WSResult<R> {
  let json_response = match agent
    .get(format!("http://{}/{}", ip, target).as_str())
    .send()
    .and_then(|resp| resp.json::<serde_json::Value>())
  {
    Ok(json_response) => Ok(json_response),
    Err(e) => {
      log::warn!("Json GET error: {:?}", e);
      Err(Error::Network)
    }
  }?;

  match serde_json::from_value::<R>(json_response.clone()) {
    Ok(value) => Ok(value),
    Err(e) => {
      log::warn!("Invalid JSON: {:?} ({})", e, json_response.clone());
      Err(Error::Protocol)
    }
  }
}
