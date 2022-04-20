use super::{Configuration, ConnectionState, State, WashingMachineConnection};
use reqwest;
use reqwest::blocking::{Client, ClientBuilder};
use urlencoding::encode;

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
    match json_get(ip, agent, "state").and_then(|state| {
      json_get(ip, agent, "machine").map(|configuration| ConnectionState::Connected {
        state,
        configuration,
      })
    }) {
      Ok(state) => state,
      Err(e) => ConnectionState::Error(e),
    }
  }

  fn json_get<R: serde::de::DeserializeOwned>(self: &Self, target: &str) -> Result<R, String> {
    json_get(&self.ip, &self.agent, target)
  }

  fn post(self: &Self, target: &str) -> Result<(), String> {
    self
      .agent
      .post(format!("http://{}/{}", self.ip, target).as_str())
      .header("Connection", "close")
      .send()
      .map_err(|e| err2str(e))
      .map(|_| ())
  }

  fn post_json<T: serde::Serialize>(self: &Self, target: &str, data: &T) -> Result<(), String> {
    self
      .agent
      .post(format!("http://{}/{}", self.ip, target).as_str())
      .header("Connection", "close")
      .json(data)
      .send()
      .map_err(|e| err2str(e))
      .map(|_| ())
  }
}

impl WashingMachineConnection for WashingMachineHttpConnection {
  fn refresh_state(self: &mut Self) {
    use ConnectionState::*;
    self.connection_state = match self.connection_state.clone() {
      Error(_) => Self::first_connection(&self.ip, &self.agent),
      Connected {
        state: _,
        configuration,
      } => match self.json_get::<State>("state") {
        Ok(state) => Connected {
          state,
          configuration,
        },
        Err(e) => Error(e),
      },
    };
  }

  fn refresh_configuration_archive(self: &mut Self) {
    use ConnectionState::*;
    self.connection_state = match self.connection_state.clone() {
      Error(_) => Self::first_connection(&self.ip, &self.agent),
      Connected {
        state,
        configuration: _,
      } => match self.json_get::<Configuration>("machine") {
        Ok(configuration) => Connected {
          state,
          configuration,
        },
        Err(e) => Error(e),
      },
    };
  }

  fn send_machine_configuration(self: &Self, archive: String, data: Vec<u8>) -> Result<(), String> {
    self
      .agent
      .post(format!("http://{}/machine/{}", &self.ip, encode(archive.as_str())).as_str())
      .body(data)
      .send()
      .map(|_| ())
      .map_err(|e| err2str(e))
  }

  fn select_machine_configuration(self: &Self, archive: String) -> Result<(), String> {
    self.post(format!("select_machine/{}", encode(archive.as_str())).as_str())
  }

  fn get_machine_configuration(self: &Self, archive: String) -> Result<Vec<u8>, String> {
    let mut resp = self
      .agent
      .get(format!("http://{}/machine/{}", &self.ip, encode(archive.as_str())).as_str())
      .send()
      .map_err(|e| err2str(e))?;

    if let Some(header) = resp.headers().get("Content-Length") {
      let len = header.len();
      let mut bytes: Vec<u8> = Vec::with_capacity(len);
      resp.copy_to(&mut bytes).map_err(|e| err2str(e))?;
      Ok(bytes)
    } else {
      Err("No content length".into())
    }
  }

  fn get_connection_state(self: &Self) -> ConnectionState {
    self.connection_state.clone()
  }

  fn stop(self: &Self) -> Result<(), String> {
    self.post("stop")
  }

  fn start_program(self: &Self, program: u16) -> Result<(), String> {
    self.post_json(
      "start",
      &serde_json::json!({"cycle" : program}),
    )
  }
}

fn json_get<R: serde::de::DeserializeOwned>(
  ip: &String,
  agent: &Client,
  target: &str,
) -> Result<R, String> {
  let json_response = agent
    .get(format!("http://{}/{}", ip, target).as_str())
    .send()
    .map_err(|e| err2str(e))?
    .json::<serde_json::Value>()
    .map_err(|e| err2str(e))?;
  serde_json::from_value::<R>(json_response).map_err(|e| String::from(format!("{:?}", e)))
}

fn err2str(err: impl std::fmt::Debug) -> String {
  String::from(format!("{:?}", err))
}
