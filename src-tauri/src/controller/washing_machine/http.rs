use super::{ConnectionState, Metadata, State, WashingMachineConnection};
use std::io::Read;
use urlencoding::encode;

pub struct WashingMachineHttpConnection {
  ip: String,
  agent: ureq::Agent,
  connection_state: ConnectionState,
}

impl WashingMachineHttpConnection {
  pub fn new(ip: String) -> Self {
    let agent: ureq::Agent = ureq::builder()
      .timeout(std::time::Duration::from_secs(4))
      .build();
    let connection_state = Self::first_connection(&ip, &agent);

    Self {
      ip,
      agent,
      connection_state,
    }
  }

  fn first_connection(ip: &String, agent: &ureq::Agent) -> ConnectionState {
    match json_request(ip, agent, "state").and_then(|state| {
      json_request(ip, agent, "machine")
        .map(|metadata| ConnectionState::Connected { state, metadata })
    }) {
      Ok(state) => state,
      Err(e) => ConnectionState::Error(e),
    }
  }

  fn json_request<R: serde::de::DeserializeOwned>(self: &Self, target: &str) -> Result<R, String> {
    json_request(&self.ip, &self.agent, target)
  }
}

impl WashingMachineConnection for WashingMachineHttpConnection {
  fn refresh_state(self: &mut Self) {
    use ConnectionState::*;
    self.connection_state = match self.connection_state.clone() {
      Error(_) => Self::first_connection(&self.ip, &self.agent),
      Connected { state: _, metadata } => match self.json_request::<State>("state") {
        Ok(state) => Connected { state, metadata },
        Err(e) => Error(e),
      },
    };
  }

  fn refresh_configuration_archive(self: &mut Self) {
    use ConnectionState::*;
    self.connection_state = match self.connection_state.clone() {
      Error(_) => Self::first_connection(&self.ip, &self.agent),
      Connected { state, metadata: _ } => match self.json_request::<Metadata>("machine") {
        Ok(metadata) => Connected { state, metadata },
        Err(e) => Error(e),
      },
    };
  }

  fn send_machine_configuration(self: &Self, archive: String, data: &[u8]) -> Result<(), String> {
    self
      .agent
      .post(format!("http://{}/machine/{}", &self.ip, encode(archive.as_str())).as_str())
      .send_bytes(data)
      .map(|_| ())
      .map_err(|e| err2str(e))
  }

  fn get_machine_configuration(self: &Self, archive: String) -> Result<Vec<u8>, String> {
    let resp = self
      .agent
      .get(format!("http://{}/machine/{}", &self.ip, encode(archive.as_str())).as_str())
      .call()
      .map_err(|e| err2str(e))?;

    if let Some(header) = resp.header("Content-Length") {
      let len = header.parse().map_err(|e| err2str(e))?;
      let mut bytes: Vec<u8> = Vec::with_capacity(len);
      resp
        .into_reader()
        .take(10_000_000)
        .read_to_end(&mut bytes)
        .map_err(|e| err2str(e))?;
      Ok(bytes)
    } else {
      Err("No content length".into())
    }
  }

  fn get_connection_state(self: &Self) -> ConnectionState {
    self.connection_state.clone()
  }
}

fn json_request<R: serde::de::DeserializeOwned>(
  ip: &String,
  agent: &ureq::Agent,
  target: &str,
) -> Result<R, String> {
  let json_response = agent
    .get(format!("http://{}/{}", ip, target).as_str())
    .call()
    .map_err(|e| err2str(e))?
    .into_json::<serde_json::Value>()
    .map_err(|e| err2str(e))?;
  serde_json::from_value::<R>(json_response).map_err(|e| String::from(format!("{:?}", e)))
}

fn err2str(err: impl std::fmt::Debug) -> String {
  String::from(format!("{:?}", err))
}
