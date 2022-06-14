use super::Error;
use reqwest;
use reqwest::blocking::{Client, ClientBuilder};
use serde::{Deserialize, Serialize};
use serde_json;
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
pub struct Device {
  pub id: String,
  pub name: String,
}

pub fn get_devices(token: String) -> Result<Vec<Device>, Error> {
  let agent: Client = ClientBuilder::new()
    .timeout(std::time::Duration::from_secs(4))
    .build()
    .unwrap();

  agent
    .get("https://api.things5.digital/v1/devices")
    .header("Accept", "application/json")
    .header("authorization", format!("Bearer {}", token))
    .send()
    .map_err(|_| Error::Network)
    .and_then(|response| {
      if response.status().is_success() {
        let content = response.text().map_err(|_| Error::Protocol)?;
        let value: serde_json::Value =
          serde_json::from_str(content.as_str()).map_err(|_| Error::Protocol)?;
        if let Some(data) = value.get("data") {
          let devices: Vec<Device> =
            serde_json::from_value(data.clone()).map_err(|_| Error::Protocol)?;
          Ok(devices)
        } else {
          Err(Error::Protocol)
        }
      } else {
        Err(Error::Value)
      }
    })
}

pub fn authorize(user: String, password: String) -> Result<String, Error> {
  let agent: Client = ClientBuilder::new()
    .timeout(std::time::Duration::from_secs(4))
    .build()
    .unwrap();

  let mut params = HashMap::new();
  params.insert("client_id", "api");
  params.insert("grant_type", "password");
  params.insert("scope", "openid");
  params.insert("username", user.as_str());
  params.insert("password", password.as_str());

  agent
    .post("https://auth.things5.digital/auth/realms/hswsnc/protocol/openid-connect/token")
    .form(&params)
    .send()
    .map_err(|_| Error::Network)
    .and_then(|response| {
      log::debug!("Things5 auth resp {:?}", response);
      if response.status().is_success() {
        let content = response.text().map_err(|_| Error::Protocol)?;
        let value: serde_json::Value =
          serde_json::from_str(content.as_str()).map_err(|_| Error::Protocol)?;
        if let Some(token) = value.get("access_token").and_then(|t| t.as_str()) {
          Ok(String::from(token))
        } else {
          Err(Error::Value)
        }
      } else {
        Err(Error::Value)
      }
    })
}

pub fn ping_device(token: String, device_id: String) -> Result<(), Error> {
  let agent: Client = ClientBuilder::new()
    .timeout(std::time::Duration::from_secs(4))
    .build()
    .unwrap();

  agent
    .get(
      format!(
        "https://api.things5.digital/v1/devices/{}/parameters",
        device_id
      )
      .as_str(),
    )
    .header("Accept", "application/json")
    .header("authorization", format!("Bearer {}", token))
    .send()
    .map_err(|_| Error::Network)
    .and_then(|response| {
      if response.status().is_success() {
        let content = response.text().map_err(|_| Error::Protocol)?;
        log::info!("{}", content);
        Ok(())
      } else {
        Err(Error::Value)
      }
    })
}
