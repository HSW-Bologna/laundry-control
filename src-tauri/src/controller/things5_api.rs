use super::washing_machine::{State, Statistics};
use super::Error;
use reqwest;
use reqwest::blocking::{Client, ClientBuilder};
use serde::{Deserialize, Serialize};
use serde_json;
use std::collections::HashMap;
use std::time::Duration;

#[derive(Serialize, Deserialize)]
pub struct Device {
  pub id: String,
  pub name: String,
}

#[derive(Deserialize)]
struct Things5State {
  id: String,
  end_time: String,
  metadata: Option<String>,
  name: String,
  start_time: String,
  value: String,
}

pub fn refresh_data_ingestion(token: &str, device_id: &str) -> Result<(), Error> {
  let value: serde_json::Value = serde_json::json!({
      "assets" : [
          {
              "name" :"configuration",
              "values" : [
                  {
                      "name" : "active",
                      "type" : "integer",
                      "value": "1",
                  }
              ]
          }
      ]
  });

  post_request(
    format!(
      "https://api.things5.digital/v1/devices/{}/parameters",
      device_id
    )
    .as_str(),
    token,
    value,
  )
}

pub fn get_state_and_statistics(
  token: &str,
  device_id: &str,
) -> Result<(State, Statistics), Error> {
  let from =
    chrono::offset::Utc::now() - chrono::Duration::from_std(Duration::from_secs(1)).unwrap();
  let to = chrono::offset::Utc::now();
  let from = format!("{}", from.format("%Y-%m-%dT%H:%M:%SZ"));
  let to = format!("{}", to.format("%Y-%m-%dT%H:%M:%SZ"));

  let url = reqwest::Url::parse_with_params(
    format!(
      "https://api.things5.digital/v1/devices/{}/states",
      device_id
    )
    .as_str(),
    &[("from", from), ("to", to)],
  )
  .unwrap();

  let states: Vec<Things5State> = serde_json::from_value(
    get_request(url.as_str(), token)?
      .get("data")
      .ok_or(Error::Protocol)?
      .clone(),
  )
  .map_err(|_| Error::Protocol)?;

  let mut statistics = Statistics::default();
  let mut state = State::default();
  for s in states {
    match s.name.as_str() {
      "cycles" => statistics.cycles = s.value.parse().unwrap_or(0),
      "interrupted_cycles" => statistics.interrupted_cycles = s.value.parse().unwrap_or(0),
      "loop_cycles" => statistics.loop_cycles = s.value.parse().unwrap_or(0),
      "on_time" => statistics.on_time = s.value.parse().unwrap_or(0),
      "work_time" => statistics.work_time = s.value.parse().unwrap_or(0),
      "rotation_time" => statistics.rotation_time = s.value.parse().unwrap_or(0),
      "heating_time" => statistics.heating_time = s.value.parse().unwrap_or(0),
      "cold_water_time" => statistics.cold_water_time = s.value.parse().unwrap_or(0),
      "warm_water_time" => statistics.warm_water_time = s.value.parse().unwrap_or(0),
      "recovery_water_time" => statistics.recovery_water_time = s.value.parse().unwrap_or(0),
      "flux_water_time" => statistics.flux_water_time = s.value.parse().unwrap_or(0),
      "porthole_closings" => statistics.porthole_closings = s.value.parse().unwrap_or(0),
      "porthole_openings" => statistics.porthole_openings = s.value.parse().unwrap_or(0),
      "alarm_code" => state.alarm_code = s.value.parse().unwrap_or(0),
      "credit" => state.credit = s.value.parse().unwrap_or(0),
      "name" => state.name = s.value.clone(),
      "porthole_open" => state.porthole_open = s.value.parse().unwrap_or(false),
      "state" => state.state = s.value.parse().unwrap_or(0),
      "cycle" => state.cycle = s.value.parse().unwrap_or(0),
      "step_code" => state.step_code = s.value.parse().unwrap_or(0),
      "step_number" => state.step_number = s.value.parse().unwrap_or(0),
      "cycle_remaining" => state.cycle_remaining = s.value.parse().unwrap_or(0),
      "step_count" => state.step_count = s.value.parse().unwrap_or(0),
      "temperature" => state.temperature = s.value.parse().unwrap_or(0),
      "level" => state.level = s.value.parse().unwrap_or(0),
      "speed" => state.speed = s.value.parse().unwrap_or(0),
      _ => (),
    }
  }

  Ok((state, statistics))
}

pub fn get_device_details(token: &str, device_id: &str) -> Result<(String, bool), Error> {
  let value: serde_json::Value = get_request(
    format!("https://api.things5.digital/v1/devices/{}", device_id).as_str(),
    token,
  )?;
  if let (Some(connected), Some(name)) = (
    value
      .get("data")
      .and_then(|d| d.get("is_connected"))
      .and_then(|c| c.as_bool()),
    value
      .get("data")
      .and_then(|d| d.get("name"))
      .and_then(|n| n.as_str()),
  ) {
    Ok((String::from(name), connected))
  } else {
    Err(Error::Protocol)
  }
}

pub fn get_devices(token: &str) -> Result<Vec<Device>, Error> {
  let value: serde_json::Value = get_request("https://api.things5.digital/v1/devices", token)?;
  if let Some(data) = value.get("data") {
    let devices: Vec<Device> = serde_json::from_value(data.clone()).map_err(|_| Error::Protocol)?;
    Ok(devices)
  } else {
    Err(Error::Protocol)
  }
}

pub fn authorize(user: &str, password: &str) -> Result<String, Error> {
  let agent: Client = ClientBuilder::new()
    .timeout(std::time::Duration::from_secs(4))
    .build()
    .unwrap();

  let mut params = HashMap::new();
  params.insert("client_id", "api");
  params.insert("grant_type", "password");
  params.insert("scope", "openid");
  params.insert("username", user);
  params.insert("password", password);

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

fn get_request(url: &str, token: &str) -> Result<serde_json::Value, Error> {
  let agent: Client = ClientBuilder::new()
    .timeout(std::time::Duration::from_secs(4))
    .build()
    .unwrap();

  agent
    .get(url)
    .header("Accept", "application/json")
    .header("authorization", format!("Bearer {}", token))
    .send()
    .map_err(|_| Error::Network)
    .and_then(|response| {
      if response.status().is_success() {
        let content = response.text().map_err(|_| Error::Protocol)?;
        let value: serde_json::Value =
          serde_json::from_str(content.as_str()).map_err(|_| Error::Protocol)?;

        Ok(value)
      } else {
        log::debug!("Error {} {}", url, response.text().unwrap());
        Err(Error::Value)
      }
    })
}

fn post_request(url: &str, token: &str, value: serde_json::Value) -> Result<(), Error> {
  let agent: Client = ClientBuilder::new()
    .timeout(std::time::Duration::from_secs(4))
    .build()
    .unwrap();

  agent
    .post(url)
    .header("Accept", "application/json")
    .header("authorization", format!("Bearer {}", token))
    .json(&value)
    .send()
    .map_err(|_| Error::Network)
    .and_then(|response| {
      if response.status().is_success() {
        Ok(())
      } else {
        log::warn!("Error response for {}: {}", url, response.text().unwrap());
        Err(Error::Value)
      }
    })
}
