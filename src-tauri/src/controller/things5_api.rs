use super::washing_machine::{
  Configuration, ProgramPreview, State as WashingMachineState, Statistics,
};
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
struct State {
  #[serde(rename = "id")]
  _id: String,
  #[serde(rename = "end_time")]
  _end_time: String,
  #[serde(rename = "metadata")]
  _metadata: Option<String>,
  name: String,
  #[serde(rename = "start_time")]
  _start_time: String,
  value: String,
}

#[derive(Deserialize)]
struct ParameterVariable {
  name: String,
  value: String,
  #[serde(rename = "type")]
  _type: String,
}

#[derive(Deserialize)]
struct Variables {
  name: String,
  variables: Vec<ParameterVariable>,
}

#[derive(Deserialize)]
struct WriteResponse {
  assets: Vec<Variables>,
  #[serde(default)]
  errors: Vec<String>,
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
  .map(|_| ())
}

pub fn get_current_machine(token: &str, device_id: &str) -> Result<Vec<u8>, Error> {
  let value: serde_json::Value = serde_json::json!({
      "assets" : [
          {
              "name" :"configuration",
              "values" : [
                  {
                      "name" : "machine",
                      "type" : "integer",
                      "value": "",
                  }
              ]
          }
      ]
  });

  let response = post_request(
    format!(
      "https://api.things5.digital/v1/devices/{}/parameters",
      device_id
    )
    .as_str(),
    token,
    value,
  )?;

  let json_response: WriteResponse =
    serde_json::from_str(response.as_str()).map_err(|e| Error::Json(e.to_string()))?;

  for asset in json_response.assets {
    if asset.name == "configuration" {
      for variable in asset.variables {
        if variable.name == "machine" {
          return Ok(base64::decode(variable.value).unwrap());
        }
      }
    }
  }

  Err(Error::Protocol)
}

pub fn put_current_machine(token: &str, device_id: &str, machine: Vec<u8>) -> Result<(), Error> {
  let encoded = base64::encode(machine);

  let value: serde_json::Value = serde_json::json!({
      "assets" : [
          {
              "name" :"configuration",
              "values" : [
                  {
                      "name" : "machine",
                      "type" : "integer",
                      "value": encoded
                  }
              ]
          }
      ]
  });

  let response = post_request(
    format!(
      "https://api.things5.digital/v1/devices/{}/parameters",
      device_id
    )
    .as_str(),
    token,
    value,
  )?;

  let json_response: WriteResponse =
    serde_json::from_str(response.as_str()).map_err(|e| Error::Json(e.to_string()))?;

  if json_response.errors.len() > 0 {
    Err(Error::Server(json_response.errors.get(0).unwrap().clone()))
  } else {
    Ok(())
  }
}

pub fn get_preview_configuration(token: &str, device_id: &str) -> Result<Configuration, Error> {
  fn get_value<'a>(
    key: &str,
    value: &'a serde_json::Value,
  ) -> Result<&'a serde_json::Value, Error> {
    value.get(key).ok_or(Error::Protocol)
  }

  fn get_string<'a>(key: &str, value: &'a serde_json::Value) -> Result<&'a str, Error> {
    value
      .get(key)
      .ok_or(Error::Protocol)?
      .as_str()
      .ok_or(Error::Protocol)
  }

  let response = &get_request(
    format!(
      "https://api.things5.digital/v1/devices/{}/parameters",
      device_id
    )
    .as_str(),
    token,
  )?;

  let value = get_named_item(
    &serde_json::from_value::<Vec<serde_json::Value>>(get_value("assets", &response)?.clone())
      .map_err(|_| Error::Protocol)?,
    "configuration",
  )
  .ok_or(Error::Protocol)?;

  let variables = serde_json::from_value::<Vec<serde_json::Value>>(
    value.get("variables").ok_or(Error::Protocol)?.clone(),
  )
  .map_err(|_| Error::Protocol)?;

  let mut config: Configuration = Configuration::default();
  for variable in variables {
    let name = get_string("name", &variable)?;

    match name {
      "name" => config.name = String::from(get_string("value", &variable)?),
      "app_version" => config.app_version = String::from(get_string("value", &variable)?),
      "machine_version" => config.app_version = String::from(get_string("value", &variable)?),
      "programs" => {
        let programs = get_string("value", &variable)?
          .split(",")
          .collect::<Vec<&str>>();
        for i in 0..programs.len() / 2 {
          let name = programs.get(i * 2).unwrap();
          let wash_type = programs.get(i * 2 + 1).unwrap().parse::<u16>().unwrap_or(0);
          config.programs.push(ProgramPreview {
            name: String::from(*name),
            wash_type,
          });
        }
      }
      _ => (),
    }
  }

  Ok(config)
}

pub fn get_state_and_statistics(
  token: &str,
  device_id: &str,
) -> Result<(WashingMachineState, Statistics), Error> {
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

  let states: Vec<State> = serde_json::from_value(
    get_request(url.as_str(), token)?
      .get("data")
      .ok_or(Error::Protocol)?
      .clone(),
  )
  .map_err(|_| Error::Protocol)?;

  let mut statistics = Statistics::default();
  let mut state = WashingMachineState::default();
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
    .timeout(std::time::Duration::from_secs(8))
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
    .map_err(|e| Error::Network(e.to_string()))
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

pub fn start(token: &str, device_id: &str, program: u16) -> Result<(), Error> {
  send_command(
    token,
    device_id,
    "start",
    String::from(format!("{}", program)).as_str(),
  )
}

pub fn restart(token: &str, device_id: &str) -> Result<(), Error> {
  send_command(token, device_id, "start", "")
}

pub fn pause(token: &str, device_id: &str) -> Result<(), Error> {
  send_command(token, device_id, "pause", "")
}

pub fn stop(token: &str, device_id: &str) -> Result<(), Error> {
  send_command(token, device_id, "stop", "")
}

pub fn clear_alarms(token: &str, device_id: &str) -> Result<(), Error> {
  send_command(token, device_id, "clear_alarms", "")
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
    .map_err(|e| Error::Network(e.to_string()))
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

fn post_request(url: &str, token: &str, value: serde_json::Value) -> Result<String, Error> {
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
    .map_err(|e| Error::Network(e.to_string()))
    .and_then(|response| {
      if response.status().is_success() {
        Ok(response.text().unwrap())
      } else {
        log::warn!("Error response for {}: {}", url, response.text().unwrap());
        Err(Error::Value)
      }
    })
}

fn get_named_item(items: &Vec<serde_json::Value>, name: &str) -> Option<serde_json::Value> {
  for item in items {
    let value = item.get("name")?.as_str()?;
    if value == name {
      return Some(item.clone());
    }
  }

  None
}

fn send_command(token: &str, device_id: &str, command: &str, value: &str) -> Result<(), Error> {
  let value: serde_json::Value = serde_json::json!({
      "assets" : [
          {
              "name" :"configuration",
              "values" : [
                  {
                      "name" : command,
                      "type" : "string",
                      "value": value,
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
  .map(|_| ())
}
