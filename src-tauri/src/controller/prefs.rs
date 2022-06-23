use log::warn;
use preferences::{AppInfo, Preferences, PreferencesMap};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct AppPreferences {
  pub language: String,
  pub token: String,
}

const APP_INFO: AppInfo = AppInfo {
  name: "preferences",
  author: "HSW",
};
const TOKEN_PREF: &str = "token";
const PREFERENCES_KEY: &str = "laundry-control-preferences";

pub fn set_token(token: String) {
  // Create a new preferences key-value map
  // (Under the hood: HashMap<String, String>)
  let mut faves: PreferencesMap<String> = PreferencesMap::new();

  // Edit the preferences (std::collections::HashMap)
  faves.insert(TOKEN_PREF.into(), token);

  // Store the user's preferences
  let save_result = faves.save(&APP_INFO, PREFERENCES_KEY);
  assert!(save_result.is_ok());
}

pub fn get_token() -> Option<String> {
  match PreferencesMap::<String>::load(&APP_INFO, PREFERENCES_KEY) {
    Ok(map) => {
      if let Some(token) = map.get(TOKEN_PREF) {
        Some(String::from(token))
      } else {
        None
      }
    }
    Err(e) => {
      warn!("Error while loading preferences: {:?}", e);
      None
    }
  }
}
