use preferences::{AppInfo, Preferences, PreferencesMap};
use serde::{Deserialize, Serialize};
use log::warn;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct AppPreferences {
  pub language: String,
  pub machine: String,
}

const APP_INFO: AppInfo = AppInfo {
  name: "preferences",
  author: "HSW",
};
const LANGUAGE_PREF: &str = "language";
const MACHINE_PREF: &str = "machine";
const PREFERENCES_KEY: &str = "laundry-control-preferences";

pub fn set_usage_preferences(language: String, machine: String) {
  // Create a new preferences key-value map
  // (Under the hood: HashMap<String, String>)
  let mut faves: PreferencesMap<String> = PreferencesMap::new();

  // Edit the preferences (std::collections::HashMap)
  faves.insert(LANGUAGE_PREF.into(), language);
  faves.insert(MACHINE_PREF.into(), machine);

  // Store the user's preferences
  let save_result = faves.save(&APP_INFO, PREFERENCES_KEY);
  assert!(save_result.is_ok());
}

pub fn get_user_preferences() -> Option<AppPreferences> {
  match PreferencesMap::<String>::load(&APP_INFO, PREFERENCES_KEY) {
    Ok(map) => {
      if let (Some(language), Some(machine)) = (map.get(LANGUAGE_PREF), map.get(MACHINE_PREF)) {
        Some(AppPreferences {
          language: language.clone(),
          machine: machine.clone(),
        })
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
