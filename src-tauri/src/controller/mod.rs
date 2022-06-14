pub mod backend;
mod discovery;
mod prefs;
mod things5;
mod washing_machine;

pub enum Error {
  Network,
  Protocol,
  Value,
}
