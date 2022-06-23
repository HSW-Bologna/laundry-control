pub mod backend;
mod discovery;
mod prefs;
mod things5_api;
mod washing_machine;

#[derive(Debug)]
pub enum Error {
  Network,
  Protocol,
  Value,
}
