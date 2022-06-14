use std::str;
use std::time::Duration;
use tokio;
use tokio::{io, net::UdpSocket};

const BROADCAST_ADDRESS: &str = "255.255.255.255:4040";

pub async fn poll() -> Result<Vec<(String, String)>, io::Error> {
  let socket: UdpSocket = UdpSocket::bind("0.0.0.0:0").await?;
  socket.set_broadcast(true)?;

  let mut results: Vec<(String, String)> = vec![];

  let call: Vec<u8> = "WS2020_ROTONDI_DISCOVERY".as_bytes().to_vec();
  match socket.send_to(&call, BROADCAST_ADDRESS).await {
    Ok(n) => {
      if n != call.len() {
        return Err(io::Error::new(
          io::ErrorKind::Other,
          "Sent the wrong number of bytes",
        ));
      } else {
        // Do nothing because we sent the number of bytes we expected to send
      }
    }
    Err(e) => return Err(e),
  }

  let mut recv_buff: [u8; 32] = [0; 32];
  while let Ok(Ok((n, _))) =
    tokio::time::timeout(Duration::new(2, 0), socket.recv_from(&mut recv_buff)).await
  {
    if let Ok(message) = str::from_utf8(&recv_buff) {
      let split = message[0..n].split("|");
      let pieces: Vec<&str> = split.collect();

      if let (Some(&id), Some(&eth), Some(&wifi), Some(&node)) =
        (pieces.get(0), pieces.get(1), pieces.get(2), pieces.get(3))
      {
        if id.eq("WS2020") {
          if eth.len() > 0 {
            results.push((eth.into(), node.into()));
          }
          if wifi.len() > 0 {
            results.push((wifi.into(), node.into()));
          }
        }
      }
    }
  }

  Ok(results)
}
