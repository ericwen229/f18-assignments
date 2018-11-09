extern crate rand;

use session::*;
use std::collections::{HashSet, HashMap};

pub struct Syn;
pub struct SynAck;
pub struct Ack;
pub struct Fin;

pub type TCPHandshake<TCPRecv> = ();

pub type TCPRecv<TCPClose> = ();

pub type TCPClose = ();

pub type TCPServer = TCPHandshake<TCPRecv<TCPClose>>;

pub type TCPClient = <TCPServer as HasDual>::Dual;

pub fn tcp_server(c: Chan<(), TCPServer>) -> Vec<Buffer> {
  unimplemented!();
}

pub fn tcp_client(c: Chan<(), TCPClient>, bufs: Vec<Buffer>) {
  unimplemented!();
}

#[cfg(test)]
mod test {
  use session::*;
  use session::NOISY;
  use std::sync::atomic::Ordering;
  use rand;
  use rand::Rng;
  use tcp::*;
  use std::marker::PhantomData;
  use std::sync::mpsc::channel;
  use std::thread;

  #[test]
  fn test_basic() {
    let mut bufs: Vec<Buffer> = Vec::new();
    let mut rng = rand::thread_rng();
    for i in 0usize..20 {
      let buf: Buffer = vec![0; rng.gen_range(1, 10)];
      let buf: Buffer = buf.into_iter().map(|x: u8| rng.gen()).collect();
      bufs.push(buf);
    }

    let (s, c): ((Chan<(), TCPServer>), (Chan<(), TCPClient>)) = Chan::new();
    let thread = thread::spawn(move || { tcp_client(c, bufs); });

    tcp_server(s);

    let res = thread.join();
  }

  #[test]
  fn test_noisy() {
    let mut bufs: Vec<Buffer> = Vec::new();
    let mut rng = rand::thread_rng();
    for i in 0usize..20 {
      let buf: Buffer = vec![0; rng.gen_range(1, 10)];
      let buf: Buffer = buf.into_iter().map(|x: u8| rng.gen()).collect();
      bufs.push(buf);
    }

    NOISY.with(|noisy| {
      noisy.store(true, Ordering::SeqCst);
    });

    let (s, c): ((Chan<(), TCPServer>), (Chan<(), TCPClient>)) = Chan::new();
    let thread = thread::spawn(move || { tcp_client(c, bufs); });

    tcp_server(s);

    let res = thread.join();
  }
}