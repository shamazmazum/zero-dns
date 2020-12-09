# ZeroDNS daemon

This is ZeroDNS daemon which provides DNS functionality on top of ZeroMQ library
and NORM multicast protocol. Tested on FreeBSD only, but may work on Linux as
well.

## How to use it?

* Install ZeroMQ.
* Clone this repository into `local-projects`.
* Run `(ql:quickload :zero-dns)` in REPL.
* Run `(asdf:make :zero-dns/executable)`. This step will build an executable
  file in `local-projects/zero-dns` directory.
* Allow incoming and outgoing UDP traffic to `239.192.20.1:5354` destination
  address.
* DNS queries will be accepted on `localhost:5355`.
* Run `zero-dns` supplying the interface which is connected to your LAN,
  e.g. `zero-dns re0`. The process will daemonize itself and can be killed with
  `SIGTERM` or `SIGINT`.

An example of name resolution query using `py-zmq`:

~~~~
>>> import zmq
>>> ctx = zmq.Context()
>>> socket = ctx.socket (zmq.REQ)
>>> socket.connect ('tcp://localhost:5355')
>>> socket.send_string ('!vonbraun.local')
>>> socket.recv()
b'192.168.20.10'
>>> socket.send_string ('~192.168.20.10')
>>> socket.recv()
b'vonbraun.local'
~~~~

`!` is used for forward DNS lookup and `~` for reverse DNS lookup. Currently,
only IPv4 is supported.

## How to use it system-wide?

To allow all programs use ZeroDNS install [ZeroDNS NSS
module](https://github.com/shamazmazum/nss-zero-dns).
