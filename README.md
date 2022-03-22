# ZeroDNS daemon
![CI](https://github.com/shamazmazum/zero-dns/workflows/CI/badge.svg)

This is ZeroDNS daemon which provides DNS functionality on top of ZeroMQ library
and NORM multicast protocol. Tested on FreeBSD only, but may work on Linux as
well.

## How to use it?

Quick start:

* Install ZeroMQ.
* Clone this repository into `local-projects`.
* Run `(ql:quickload :zero-dns)` in REPL.
* Run `(asdf:make :zero-dns/executable)`. This step will build an executable
  file in `local-projects/zero-dns` directory.
* Allow incoming and outgoing UDP traffic to `239.192.20.1:5354` destination
  address.
* DNS queries will be accepted on `/var/run/zero-dns/<iface_name>` UNIX domain
  socket by default where `<iface_name>` is the name of that interface ZeroDNS is
  running on. Make sure that `/var/run/zero-dns/` exists and is
  accessible before starting `zero-dns`.
* Run `zero-dns -d` supplying the interface which is connected to your LAN,
  e.g. `zero-dns -d re0`. The process will daemonize itself and can be killed
  with `SIGTERM` or `SIGINT`.

An example of name resolution query using `py-zmq`:

~~~~
>>> import zmq
>>> ctx = zmq.Context()
>>> socket = ctx.socket (zmq.REQ)
>>> socket.connect ('ipc:///var/run/zero-dns/<iface_name>')
>>> socket.send_string ('!vonbraun.local')
>>> socket.recv()
b'192.168.20.10'
>>> socket.send_string ('~192.168.20.10')
>>> socket.recv()
b'vonbraun.local'
~~~~

`!` is used for forward DNS lookup and `~` for reverse DNS lookup. Currently,
only IPv4 is supported.

`zero-dns` executable supports the following options:

* `-a, --address ADDRESS` Multicast address and port for Zero DNS
  messages. Default value: `239.192.20.1:5354`.
* `-n, --network NETWORK` Working network. Can be a network in CIDR notation or
  `any`. If the working network is specified ZeroDNS will do I/O on the
  interface only if the interface has an IP address belonging to that
  network. If this option is not specified or the working network is `any`
  ZeroDNS will always send/receive DNS messages on the interface.
* `-i, --sending-interval SECONDS` Interval between two ZeroDNS messages in
  seconds. Default value: `30`.
* `-t, --time-to-live SECONDS` Time before ZeroDNS entry expiration in
  seconds. Default: `120`.
* `-q, --socket-directory DIRECTORY` Path to a directory where UNIX domain
  sockets for DNS queries will be created. Default value: `/var/run/zero-dns/`.
* `-d, --daemonize` Run as a daemon.
* `-h, --help` Print help and exit.

## Startup script for FreeBSD

If you are FreeBSD user, you can place `etc/rc.d/zerodns` script to
`/usr/local/etc/rc.d/` and `etc/devd/zerodns.conf` to `/etc/devd/`.

ZeroDNS daemon will be started as soon as some Ethernet or Wi-Fi interface is
up. When this interface goes down, ZeroDNS daemon will kill itself.

Do not forget to install ZeroDNS binary to `/usr/local/bin` and add `zerodns`
user.

## How to use it system-wide?

To allow all programs use ZeroDNS install [ZeroDNS NSS
module](https://github.com/shamazmazum/nss-zero-dns).

## Bugs & Limitations

* Has support only for IPv4.
* Can be full of undetected bugs.
* SBCL only.
* Tested on FreeBSD, but must work on other UNIX-like systems such as Linux as
  well.
