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
* DNS queries will be accepted on `/var/run/zero-dns/zero-dns.sock` UNIX domain
  socket by default. Make sure that `/var/run/zero-dns/` exists and is
  accessible before starting `zero-dns`.
* Run `zero-dns` supplying the interface which is connected to your LAN,
  e.g. `zero-dns re0`. The process will daemonize itself and can be killed with
  `SIGTERM` or `SIGINT`.

An example of name resolution query using `py-zmq`:

~~~~
>>> import zmq
>>> ctx = zmq.Context()
>>> socket = ctx.socket (zmq.REQ)
>>> socket.connect ('ipc:///var/run/zero-dns/zero-dns.sock')
>>> socket.send_string ('!vonbraun.local')
>>> socket.recv()
b'192.168.20.10'
>>> socket.send_string ('~192.168.20.10')
>>> socket.recv()
b'vonbraun.local'
~~~~

`!` is used for forward DNS lookup and `~` for reverse DNS lookup. Currently,
only IPv4 is supported.

## Startup script for FreeBSD

If you are FreeBSD user, you can place `zerodns` script to
`/usr/local/etc/rc.d/` and add the following lines to `/etc/rc.conf`:

~~~~
zerodns_enable="YES"
zerodns_iface="<name of your interface here>"
~~~~

ZeroDNS daemon will be started at startup and stopped on poweroff/reboot. Just
install it to `/usr/local/bin` and add `zerodns` user.

## How to use it system-wide?

To allow all programs use ZeroDNS install [ZeroDNS NSS
module](https://github.com/shamazmazum/nss-zero-dns).

## Bugs & Limitations

* Has support only for IPv4.
* Can be full of undetected bugs.
* SBCL only.
* Tested on FreeBSD, but must work on other UNIX-like systems such as Linux as
  well.
