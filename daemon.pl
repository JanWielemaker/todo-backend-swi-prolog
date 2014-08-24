#!/usr/bin/swipl

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ClioPatria is prepared to be combined with the SWI-Prolog library
library(http/http_unix_daemon).  ClioPatria is started as a deamon
using

  % ./daemon.pl --port=Port [option ...]

Options are briefly described by running the daemon with --help:

  % ./daemon.pl --help

See library(http/http_unix_daemon) for details.  The SWI-Prolog
distribution contains a file debian-init-script, which can be
used as a skeleton for managing the server from /etc/init.d.  To
do so, follow these steps:

  1. copy debian-init-script to /etc/init.d/<server>.  This
     script is in the directory doc/packages/examples/http of
     your Prolog installation.
  2. Edit /etc/init.d/<server>, setting DIR to the working
     directory of the ClioPatria server.
  3. Edit the remainder of the configuration section to suit
     your needs.
  4. Run
       % update-rc.d <server> defaults
       % /etc/init.d/<server> start

Permissions

Ideally, the ClioPatria directory is owned by another user than the user
used by the daemon. If the user of   the  daemon has no write access, it
acts as a read-only service. Write access   may be given by changing the
group of certain files to the  group   of  the daemon and allowing group
write.  You may want to add write access to

  - The RDF-store directory to allow adding persistent triples (otherwise
    tiples may be added, but will not persist).
  - The file settings.db to allow changing settings through the API.
  - The file users.db to allow modifying the user database throuh th API
  - The '.' directory to allow creating httpd.log.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- set_prolog_flag(verbose, silent).
:- use_module(library(http/http_unix_daemon)).

:- use_module(server, []).

:- initialization http_daemon.
