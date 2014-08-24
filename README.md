# SWI-Prolog implementation of the todo-backend API

These files implement  [Todo-Backend,  a   shared  example  to  showcase
backend  tech  stacks](http://todo-backend.thepete.net/index.html)    in
[SWI-Prolog](http://www.swi-prolog.org) version 7.   The  implementation
uses the Prolog database with file backup to provide persistency.

Roadmap:

  - `run.pl` allows for loading and starting the server.
  - `daemon.pl` As `run.pl`, but supports Unix service interaction.
    Edit the `#!` line and run `./daemon.pl --help`.
  - `server.pl` provides the webserver intialization.
  - `todo.pl` is the real server.  It is extensively documented
    to help Prolog novices understand the code.

## Notes

  - The  implementation  relies  heavily  on  SWI-Prolog's  web  server
    infrastructure. I have also used SWI-Prolog version 7 extensions
    that are intended to make Prolog code look _less alien_ by providing
    dicts and strings.

  - Requires SWI-Prolog >= 7.1.21
