:- module(server,
	  [ server/0,
	    server/1				% ?Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).
:- use_module(todo).

:- set_setting_default(http:cors, [*]).

%%	server is det.
%%	server(?Port) is det.
%
%	Attach the TODO persistent store  and   start  the web-server on
%	Port.

server :-
	server(3030).
server(Port) :-
	attach_todo_db,
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]).
