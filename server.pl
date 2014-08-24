:- module(server,
	  [ server/0,
	    server/1				% ?Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).
:- use_module(library(broadcast)).

:- use_module(todo).

:- set_setting_default(http:cors, [*]).

%%	server is det.
%%	server(?Port) is det.
%
%	Attach the TODO persistent store  and   start  the web-server on
%	Port.
%
%	@see daemon.pl to start this file as a (Unix) service

server :-
	server(3030).
server(Port) :-
	broadcast(http(pre_server_start)),
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]).

:- listen(http(pre_server_start), attach_todo_db(data)).
