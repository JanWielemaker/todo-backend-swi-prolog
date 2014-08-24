:- module(todo,
	  [ attach_todo_db/0
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_path)).
:- use_module(library(option)).
:- use_module(library(uuid)).
:- use_module(library(persistency)).

:- http_handler(root(todo), todo, [prefix]).

/** <module> SWI-Prolog implementation of the todo-backend API

Implemetation of "Todo-Backend, a  shared   example  to showcase backend
tech stacks."

@see http://todo-backend.thepete.net/index.html
*/

%%	todo(+Request)
%
%	HTTP handler for the =todo=backend=   API. First clause supports
%	the _PreFlight_ =OPTIONS= request. The  next clause extracts the
%	method, and query information and calls todo/2.

todo(Request) :-
	option(method(options), Request), !,
	cors_enable(Request,
		    [ methods([get,post,patch,delete])
		    ]),
	format('~n').				% empty body
todo(Request) :-
	read_query(Request, Query),
	cors_enable,
	option(method(Method), Request),
	todo(Method, Query).

%%	todo(+Method, +Query)
%
%	Implements the 4 real methods of the TODO API.

todo(get, Query) :- !,
	get_todos(Query.id, List),
	reply_json_dict(List).
todo(post, Query) :- !,
	create_todo(Query, TODO),
	reply_json_dict(TODO).
todo(delete, Query) :- !,
	get_todos(Query.id, List),
	retractall_todo(Query.id, _, _, _),
	reply_json_dict(List).
todo(patch, Query) :- !,
	get_todo(Query.id, Old),
	New = Old.put(Query),
	retractall_todo(Query.id, _, _, _),
	create_todo(New, TODO),
	reply_json_dict(TODO).

%%	read_query(+Request, -Query) is det.
%
%	Read the query information we need from the HTTP requst.  There
%	are two pieces of information:
%
%	  - If there is content, it is JSON.  http_read_json_dict/2
%	    will raise an error if there is something wrong.
%	  - The TODO =id= may be appended to the path.  In that
%	    case there is path_info(Text) in the Request.  We need
%	    to strip the leading '/'.
%
%	Note that if there is no  =id=,   we  return a dict with unbound
%	=id=. This avoids switching in =GET=   and  =DELETE= between the
%	case with and without an =id=.

read_query(Request, Dict) :-
	option(content_length(Len), Request), Len > 0, !,
	http_read_json_dict(Request, Dict0),
	(   request_path_id(Request, ID)
	->  Dict = Dict0.put(id,ID)
	;   Dict = Dict0
	).
read_query(Request, _{id:ID}) :-
	request_path_id(Request, ID), !.
read_query(_, empty{id:_}).

request_path_id(Request, ID) :-
	option(path_info(SlashID), Request),
	atom_concat(/, ID, SlashID).


		 /*******************************
		 *	      DATA		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We use the Prolog database for storing   the TODOs, but using persistent
backup as provided by library(persistency).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	create_todo(+Dict, -CreatedTODO) is det.
%
%	Create a new TODO from info in Dict.  %We ignore all data in the
%	dict, except for,  =title=,  =completed=   and  =order=  fields.
%	Missing fields are left unbound, so we   need  to bind them to a
%	default value. Finally, we return the created TODO.

create_todo(Dict, TODO) :-
	uuid(Id),
	Dict >:< _{title:Title, completed:Completed, order:Order},
	value(Title,     ""),
	value(Completed, false),
	value(Order,     0),
	assert_todo(Id, Title, Completed, Order),
	get_todo(Id, TODO).

value(Val,  Default) :- var(Val), !, Val = Default.
value(_, _).

%%	get_todos(?Id, -Result) is det.
%
%	If Id is bound, return the  matching   TODO.  Else return a list
%	holding all TODOs. This allows for  a single reply predicate for
%	all operations that return TODOs.

get_todos(Id, TODO) :-
	nonvar(Id), !,
	get_todo(Id, TODO).
get_todos(Id, List) :-
	findall(Todo, get_todo(Id, Todo), List).

%%	get_todo(?Id, -TODO) is nondet.
%
%	True when TODO is the TODO with id=ID. Note that we do not store
%	the URL in our  database,  but   add  it  dynamically here. This
%	allows moving our server  to  a   new  location  using  the same
%	database.

get_todo(Id, _{id:Id, url:URL,
	       title:Title, completed:Completed, order:Order}) :-
	http_absolute_uri(root(todo), TODOURL),
	todo(Id, Title, Completed, Order),
	directory_file_path(TODOURL, Id, URL).

% declare our persistent database and export a predicate to attach it

:- persistent
	todo(id:atom,
	     title:string,
	     completed:boolean,
	     order:integer).

attach_todo_db :-
	db_attach('todo.db', []).
