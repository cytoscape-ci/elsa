
-module(elsa_task_handler).

-export([init/3,
         handle/2,
         terminate/3
         ]).

init({tcp, http}, Req, State) ->
  lager:info("Received task request: ~p", [Req]),
  {ok, Req, State}.

handle(Req, _State) ->
  {ID, Request} = cowboy_req:binding(id, Req),
  process(Request, ID).

terminate(_Reason, _Req, _State) ->
  ok.

process(Req, ID) ->
  validate(Req, ID, elsa_task:status(ID)).

validate(Req, ID, complete) ->
  lager:info("Client requested completed task: ~p", [ID]),
  send(Req, ID);
validate(Req, ID, incomplete) ->
  lager:info("Client requested task that is not yet complete: ~p", [ID]),
  {ok, elsa_handler:reply(204, <<"Task not yet completed. Please try again later.">>, Req), ID};
validate(Req, ID, not_found) ->
  lager:error("Client requested task that does not exist: ~p", [ID]),
  {ok, elsa_handler:reply(404, <<"Task not found.">>, Req), ID}.

send(Req, ID) ->
  {Headers, Response} = elsa_task:data(ID),
  {ok, elsa_handler:reply(200, Headers, Response, Req), ID}.
