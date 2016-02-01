
-module(v1_elsa_tasks_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         json_response/2]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, json_response}], Req, State}.

json_response(Req, State) ->
  Tasks = elsa_json:to([[
    {<<"task_id">>, ID},
    {<<"completed">>, Completed},
    {<<"service">>, Service},
    {<<"version">>, Version},
    {<<"method">>, Method},
    {<<"endpoint">>, Endpoint}
  ] || {ID, Completed, Service, Version, Method, Endpoint} <- elsa_task:all()]),
  {Tasks, Req, State}.
