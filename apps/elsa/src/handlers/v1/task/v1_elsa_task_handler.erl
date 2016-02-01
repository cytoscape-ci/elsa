
-module(v1_elsa_task_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         delete_resource/2,
         json_response/2]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {ID, Request} = cowboy_req:binding(id, Req),
  {ok, Request, {id, ID}}.

allowed_methods(Req, ID) ->
  {[<<"GET">>,<<"DELETE">>], Req, ID}.

content_types_provided(Req, ID) ->
  {[{<<"application/json">>, json_response}], Req, ID}.

resource_exists(Req, {id, ID}) ->
  case elsa_task:status(ID) of
    not_found -> {false, Req, {id, ID}};
    _ -> {true, Req, {id, ID}}
  end.

delete_resource(Req, {id, ID}) ->
  elsa_task:delete(ID),
  {true, Req, {id, ID}}.

json_response(Req, {id, ID}) ->
  {task, ID, Completed, Service, Version, Method, Endpoint, _, _, _, _} = elsa_task_database:retreive(ID),
  Task = elsa_json:to([
    {<<"task_id">>, ID},
    {<<"completed">>, Completed},
    {<<"service">>, Service},
    {<<"version">>, Version},
    {<<"method">>, Method},
    {<<"endpoint">>, Endpoint}
  ]),
  {Task, Req, {id, ID}}.
