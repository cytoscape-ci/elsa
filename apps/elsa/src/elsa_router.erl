
-module(elsa_router).

-export([start/0
         ]).

start() ->
    {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, dispatch()}]}]
    ).

dispatch() ->
  Host = '_',
  lager:info("Compiling routes."),
  cowboy_router:compile([{Host, v0_routes() ++ v1_routes() ++ kernel_route()}]).

v0_routes() ->
  [
    {"/task/:id", elsa_task_handler, []},
    {"/registration/[...]", elsa_registration_handler, []}
  ].

v1_routes() ->
  version("v1", [
    {"/task", elsa_v1_tasks_handler, []},
    {"/task/:id", v1_elsa_task_handler, []}
  ]).

kernel_route() ->
  [{"/:service/:version/[...]", elsa_kernel, []}].

version(Version, Routes) when is_list(Routes) ->
  [version(Version, Route) || Route <- Routes];
version(Version, {Route, Handler, Options}) ->
  {"/" ++ Version ++ Route, Handler, Options}.


% [
%   {Host, [
%     {"/version", elsa_v1_version_handler, [GET]}
%   ]},
%   {Host, [
%     {"/v1/task", elsa_v1_task_handler, [GET]},
%     {"/v1/task/:id", elsa_v1_task_handler, [GET]},
%     {"/v1/task/:id", elsa_v1_task_handler, [DELETE]},
%   ]},
%   {Host, [
%     {"/v1/service", elsa_v1_service_handler, [GET]},
%     {"/v1/service/:name", elsa_v1_service_handler, [GET]},
%     {"/v1/service/:name/:version", elsa_v1_service_handler, [GET]},
%     {"/v1/service", elsa_v1_service_handler, [POST]},
%     {"/v1/service", elsa_v1_service_handler, [DELETE]},
%     {"/v1/service/:name", elsa_v1_service_handler, [DELETE]},
%     {"/v1/service/:name/:version", elsa_v1_service_handler, [DELETE]}
%   ]},
%   {Host, [
%     {"/v1/agent", elsa_v1_agent_handler, [GET]},
%     {"/v1/agent/:name", elsa_v1_agent_handler, [GET]},
%     {"/v1/agent/:name", elsa_v1_agent_handler, [DELETE]},
%   ]},
%   {Host, [
%     {"/:service/:version/[...]", elsa_kernel, []}
%   ]}
% ]
