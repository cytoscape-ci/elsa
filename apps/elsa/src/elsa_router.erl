
-module(elsa_router).

-export([start/0
         ]).

start() ->
    {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, dispatch()}]}]
    ).

dispatch() ->
  Host = '_',
  Routes = routes(),
  lager:info("Compiling routes: ~p", [routes()]),
  cowboy_router:compile([{Host, Routes}]).

routes() ->
  v0_routes() ++
  v1_routes() ++
  kernel_route().

v0_routes() ->
  [
    {"/task/:id", elsa_task_handler, []},
    {"/registration/[...]", elsa_registration_handler, []}
  ].

v1_routes() ->
  version("v1", [
    {"/task", v1_elsa_tasks_handler, []},
    {"/task/:id", v1_elsa_task_handler, []}
  ]) ++ version("v1", [
    {"/service", v1_elsa_services_handler, []},
    {"/service/:service_name", v1_elsa_service_handler, []},
    {"/service/:service_name/:version", v1_elsa_version_handler, []}
  ]).

kernel_route() ->
  [{"/:service/:version/[...]", elsa_kernel, []}].

version(Version, Routes) when is_list(Routes) ->
  [version(Version, Route) || Route <- Routes];
version(Version, {Route, Handler, Options}) ->
  {"/" ++ Version ++ Route, Handler, Options}.



%     {"/version", elsa_v1_version_handler, [GET]}
%     {"/v1/service/:name/:version", elsa_v1_service_handler, [GET]},
%     {"/v1/service/:name", elsa_v1_service_handler, [DELETE]},
%     {"/v1/service/:name/:version", elsa_v1_service_handler, [DELETE]}
%     {"/v1/agent", elsa_v1_agent_handler, [GET]},
%     {"/v1/agent/:name", elsa_v1_agent_handler, [GET]},
%     {"/v1/agent/:name", elsa_v1_agent_handler, [DELETE]},
