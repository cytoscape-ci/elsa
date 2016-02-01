
-module(elsa_router).

-export([start/0
         ]).

start() ->
    {ok, _} = cowboy:start_http(elsa_server, 100, [{port, 80}],
        [{env, [{dispatch, dispatch('_')}]}]
    ).

dispatch(Host) ->
  Routes = routes(),
  lager:info("Compiling routes: ~p", [routes()]),
  cowboy_router:compile([{Host, Routes}]).

routes() ->
  v1_routes() ++
  kernel_route().

v1_routes() ->
  version("v1", [
    {"/task", v1_elsa_tasks_handler, []},
    {"/task/:id", v1_elsa_task_handler, []}
  ]) ++ version("v1", [
    {"/service", v1_elsa_services_handler, []},
    {"/service/:service_name", v1_elsa_service_handler, []},
    {"/service/:service_name/:version", v1_elsa_instances_handler, []}
  ]).

kernel_route() ->
  [{"/:service/:version/[...]", elsa_kernel, []}].

version(Version, Routes) when is_list(Routes) ->
  [version(Version, Route) || Route <- Routes];
version(Version, {Route, Handler, Options}) ->
  {"/" ++ Version ++ Route, Handler, Options}.


%TODO \/
%     {"/version", elsa_v1_version_handler, [GET]}
%     {"/v1/agent", elsa_v1_agent_handler, [GET]},
%     {"/v1/agent/:name", elsa_v1_agent_handler, [GET]},
%     {"/v1/agent/:name", elsa_v1_agent_handler, [DELETE]},
