
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
  cowboy_router:compile([
        {Host, [
          {"/task/:id", elsa_task_handler, []},
          {"/registration/[...]", elsa_registration_handler, []},
          {"/:service/:version/[...]", elsa_kernel, []}
        ]}
    ]).
