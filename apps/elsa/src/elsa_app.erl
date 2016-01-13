
-module(elsa_app).

-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting ELSA."),
    lager:info("Setting up database."),
    database_setup([node()]),
    lager:info("Starting router."),
    elsa_router:start(),
    lager:info("Router started."),
    lager:info("Starting ELSA supervisor."),
    'elsa_sup':start_link().

stop(_State) ->
    database_teardown([node()]),
    ok.

database_setup(Nodes) ->
  mnesia:stop(),
  case mnesia:create_schema([node()]) of
    ok -> lager:info("Schema created.");
    {error, Reason} -> lager:error("Error while creating schema: ~w", [Reason])
  end,
  mnesia:start(),
  rpc:multicall(Nodes, application, start, [mnesia]),
  lager:info("Mnesia started.").

database_teardown(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]),
  lager:info("Mnesia stopped.").
