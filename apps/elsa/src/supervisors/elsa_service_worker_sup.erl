
-module(elsa_service_worker_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_child/2
        ]).

-export([init/1
        ]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD, elsa_service_worker).

start_link() ->
  S = supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []),
  lager:info("Service worker supervisor started."),
  elsa_service_database:load(),
  S.

start_child(Service, Version) ->
  case supervisor:start_child(?SUPERVISOR, [Service, Version]) of
    {ok, Pid} ->
      lager:info("Supervisor started service ~s version ~s", [Service, Version]),
      {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid}
  end.

%% @hidden
init([]) ->
  Sup_flags = #{
    strategy => simple_one_for_one,
    intensity => 10,
    period => 60
  },
  Service_worker = #{
    id => ?CHILD,
    start => {?CHILD, start_link, []},
    restart => transient,
    shutdown => 1000,
    type => worker
  },
  {ok, {Sup_flags, [Service_worker]}}.
