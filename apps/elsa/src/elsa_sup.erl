
-module(elsa_sup).
-behaviour(supervisor).

-export([start_link/0
        ]).

-export([init/1
        ]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD(Mod, Restart, Type),
        {Mod, {Mod, start_link, []}, Restart, 1000, Type, [Mod]}).

start_link() ->
  lager:info("ELSA supervisor started."),
  supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []).

init([]) ->
  Sup_flags = #{
    strategy => one_for_one
  },
  Supervisors = [
    ?CHILD(elsa_service_worker_sup, permanent, supervisor),
    ?CHILD(elsa_task_worker_sup, permanent, supervisor)
  ],
  {ok, {Sup_flags, Supervisors}}.
