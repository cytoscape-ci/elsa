
-module(elsa_task_worker_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_child/1
        ]).

-export([init/1
        ]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD, elsa_task_worker).

start_link() ->
  lager:info("Task worker supervisor started."),
  supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, []).

start_child(PID) ->
  case supervisor:start_child(?SUPERVISOR, [PID]) of
    {ok, Pid} ->
      lager:info("Task started for monitor: ~p", [PID]),
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
