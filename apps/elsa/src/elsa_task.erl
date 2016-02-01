
-module(elsa_task).

-export([new/5,
         status/1,
         store/4,
         store/2,
         retreive/1,
         delete/1,
         all/0
         ]).

new(PID, Service, Version, Method, Endpoint) when is_pid(PID) ->
  ID = base16:encode(crypto:hash(sha256, pid_to_list(PID))),
  case elsa_task_database:info(ID) of
    not_found ->
      elsa_task_database:create(ID, Service, Version, Method, Endpoint),
      ID;
    _ -> already_exists
  end.

status(ID) ->
  lager:info("Checking status of task: ~s", [ID]),
  elsa_task_database:info(ID).

store(ID, Status, Headers, Response) ->
  store(ID, {Status, Headers, Response}).

store(ID, {S, H, R}) ->
  elsa_task_database:store(ID, {S, H, R}).

retreive(ID) ->
  case elsa_task_database:retreive(ID) of
    {task, ID, _Completed, _, _, _, _, _, S, H, R} -> {S, H, R};
    not_found -> not_found
  end.

delete(ID) ->
  elsa_task_database:delete(ID).

all() ->
  elsa_task_database:all().
