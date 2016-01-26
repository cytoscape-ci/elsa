
-module(elsa_task).

-export([status/1,
         store_data/2,
         data/1,
         get_id/1,
         remove/1
         ]).

status(ID) ->
  lager:info("Checking status of task: ~s", [ID]),
  case elsa_task_worker:find(ID) of
    undefined ->
      not_found;
    _ ->
      check_data(elsa_task_worker:get_data(ID))
  end.

check_data(incomplete) ->
  incomplete;
check_data(_Data) ->
  complete.

store_data(ID, Data) ->
  lager:info("Storing data for task: ~s", [ID]),
  elsa_task_worker:store_data(ID, Data).

data(ID) ->
  lager:info("Retreiving data for task: ~s", [ID]),
  elsa_task_worker:get_data(ID).

get_id(Task) when is_pid(Task) ->
  base16:encode(crypto:hash(sha256, pid_to_list(Task))).

remove(ID) ->
  lager:info("Request to remove task").
