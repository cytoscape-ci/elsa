-module(elsa_task_worker_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_service_worker_test_() ->
  {"Test service worker API.",
   { foreach
   , fun setup/0
   , fun cleanup/1
   , [fun test_find_task_worker/1,
      fun test_store_and_get_params/1,
      fun test_change_params/1,
      fun test_store_and_get_data/1,
      fun test_change_data/1
     ]}}.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  Worker_Pid = case elsa_task_worker:start_link(self()) of
    {error,{already_started, Worker}} -> Worker;
    {ok, Worker} -> Worker
  end,
  {Worker_Pid, elsa_task:get_id(self())}.

cleanup(Worker)->
  ok.

test_find_task_worker({Worker, ID}) ->
  TestWorker = elsa_task_worker:find(ID),
  [?_assertEqual(Worker, TestWorker)].

test_store_and_get_params({_, ID}) ->
  elsa_task_worker:store_params(ID, test_params),
  Params = elsa_task_worker:get_params(ID),
  [?_assertEqual(test_params, Params)].

test_change_params({_, ID}) ->
  elsa_task_worker:store_params(ID, first_params),
  elsa_task_worker:store_params(ID, test_params),
  Params = elsa_task_worker:get_params(ID),
  [?_assertEqual(test_params, Params)].

test_store_and_get_data({_, ID}) ->
  elsa_task_worker:store_data(ID, test_data),
  Data = elsa_task_worker:get_data(ID),
  [?_assertEqual(test_data, Data)].

test_change_data({_, ID}) ->
  elsa_task_worker:store_data(ID, first_data),
  elsa_task_worker:store_data(ID, test_data),
  Data = elsa_task_worker:get_data(ID),
  [?_assertEqual(test_data, Data)].
