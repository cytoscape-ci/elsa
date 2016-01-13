
-module(elsa_task_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_service_worker_test_() ->
  {"Test service worker API.",
   { foreach
   , fun setup/0
   , fun cleanup/1
   , [fun test_status_without_task/1,
      fun test_status_with_task/1,
      fun test_status_with_task_and_data/1
     ]}}.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  ok.

cleanup(Worker)->
  ok.

test_status_without_task(_Setup) ->
  Not_found = elsa_task:status(<<"FAKETASK">>),
  [?_assertEqual(not_found, Not_found)].

test_status_with_task(_Setup) ->
  elsa_task_worker:start_link(self()),
  Incomplete = elsa_task:status(elsa_task:get_id(self())),
  [?_assertEqual(incomplete, Incomplete)].

test_status_with_task_and_data(_Setup) ->
  elsa_task_worker:start_link(self()),
  elsa_task:store_data(elsa_task:get_id(self()), data),
  Complete = elsa_task:status(elsa_task:get_id(self())),
  [?_assertEqual(complete, Complete)].
