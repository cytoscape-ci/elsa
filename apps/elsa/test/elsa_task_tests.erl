
-module(elsa_task_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_task_database_test_() ->
  {"Test task API.",
   {inorder,
     {foreach
       , fun setup/0
       , fun cleanup/1
       , [fun test_create_new_task/1,
          fun test_task_already_exists/1,
          fun test_store_response_in_task/1,
          fun test_remove_task/1,
          fun test_status_complete/1,
          fun test_status_incomplete/1
         ]
     }
   }
  }.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  elsa_task_database:load(),
  mnesia:wait_for_tables([elsa_tasks], 3000),
  elsa_task_database:clear(),
  ok.

cleanup(_Setup)->
  elsa_task_database:clear(),
  ok.

test_create_new_task(_Setup) ->
  [?_assertNotEqual(elsa_task:new(self(),<<"test">>,<<"v1">>,<<"GET">>,<<"/">>), already_exists)].

test_task_already_exists(_Setup) ->
  elsa_task:new(self(),<<"test">>,<<"v1">>,<<"GET">>,<<"/">>),
  Exists = elsa_task:new(self(),<<"test">>,<<"v1">>,<<"GET">>,<<"/">>),
  [?_assertEqual(Exists, already_exists)].

test_store_response_in_task(_Setup) ->
  ID = elsa_task:new(self(),<<"test">>,<<"v1">>,<<"GET">>,<<"/">>),
  Data = {<<"400">>, [<<"content-type">>,<<"test">>], <<"body">>},
  elsa_task:store(ID, Data),
  Data2 = elsa_task:retreive(ID),
  [?_assertEqual(Data, Data2)].

test_remove_task(_Setup) ->
  ID = elsa_task:new(self(),<<"test">>,<<"v1">>,<<"GET">>,<<"/">>),
  elsa_task:delete(ID),
  Exists = elsa_task:status(ID),
  [?_assertEqual(not_found, Exists)].

test_status_complete(_Setup) ->
  ID = elsa_task:new(self(),<<"test">>,<<"v1">>,<<"GET">>,<<"/">>),
  Data = {<<"400">>, [<<"content-type">>,<<"test">>], <<"body">>},
  elsa_task:store(ID, Data),
  {completed, Completed} = elsa_task:status(ID),
  [?_assertEqual(true, Completed)].

test_status_incomplete(_Setup) ->
  ID = elsa_task:new(self(),<<"test">>,<<"v1">>,<<"GET">>,<<"/">>),
  {completed, Completed} = elsa_task:status(ID),
  [?_assertEqual(false, Completed)].
