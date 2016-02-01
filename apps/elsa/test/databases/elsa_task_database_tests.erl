
-module(elsa_task_database_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_task_database_test_() ->
  {"Test task database API.",
   {inorder,
     {foreach
       , fun setup/0
       , fun cleanup/1
       , [fun test_task_not_found/1,
          fun test_store_task/1,
          fun test_store_data_for_created_task/1,
          fun test_retreive_non_existant_task/1,
          fun test_delete_task/1,
          fun test_delete_non_existant_task/1,
          fun test_get_all_tasks/1,
          fun test_get_all_tasks_for_empty_table/1,
          fun test_get_all_complete_tasks/1,
          fun test_get_all_complete_tasks_for_empty_table/1,
          fun test_get_all_incomplete_tasks/1,
          fun test_get_all_incomplete_tasks_for_empty_table/1
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

test_task_not_found(_Setup) ->
  [?_assertEqual(elsa_task_database:info(<<"test">>), not_found)].

test_store_task(_Setup) ->
  ok = elsa_task_database:create(<<"test">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  {completed, Completed} = elsa_task_database:info(<<"test">>),
  [?_assertEqual(Completed, false)].

test_store_data_for_created_task(_Setup) ->
  ok = elsa_task_database:create(<<"test">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:store(<<"test">>, {200, [], <<"body">>}),
  {task, <<"test">>, true, _, _, _, _, _, Status, Headers, Body} = elsa_task_database:retreive(<<"test">>),
  [?_assertEqual({200, [], <<"body">>}, {Status, Headers, Body})].

test_store_data_for_task(_Setup) ->
  Info = elsa_task_database:store(<<"test">>, {200, [], <<"body">>}),
  [?_assertEqual(Info, not_found)].

test_retreive_non_existant_task(_Setup) ->
  [?_assertEqual(elsa_task_database:retreive(<<"test">>), not_found)].

test_delete_task(_Setup) ->
  ok = elsa_task_database:create(<<"test">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:store(<<"test">>, {200, [], <<"body">>}),
  ok = elsa_task_database:delete(<<"test">>),
  [?_assertEqual(elsa_task_database:retreive(<<"test">>), not_found)].

test_delete_non_existant_task(_Setup) ->
  ok = elsa_task_database:delete(<<"test">>),
  [?_assertEqual(elsa_task_database:retreive(<<"test">>), not_found)].

test_get_all_tasks(_Setup) ->
  ok = elsa_task_database:create(<<"test">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test1">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test2">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test3">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:store(<<"test3">>, {200, [], <<"body">>}),
  [?_assertEqual(length(elsa_task_database:all()), 4)].

test_get_all_tasks_for_empty_table(_Setup) ->
  [?_assertEqual(elsa_task_database:all(), [])].

test_get_all_complete_tasks(_Setup) ->
  ok = elsa_task_database:create(<<"test">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test1">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test2">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test3">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:store(<<"test3">>, {200, [], <<"body">>}),
  [?_assertEqual(length(elsa_task_database:complete()), 1)].

test_get_all_complete_tasks_for_empty_table(_Setup) ->
  [?_assertEqual(elsa_task_database:complete(), [])].

test_get_all_incomplete_tasks(_Setup) ->
  ok = elsa_task_database:create(<<"test">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test1">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test2">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:create(<<"test3">>,<<"service">>,<<"v1">>,<<"GET">>,<<"/">>),
  ok = elsa_task_database:store(<<"test3">>, {200, [], <<"body">>}),
  [?_assertEqual(length(elsa_task_database:incomplete()), 3)].

test_get_all_incomplete_tasks_for_empty_table(_Setup) ->
  [?_assertEqual(elsa_task_database:incomplete(), [])].
