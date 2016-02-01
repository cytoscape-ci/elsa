
-module(elsa_service_database_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_task_database_test_() ->
  {"Test service database API.",
   {inorder,
     {foreach
       , fun setup/0
       , fun cleanup/1
       , [fun test_create_service/1,
          fun test_instance_added/1,
          fun test_instance_removed/1,
          fun test_instance_in/1,
          fun test_instance_out/1,
          fun test_retreive_service/1,
          fun test_retreive_non_existant_service/1,
          fun test_get_all_services/1,
          fun test_get_all_services_for_empty_table/1,
          fun test_get_all_versions/1,
          fun test_get_all_version_for_non_existant_service/1
         ]
     }
   }
  }.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  elsa_service_database:load(),
  mnesia:wait_for_tables([elsa_services], 3000),
  elsa_service_database:clear(),
  ok.

cleanup(_Setup)->
  elsa_service_database:clear(),
  ok.

test_create_service(_Setup) ->
  [?_assertEqual(ok, elsa_service_database:create(<<"test">>, <<"v1">>))].

test_instance_added(_Setup) ->
  elsa_service_database:create(<<"test">>, <<"v1">>),
  elsa_service_database:instance_added(<<"test">>, <<"v1">>, 0),
  {_, _, _, I, _, _} = elsa_service_database:retreive(<<"test">>, <<"v1">>),
  [?_assertEqual(1, I)].

test_instance_removed(_Setup) ->
  elsa_service_database:create(<<"test">>, <<"v1">>),
  elsa_service_database:instance_removed(<<"test">>, <<"v1">>, 0),
  {_, _, _, I, _, _} = elsa_service_database:retreive(<<"test">>, <<"v1">>),
  [?_assertEqual(-1, I)].

test_instance_in(_Setup) ->
  elsa_service_database:create(<<"test">>, <<"v1">>),
  elsa_service_database:instance_in(<<"test">>, <<"v1">>),
  {_, _, _, _, _, O} = elsa_service_database:retreive(<<"test">>, <<"v1">>),
  [?_assertEqual(1, O)].

test_instance_out(_Setup) ->
  elsa_service_database:create(<<"test">>, <<"v1">>),
  elsa_service_database:instance_out(<<"test">>, <<"v1">>),
  {_, _, _, _, _, O} = elsa_service_database:retreive(<<"test">>, <<"v1">>),
  [?_assertEqual(-1, O)].

test_retreive_service(_Setup) ->
  elsa_service_database:create(<<"test">>, <<"v1">>),
  Service = elsa_service_database:retreive(<<"test">>, <<"v1">>),
  [?_assertNotEqual(not_found, Service)].

test_retreive_non_existant_service(_Setup) ->
  [?_assertEqual(not_found, elsa_service_database:retreive(<<"test">>, <<"v1">>))].

test_get_all_services(_Setup) ->
  elsa_service_database:create(<<"test">>, <<"v1">>),
  elsa_service_database:create(<<"test">>, <<"v2">>),
  elsa_service_database:create(<<"test">>, <<"v3">>),
  elsa_service_database:create(<<"test2">>, <<"v1">>),
  elsa_service_database:create(<<"test2">>, <<"v2">>),
  [?_assertEqual(5, length(elsa_service_database:all()))].


test_get_all_services_for_empty_table(_Setup) ->
  [?_assertEqual(0, length(elsa_service_database:all()))].

test_get_all_versions(_Setup) ->
  elsa_service_database:create(<<"test">>, <<"v1">>),
  elsa_service_database:create(<<"test">>, <<"v2">>),
  elsa_service_database:create(<<"test">>, <<"v3">>),
  elsa_service_database:create(<<"test2">>, <<"v1">>),
  elsa_service_database:create(<<"test2">>, <<"v2">>),
  [?_assertEqual(3, length(elsa_service_database:versions(<<"test"  >>)))].

test_get_all_version_for_non_existant_service(_Setup) ->
  [?_assertEqual(0, length(elsa_service_database:versions(<<"test">>)))].
