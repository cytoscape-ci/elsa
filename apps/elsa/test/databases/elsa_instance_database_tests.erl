-module(elsa_instance_database_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_task_database_test_() ->
  {"Test service database API.",
   {inorder,
     {foreach
       , fun setup/0
       , fun cleanup/1
       , [fun test_register_instance_checkout/1,
          fun test_unregister_instance_checkin/1,
          fun test_checkin_checkout_instance/1,
          fun test_checkout_bound_before_unbounded_instances/1
         ]
     }
   }
  }.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  elsa_instance_database:load(<<"test">>,<<"v1">>),
  mnesia:wait_for_tables([elsa_service__test_v1], 3000),
  elsa_instance_database:clear(<<"test">>,<<"v1">>),
  ok.

cleanup(_Setup)->
  elsa_instance_database:clear(<<"test">>,<<"v1">>),
  ok.

test_register_instance_checkout(_Setup) ->
  elsa_instance_database:register(<<"test">>,<<"v1">>,<<"1.1.1.1">>,1),
  Instance = elsa_instance_database:checkout(<<"test">>,<<"v1">>),
  [?_assertEqual(<<"1.1.1.1">>, Instance)].

test_unregister_instance_checkin(_Setup) ->
  elsa_instance_database:register(<<"test">>,<<"v1">>,<<"1.1.1.1">>,1),
  elsa_instance_database:unregister(<<"test">>,<<"v1">>,<<"1.1.1.1">>),
  Instance = elsa_instance_database:checkout(<<"test">>,<<"v1">>),
  [?_assertEqual(unavailable, Instance)].

test_checkin_checkout_instance(_Setup) ->
  elsa_instance_database:register(<<"test">>,<<"v1">>,<<"1.1.1.1">>,1),
  Instance = elsa_instance_database:checkout(<<"test">>,<<"v1">>),
  elsa_instance_database:checkin(<<"test">>,<<"v1">>,Instance),
  [?_assertEqual(Instance, elsa_instance_database:checkout(<<"test">>,<<"v1">>))].

test_checkout_bound_before_unbounded_instances(_Setup) ->
  elsa_instance_database:register(<<"test">>,<<"v1">>,<<"1.1.1.1">>,1),
  elsa_instance_database:register(<<"test">>,<<"v1">>,<<"1.1.1.2">>,infinity),
  elsa_instance_database:checkout(<<"test">>,<<"v1">>),
  Instance = elsa_instance_database:checkout(<<"test">>,<<"v1">>),
  [?_assertEqual(<<"1.1.1.2">>, Instance)].
