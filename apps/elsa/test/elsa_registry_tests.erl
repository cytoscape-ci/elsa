
-module(elsa_registry_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_service_worker_test_() ->
  {"Test service worker API.",
   { foreach
   , fun setup/0
   , fun cleanup/1
   , [fun test_register_single_instance/1,
      fun test_unregister_single_instance/1,
      fun test_register_multiple_instances/1,
      fun test_unregister_multiple_instances/1,
      fun test_register_multiple_services/1,
      fun test_unregister_multiple_services/1
     ]}}.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  ok.

cleanup(Worker)->
  ok.

test_register_single_instance(_Setup) ->
  elsa_registry:register(single_service_single_instance_mock()),
  Instance = elsa_registry:checkout(<<"registry_test">>, <<"v1">>),
  [?_assertEqual(<<"1.1.1.1">>, Instance)].

test_unregister_single_instance(_Setup) ->
  elsa_registry:unregister(single_service_single_instance_mock()),
  Instance = elsa_registry:checkout(<<"registry_test">>, <<"v1">>),
  [?_assertEqual(unavailable, Instance)].

test_register_multiple_instances(_Setup) ->
  elsa_registry:register(single_service_multiple_instance_mock()),
  elsa_registry:checkout(<<"registry_test">>, <<"v2">>),
  Instance = elsa_registry:checkout(<<"registry_test">>,<<"v2">>),
  [?_assertEqual(<<"1.1.1.2">>, Instance)].

test_unregister_multiple_instances(_Setup) ->
  elsa_registry:unregister(single_service_multiple_instance_mock()),
  elsa_registry:checkout(<<"registry_test">>,<<"v2">>),
  Instance = elsa_registry:checkout(<<"registry_test">>,<<"v2">>),
  [?_assertEqual(unavailable, Instance)].

test_register_multiple_services(_Setup) ->
  elsa_registry:register(multiple_service_multiple_instance_mock()),
  V1Instance = elsa_registry:checkout(<<"registry_testall">>,<<"v1">>),
  V2Instance = elsa_registry:checkout(<<"registry_testall">>, <<"v2">>),
  [?_assertEqual(V1Instance, V2Instance)].

test_unregister_multiple_services(_Setup) ->
  elsa_registry:unregister(multiple_service_multiple_instance_mock()),
  Instance = elsa_registry:checkout(<<"registry_testall">>,<<"v1">>),
  [?_assertEqual(unavailable, Instance)].

single_service_single_instance_mock() ->
  [
  {<<"service">>, <<"registry_test">>},
  {<<"version">>,<<"v1">>},
  {<<"instances">>,[
    [{<<"location">>,<<"1.1.1.1">>},
     {<<"capacity">>, 4}]
   ]}].

single_service_multiple_instance_mock() ->
 [
 {<<"service">>, <<"registry_test">>},
 {<<"version">>,<<"v2">>},
 {<<"instances">>,[
   [{<<"location">>,<<"1.1.1.1">>},
    {<<"capacity">>, 1}],
   [{<<"location">>,<<"1.1.1.2">>},
    {<<"capacity">>, infinity}]
  ]}].

multiple_service_multiple_instance_mock() ->
 [[
   {<<"service">>, <<"registry_testall">>},
   {<<"version">>,<<"v1">>},
   {<<"instances">>,[
     [{<<"location">>,<<"1.1.1.1">>},
      {<<"capacity">>, 1}],
     [{<<"location">>,<<"1.1.1.2">>},
      {<<"capacity">>, infinity}]
    ]}
 ],
 [
   {<<"service">>, <<"registry_testall">>},
   {<<"version">>,<<"v2">>},
   {<<"instances">>,[
     [{<<"location">>,<<"1.1.1.1">>},
      {<<"capacity">>, 1}],
     [{<<"location">>,<<"1.1.1.2">>},
      {<<"capacity">>, infinity}]
   ]}
  ]
 ].
