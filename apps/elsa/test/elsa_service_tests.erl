
-module(elsa_service_tests).

-include_lib("eunit/include/eunit.hrl").

elsa_task_database_test_() ->
  {"Test service API.",
   {inorder,
     {foreach
       , fun setup/0
       , fun cleanup/1
       , [fun test_register_single_instance/1,
          fun test_unregister_single_instance/1,
          fun test_register_multiple_instances/1,
          fun test_unregister_multiple_instances/1,
          fun test_register_multiple_services/1,
          fun test_unregister_multiple_services/1,
          fun test_get_all_with_no_services/1,
          fun test_get_all_with_one_service/1,
          fun test_get_all_with_multiple_services/1,
          fun test_get_versions_with_no_versions/1,
          fun test_get_versions_with_one_version/1,
          fun test_get_versions_with_multiple_versions/1,
          fun test_get_instances_with_no_instances/1,
          fun test_get_instances_with_one_instance/1,
          fun test_get_instances_with_multiple_instances/1
         ]
     }
   }
  }.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  elsa_service_database:load(),
  elsa_service_database:clear(),
  elsa_instance_database:clear(<<"test">>, <<"v1">>),
  elsa_instance_database:clear(<<"test">>, <<"v2">>),
  elsa_instance_database:clear(<<"test2">>, <<"v1">>),
  elsa_instance_database:clear(<<"test2">>, <<"v2">>),
  ok.

cleanup(_Setup)->
  elsa_service_database:clear(),
  elsa_instance_database:clear(<<"test">>, <<"v1">>),
  elsa_instance_database:clear(<<"test">>, <<"v2">>),
  elsa_instance_database:clear(<<"test2">>, <<"v1">>),
  elsa_instance_database:clear(<<"test2">>, <<"v2">>),
  ok.

test_register_single_instance(_Setup) ->
  elsa_service:register(single_service_single_instance_mock()),
  Instance = elsa_service:checkout(<<"test">>, <<"v1">>),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  [?_assertEqual(<<"1.1.1.1">>, Instance)].

test_unregister_single_instance(_Setup) ->
  elsa_service:unregister(single_service_single_instance_mock()),
  Instance = elsa_service:checkout(<<"test">>, <<"v1">>),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  [?_assertEqual(unavailable, Instance)].

test_register_multiple_instances(_Setup) ->
  elsa_service:register(single_service_multiple_instance_mock()),
  elsa_service:checkout(<<"test">>, <<"v2">>),
  Instance = elsa_service:checkout(<<"test">>,<<"v2">>),
  elsa_service_worker:stop(<<"test">>,<<"v2">>),
  [?_assertEqual(<<"1.1.1.2">>, Instance)].

test_unregister_multiple_instances(_Setup) ->
  elsa_service:register(single_service_multiple_instance_mock()),
  elsa_service:unregister(single_service_multiple_instance_mock()),
  Instance = elsa_service:checkout(<<"test">>,<<"v2">>),
  elsa_service_worker:stop(<<"test">>,<<"v2">>),
  [?_assertEqual(unavailable, Instance)].

test_register_multiple_services(_Setup) ->
  elsa_service:register(multiple_service_multiple_instance_mock()),
  V1Instance = elsa_service:checkout(<<"test">>,<<"v1">>),
  V2Instance = elsa_service:checkout(<<"test2">>, <<"v1">>),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  elsa_service_worker:stop(<<"test2">>,<<"v1">>),
  [?_assertEqual(V1Instance, V2Instance)].

test_unregister_multiple_services(_Setup) ->
  elsa_service:register(multiple_service_multiple_instance_mock()),
  elsa_service:unregister(multiple_service_multiple_instance_mock()),
  Instance = elsa_service:checkout(<<"test">>,<<"v1">>),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  elsa_service_worker:stop(<<"test2">>,<<"v1">>),
  [?_assertEqual(unavailable, Instance)].

test_get_all_with_no_services(_Setup) ->
  Services = elsa_service:all(),
  [?_assertEqual([], Services)].

test_get_all_with_one_service(_Setup) ->
  elsa_service:register(single_service_single_instance_mock()),
  Services = elsa_service:all(),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  [?_assertEqual(1, length(Services))].

test_get_all_with_multiple_services(_Setup) ->
  elsa_service:register(multiple_service_multiple_instance_mock()),
  Services = elsa_service:all(),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  elsa_service_worker:stop(<<"test2">>,<<"v1">>),
  [?_assertEqual(2, length(Services))].

test_get_versions_with_no_versions(_Setup) ->
  Versions = elsa_service:versions(<<"test">>),
  [?_assertEqual([], Versions)].

test_get_versions_with_one_version(_Setup) ->
  elsa_service:register(single_service_single_instance_mock()),
  Versions = elsa_service:versions(<<"test">>),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  [?_assertEqual(1, length(Versions))].

test_get_versions_with_multiple_versions(_Setup) ->
  elsa_service:register(multiple_service_multiple_instance_multiple_version_mock()),
  Versions = elsa_service:versions(<<"test2">>),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  elsa_service_worker:stop(<<"test2">>,<<"v1">>),
  elsa_service_worker:stop(<<"test2">>,<<"v2">>),
  [?_assertEqual(2, length(Versions))].

test_get_instances_with_no_instances(_Setup) ->
  Instances = elsa_service:instances(<<"test">>, <<"v1">>),
  [?_assertEqual([], Instances)].

test_get_instances_with_one_instance(_Setup) ->
  elsa_service:register(single_service_single_instance_mock()),
  Instances = elsa_service:instances(<<"test">>, <<"v1">>),
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  [?_assertEqual(1, length(Instances))].

test_get_instances_with_multiple_instances(_Setup) ->
  elsa_service:register(single_service_multiple_instance_mock()),
  Instances = elsa_service:instances(<<"test">>, <<"v2">>),
  elsa_service_worker:stop(<<"test">>,<<"v2">>),
  [?_assertEqual(2, length(Instances))].

single_service_single_instance_mock() ->
  [
  {<<"service">>, <<"test">>},
  {<<"version">>,<<"v1">>},
  {<<"instances">>,[
    [{<<"location">>,<<"1.1.1.1">>},
     {<<"capacity">>, 4}]
   ]}].

single_service_multiple_instance_mock() ->
 [
 {<<"service">>, <<"test">>},
 {<<"version">>,<<"v2">>},
 {<<"instances">>,[
   [{<<"location">>,<<"1.1.1.1">>},
    {<<"capacity">>, 1}],
   [{<<"location">>,<<"1.1.1.2">>},
    {<<"capacity">>, <<"infinity">>}]
  ]}].

multiple_service_multiple_instance_mock() ->
 [[
   {<<"service">>, <<"test">>},
   {<<"version">>,<<"v1">>},
   {<<"instances">>,[
     [{<<"location">>,<<"1.1.1.1">>},
      {<<"capacity">>, 1}],
     [{<<"location">>,<<"1.1.1.2">>},
      {<<"capacity">>, <<"infinity">>}]
    ]}
 ],
 [
   {<<"service">>, <<"test2">>},
   {<<"version">>,<<"v1">>},
   {<<"instances">>,[
     [{<<"location">>,<<"1.1.1.1">>},
      {<<"capacity">>, 1}],
     [{<<"location">>,<<"1.1.1.2">>},
      {<<"capacity">>, <<"infinity">>}]
   ]}
  ]
 ].

 multiple_service_multiple_instance_multiple_version_mock() ->
  [[
    {<<"service">>, <<"test">>},
    {<<"version">>,<<"v1">>},
    {<<"instances">>,[
      [{<<"location">>,<<"1.1.1.1">>},
       {<<"capacity">>, 1}],
      [{<<"location">>,<<"1.1.1.2">>},
       {<<"capacity">>, <<"infinity">>}]
     ]}
  ],
  [
    {<<"service">>, <<"test2">>},
    {<<"version">>,<<"v1">>},
    {<<"instances">>,[
      [{<<"location">>,<<"1.1.1.1">>},
       {<<"capacity">>, 1}],
      [{<<"location">>,<<"1.1.1.2">>},
       {<<"capacity">>, <<"infinity">>}]
    ]}
   ],
   [
     {<<"service">>, <<"test2">>},
     {<<"version">>,<<"v2">>},
     {<<"instances">>,[
       [{<<"location">>,<<"1.1.1.1">>},
        {<<"capacity">>, 1}],
       [{<<"location">>,<<"1.1.1.2">>},
        {<<"capacity">>, <<"infinity">>}]
     ]}
    ]
  ].
