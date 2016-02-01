% @doc Chatastic Tests
% @end

%%% Module
-module(elsa_service_worker_tests).

%%% Includes

-include_lib("eunit/include/eunit.hrl").

%%% Macros


%%% Tests

elsa_service_worker_test_() ->
  {"Test service worker API.",
  {inorder,
   { foreach
     , fun setup/0
     , fun cleanup/1
     , [fun test_find_service_worker/1,
        fun test_register_bounded_service_instance/1,
        fun test_checkout_bounded_service_instance/1,
        fun test_checkin_bounded_service_instance/1,
        fun test_register_unbounded_service_instance/1,
        fun test_checkout_unbounded_service_instance/1,
        fun test_checkin_unbounded_service_instance/1,
        fun test_checkout_different_unbounded_service_instance/1,
        fun test_checkout_unbound_before_bound_service_instance/1
       ]
     }
   }
  }.

setup() ->
  {ok, _Started} = application:ensure_all_started(elsa),
  elsa_service_database:load(),
  elsa_service_database:clear(),
  elsa_instance_database:clear(<<"test">>, <<"v1">>),
  worker(<<"test">>, <<"v1">>).

cleanup(Worker)->
  elsa_service_worker:stop(<<"test">>,<<"v1">>),
  elsa_service_database:clear(),
  elsa_instance_database:clear(<<"test">>, <<"v1">>),
  ok.

test_find_service_worker(Worker) ->
  TestWorker = elsa_service_worker:find(<<"test">>, <<"v1">>),
  [?_assertEqual(Worker, TestWorker)].

test_register_bounded_service_instance(Worker) ->
  OK = elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.1">>, 3),
  [?_assertEqual(ok, OK)].

test_checkout_bounded_service_instance(_Setup) ->
  elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.1">>, 3),
  Instance = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  [?_assertEqual(<<"1.1.1.1">>, Instance)].

test_checkin_bounded_service_instance(_Setup) ->
  elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.1">>, 1),
  elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  Unavailable = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  elsa_service_worker:checkin(<<"test">>, <<"v1">>, <<"1.1.1.1">>),
  Instance = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  [?_assertEqual(unavailable, Unavailable), ?_assertEqual(<<"1.1.1.1">>, Instance)].

test_register_unbounded_service_instance(Worker) ->
  OK = elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.1">>, infinity),
  [?_assertEqual(ok, OK)].

test_checkout_unbounded_service_instance(_Setup) ->
   elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.2">>, infinity),
   Instance = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
   [?_assertEqual(<<"1.1.1.2">>, Instance)].

test_checkin_unbounded_service_instance(_Setup) ->
  elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.1">>, 1),
  Instance = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  elsa_service_worker:checkin(<<"test">>, <<"v1">>, <<"1.1.1.1">>),
  Instance = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  [?_assertEqual(<<"1.1.1.1">>, Instance)].

test_checkout_different_unbounded_service_instance(_Setup) ->
  elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.2">>, infinity),
  elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.3">>, infinity),
  Instance = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  Instance2 = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  [?_assertNotEqual(Instance, Instance2)].

test_checkout_unbound_before_bound_service_instance(_Setup) ->
  elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.2">>, 1),
  elsa_service_worker:register(<<"test">>, <<"v1">>, <<"1.1.1.3">>, infinity),
  Instance = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  Instance2 = elsa_service_worker:checkout(<<"test">>, <<"v1">>),
  [?_assertEqual(<<"1.1.1.2">>, Instance), ?_assertEqual(<<"1.1.1.3">>, Instance2)].

worker(Service, Version) ->
  case elsa_service_worker:start_link(Service, Version) of
    {error,{already_started, Worker}} -> Worker;
    {ok, Worker} -> Worker
  end.
