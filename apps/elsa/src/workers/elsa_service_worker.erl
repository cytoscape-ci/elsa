
-module(elsa_service_worker).
-behaviour(gen_server).

-export([start_link/2,
         stop/2,
         find/2,
         register/4,
         unregister/3,
         checkout/2,
         checkin/3]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

start_link(Service, Version) when is_binary(Service), is_binary(Version) ->
  gen_server:start_link({global, {service_worker, {Service, Version}}},
                        ?MODULE,
                        {Service, Version}, []).

stop(Service, Version) ->
    gen_server:call(find(Service, Version), stop).

init(State = {Service, Version}) ->
  elsa_instance_database:load(Service, Version),
  elsa_service_database:create(Service, Version),
  lager:info("Service worker for service ~s version ~s created.", [Service, Version]),
  {ok, {service, State}}.

find(Service, Version) when is_binary(Service), is_binary(Version) ->
  global:whereis_name({service_worker, {Service, Version}}).

register(Service, Version, Instance, Capacity) when is_binary(Service), is_binary(Version), is_binary(Instance), is_atom(Capacity); is_integer(Capacity) ->
  gen_server:call(find(Service, Version), {register, Instance, Capacity}).

unregister(Service, Version, Instance) when is_binary(Service), is_binary(Version), is_binary(Instance) ->
  gen_server:call(find(Service, Version), {unregister, Instance}).

checkout(Service, Version) when is_binary(Service), is_binary(Version) ->
  gen_server:call(find(Service, Version), checkout).

checkin(Service, Version, Instance) when is_binary(Service), is_binary(Version), is_binary(Instance) ->
  gen_server:call(find(Service, Version), {checkin, Instance}).

handle_call({register, Instance, Capacity}, _From, State = {service, {Service, Version}}) ->
  elsa_service_database:instance_added(Service, Version, Capacity),
  elsa_instance_database:register(Service, Version, Instance, Capacity),
  {reply, ok, State};

handle_call({unregister, Instance}, _From, State = {service, {Service, Version}}) ->
  {instance, _, _, Capacity, _} = elsa_instance_database:retreive(Service, Version, Instance),
  elsa_service_database:instance_removed(Service, Version, Capacity),
  elsa_instance_database:unregister(Service, Version, Instance),
  {reply, ok, State};

handle_call({checkin, Instance}, _From, State = {service, {Service, Version}}) ->
  elsa_service_database:instance_in(Service, Version),
  elsa_instance_database:checkin(Service, Version, Instance),
  {reply, ok, State};

handle_call(checkout, _From, State = {service, {Service, Version}}) ->
  elsa_service_database:instance_out(Service, Version),
  {reply, elsa_instance_database:checkout(Service, Version), State};

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.

handle_info(Message, State = {service, {Service, Version}}) ->
  lager:info("Service worker for service ~s version ~s received an unknown message: ~p", [Service, Version, Message]),
  {noreply, State}.

handle_cast({_, _}, State) -> {noreply, State}.

terminate(_Reason, {service, {Service, Version}}) ->
  lager:info("Service worker for service ~s version ~s terminated.", [Service, Version]),
  ok.

code_change(_LastVsn, State, _Opt) -> {ok, State}.
