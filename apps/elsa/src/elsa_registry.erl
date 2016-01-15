
-module(elsa_registry).

-export([register/1,
         unregister/1,
         checkout/2,
         checkin/3]).

-record(registration, {service, version, instances}).
-record(instance, {location, capacity}).

%%Registration

register([Service|_] = Registration) when is_list(Service) ->
  [register(S) || S <- Registration];

register(Registration) ->
  lager:info("Registering ~p", [Registration]),
  Reg = service(Registration),
  {Service, Version} = {Reg#registration.service, Reg#registration.version},
  ensure_service(Service, Version),
  register_instances(Service, Version, Reg#registration.instances).

register_instances(Service, Version, Instances) ->
  [elsa_service_worker:register(Service, Version, I#instance.location, I#instance.capacity) || I <- Instances].

%%Unregistration

unregister([Service|_] = Registration) when is_list(Service) ->
  [elsa_registry:unregister(S) || S <- Registration];

unregister(Registration) ->
  lager:info("Unregistering ~p", [Registration]),
  Reg = service(Registration),
  {Service, Version} = {Reg#registration.service, Reg#registration.version},
  ensure_service(Service, Version),
  unregister_instances(Service, Version,  Reg#registration.instances).

unregister_instances(Service, Version, Instances) ->
  [elsa_service_worker:unregister(Service, Version, I#instance.location) || I <- Instances].

%%Using Instances

checkout(Service, Version) ->
  lager:info("Checking out instance of service: ~s version: ~s", [Service, Version]),
  ensure_service(Service, Version),
  elsa_service_worker:checkout(Service, Version).

checkin(Service, Version, Instance) ->
  lager:info("Checking in instance of service: ~s", [Service]),
  ensure_service(Service, Version),
  elsa_service_worker:checkin(Service, Version, Instance).

%% Json registration to map

service(Service) ->
  #registration{
    service=proplists:get_value(<<"service">>, Service),
    version=proplists:get_value(<<"version">>, Service),
    instances=[instance(I) || I <- proplists:get_value(<<"instances">>, Service)]
  }.

instance(Instance) ->
  #instance{
    location=proplists:get_value(<<"location">>, Instance),
    capacity=capacity(proplists:get_value(<<"capacity">>, Instance))
  }.

%% Utility

ensure_service(Service, Version) ->
  Worker = elsa_service_worker:find(Service, Version),
  case Worker of
     undefined -> elsa_service_worker_sup:start_child(Service, Version);
     Pid -> ok
  end.

capacity(<<"infinity">>) -> infinity;
capacity(Capacity) when is_binary(Capacity) -> binary_to_integer(Capacity);
capacity(Capacity) -> Capacity.
