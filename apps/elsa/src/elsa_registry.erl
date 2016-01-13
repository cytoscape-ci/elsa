
-module(elsa_registry).

-export([register/1,
         unregister/1,
         checkout/1,
         checkin/2]).

-record(registration, {name, version, instances}).
-record(instance, {location, capacity}).

register([Service|_] = Registration) when is_list(Service) ->
  [register(S) || S <- Registration];

register(Registration) ->
  lager:info("Registering ~p", [Registration]),
  Reg = service(Registration),
  Service = get_service_name(Reg#registration.version, Reg#registration.name),
  case elsa_service_worker:find(Service) of
    undefined ->
      elsa_service_worker_sup:start_child(Service),
      register_instances(Service, Reg#registration.instances);
    _ ->
      register_instances(Service, Reg#registration.instances)
  end.

register_instances(Service, Instances) ->
  [elsa_service_worker:register(Service, I#instance.location, I#instance.capacity) || I <- Instances].

unregister([Service|_] = Registration) when is_list(Service) ->
  [elsa_registry:unregister(S) || S <- Registration];

unregister(Registration) ->
  lager:info("Unregistering ~p", [Registration]),
  Reg = service(Registration),
  Service = get_service_name(Reg#registration.version, Reg#registration.name),
  case elsa_service_worker:find(Service) of
    undefined ->
      elsa_service_worker_sup:start_child(Service),
      unregister_instances(Service, Reg#registration.instances);
    _ ->
      unregister_instances(Service, Reg#registration.instances)
  end.

unregister_instances(Service, Instances) ->
  [elsa_service_worker:unregister(Service, I#instance.location) || I <- Instances].

checkout(Service) ->
  lager:info("Checking out instance of service: ~w", [Service]),
  case elsa_service_worker:find(Service) of
    undefined ->
      elsa_service_worker_sup:start_child(Service),
      elsa_service_worker:checkout(Service);
    _ ->
      elsa_service_worker:checkout(Service)
    end.

checkin(Service, Instance) ->
  lager:info("Checking in instance of service: ~w", [Service]),
  case elsa_service_worker:find(Service) of
    undefined ->
      elsa_service_worker_sup:start_child(Service),
      elsa_service_worker:checkin(Service, Instance);
    _ ->
      elsa_service_worker:checkin(Service, Instance)
    end.

service(Service) ->
  #registration{
    name=proplists:get_value(<<"service">>, Service),
    version=proplists:get_value(<<"version">>, Service),
    instances=[instance(I) || I <- proplists:get_value(<<"instances">>, Service)]
  }.

instance(Instance) ->
  #instance{
    location=proplists:get_value(<<"location">>, Instance),
    capacity=capacity(proplists:get_value(<<"capacity">>, Instance))
  }.

capacity(Capacity) when is_binary(Capacity) ->
  infinity;
capacity(Capacity) ->
  Capacity.

get_service_name(Version, Name) ->
  list_to_binary([Version, Name]).
