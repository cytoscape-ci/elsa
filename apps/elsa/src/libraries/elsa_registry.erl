
-module(elsa_registry).

-export([register/1,
         unregister/1]).

-record(registration, {service, version, instances}).
-record(instance, {location, capacity}).

process(Type, Registration) ->
  catch service(Type, Registration).

register(Registration) ->
  process(register, Registration).

unregister(Registration) ->
  process(unregister, Registration).

parse(Registration) ->
  R = service(Registration),
  {Service, Version} = {R#registration.service, R#registration.version},
  {Service, Version, R}.

service(Type, [Service|_] = Registration) when is_list(Service) ->
  [service(Type, S) || S <- Registration];

service(Type, Registration) ->
  {Service, Version, R} = parse(Registration),
  instances(Type, Service, Version, R).

instances(Type, Service, Version, R) ->
  ensure_service(Service, Version),
  case Type of
    register ->
     [elsa_service_worker:register(Service, Version, I#instance.location, I#instance.capacity) || I <- R#registration.instances],
     elsa_agent_manager:service_registered(R);
    unregister ->
      [elsa_service_worker:unregister(Service, Version, I#instance.location) || I <- R#registration.instances],
      elsa_agent_manager:service_unregistered(R)
  end.

service(Service) ->
  #registration{
    service=extract(<<"service">>, Service),
    version=extract(<<"version">>, Service),
    instances=[instance(I) || I <- extract(<<"instances">>, Service)]
  }.

instance(Instance) ->
  #instance{
    location=extract(<<"location">>, Instance),
    capacity=capacity(extract(<<"capacity">>, Instance))
  }.

extract(Prop, Type) -> proplists:get_value(Prop, Type, {missing, Prop}).

capacity(<<"infinity">>) -> infinity;
capacity(Capacity) when is_binary(Capacity) -> binary_to_integer(Capacity);
capacity(Capacity) when is_integer(Capacity) -> Capacity.

ensure_service(Service, Version) ->
  case elsa_service_worker:find(Service, Version) of
     undefined -> elsa_service_worker_sup:start_child(Service, Version);
     _ -> ok
  end.
