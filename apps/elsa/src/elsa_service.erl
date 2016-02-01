
-module(elsa_service).

-export([all/0,
         versions/1,
         instances/2,
         instance/3,
         register/1,
         unregister/1,
         checkout/2,
         checkin/3,
         exists/1,
         exists/2,
         existed/2]).

-spec all() -> [] | [any()].
all() ->
  elsa_service_database:all().

-spec versions(binary()) -> [] | [any()].
versions(Service) when is_binary(Service) ->
  elsa_service_database:versions(Service).

-spec instances(binary(), binary()) -> [] | [any()].
instances(Service, Version) ->
  case existed(Service, Version) of
    false -> [];
    true -> elsa_instance_database:all(Service, Version)
  end.

instance(Service, Version, Location) ->
  elsa_instance_database:retreive(Service, Version, Location).

register(Registration) ->
  elsa_registry:register(Registration).

unregister(Registration) ->
  elsa_registry:unregister(Registration).

checkout(Service, Version) ->
  ensure_service(Service, Version),
  elsa_service_worker:checkout(Service, Version).

checkin(Service, Version, Instance) ->
  lager:info("Checking in instance of service: ~s", [Service]),
  ensure_service(Service, Version),
  elsa_service_worker:checkin(Service, Version, Instance).

ensure_service(Service, Version) ->
  case exists(Service, Version) of
     false -> elsa_service_worker_sup:start_child(Service, Version);
     true -> ok
  end.

exists(Service) ->
  case length(elsa_service_database:versions(Service)) of
     0 -> false;
     _ -> true
  end.

exists(Service, Version) ->
  case elsa_service_worker:find(Service, Version) of
     undefined -> false;
     Pid -> true
  end.

existed(Service, Version) ->
  case mnesia:table_info(elsa_tasks, storage_type) of
    {aborted, {no_exists, _, _}} -> false;
    _ -> true
  end.
