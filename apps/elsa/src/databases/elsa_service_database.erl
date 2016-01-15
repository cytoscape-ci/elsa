
-module(elsa_service_database).

-export([
         load_service/2,
         store_instance/4,
         remove_instance/3,
         checkout_instance/2,
         checkin_instance/3]).

-include_lib("stdlib/include/qlc.hrl").

-record(service_instance, {location, capacity, out}).

load_service(Service, Version) ->
  Table = table_id(Service, Version),
  case mnesia:create_table(Table,
    [{attributes, record_info(fields, service_instance)},
    {record_name, service_instance},
    {disc_copies, [node()]}]) of
    {atomic, ok} -> lager:info("Service table ~s created.", [Table]);
    {aborted, {already_exists, _}} -> lager:error("Service table ~s already exists.", [Table])
  end,
  ok.

store_instance(Service, Version, Instance, Capacity) ->
  Row = #service_instance{location=Instance, capacity=Capacity, out=0},
  {atomic, ok} = transaction(table_id(Service, Version), Row, write, write).

remove_instance(Service, Version, Instance) ->
  {atomic, ok} = transaction(table_id(Service, Version), Instance, delete, write).

checkout_instance(Service, Version) ->
  case bound(table_id(Service, Version)) of
    false ->
      case unbound(table_id(Service, Version)) of
        false -> unavailable;
        Instance -> Instance
      end;
    Instance -> Instance
  end.

checkin_instance(Service, Version, Instance) ->
  Table = table_id(Service, Version),
  {atomic, [{_, _, C, O}|_]} = transaction(Table, Instance, read, read),
  Row = #service_instance{location=Instance, capacity=C, out=O-1},
  {atomic, ok} = transaction(Table, Row, write, write).


transaction(ID, Target, Action, Mode) ->
  T = fun() ->
    apply(mnesia, Action, [ID, Target, Mode])
  end,
  mnesia:transaction(T).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% HELPERS

bound(ID) when is_atom(ID) ->
  B = do(qlc:q([I || I <- mnesia:table(ID),
  I#service_instance.capacity =/= infinity,
  I#service_instance.capacity =/= I#service_instance.out])),
  Sort = fun(X,Y) -> X#service_instance.out < Y#service_instance.out end,
  case lists:sort(Sort, B) of
    [] -> false;
    [Instance|_] -> remove(ID, Instance)
  end.

unbound(ID) when is_atom(ID) ->
  B = do(qlc:q([I || I <- mnesia:table(ID),
  I#service_instance.capacity == infinity])),
  Sort = fun(X,Y) -> X#service_instance.out < Y#service_instance.out end,
  case lists:sort(Sort, B) of
    [] -> false;
    [Instance|_] -> remove(ID, Instance)
  end.

remove(ID, Instance) when is_atom(ID) ->
  transaction(ID,
  #service_instance{
    location = Instance#service_instance.location,
    capacity = Instance#service_instance.capacity,
    out = Instance#service_instance.out + 1
  }, write, write),
  Instance#service_instance.location.

table_id(Service, Version) -> binary_to_atom(<< Service/binary, <<"_">>/binary, Version/binary >>, utf8).
