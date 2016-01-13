
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @author Eric Sage <eric.david.sage@gmail.com>
% @copyright 2015 Cytoscape Consortium

-module(elsa_service_worker).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-export([start_link/1,
         find/1,
         registration/1,
         register/3,
         unregister/2,
         checkout/1,
         transaction/4,
         checkin/2]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(service_instance, {location, capacity, out}).

start_link(Name) when is_binary(Name) ->
  gen_server:start_link({global, {service_worker, Name}},
                        ?MODULE,
                        Name, []).

init(Name) ->
  case mnesia:create_table(binary_to_atom(Name, utf8),
    [{attributes, record_info(fields, service_instance)},
    {record_name, service_instance},
    {disc_copies, [node()]}]) of
    {atomic, ok} -> lager:info("Service table ~s created.", [Name]);
    {aborted, {already_exists, _}} -> lager:error("Service table ~s already exists.", [Name])
  end,
  lager:info("Service worker ~s created.", [Name]),
  {ok, {service_name, Name}}.

find(Name) when is_binary(Name) ->
  global:whereis_name({service_worker, Name}).

registration(Name) when is_binary(Name) ->
  do(qlc:q([I || I <- mnesia:table(binary_to_atom(Name, utf8))])).

register(Name, Instance, Capacity) when is_binary(Name), is_binary(Instance), is_atom(Capacity); is_integer(Capacity) ->
  gen_server:call(find(Name), {register, Instance, Capacity}).

unregister(Name, Instance) when is_binary(Name), is_binary(Instance) ->
  gen_server:call(find(Name), {unregister, Instance}).

checkout(Name) ->
  gen_server:call(find(Name), checkout).

checkin(Name, Instance) when is_binary(Name), is_binary(Instance) ->
  gen_server:call(find(Name), {checkin, Instance}).

%% Callbacks

handle_call({register, Instance, Capacity}, _From, {service_name, Name}) ->
  Row = #service_instance{location=Instance, capacity=Capacity, out=0},
  {atomic, ok} = transaction(Name, Row, write, write),
  {reply, ok, {service_name, Name}};

handle_call({unregister, Instance}, _From, {service_name, Name}) ->
  {atomic, ok} = transaction(Name, Instance, delete, write),
  {reply, ok, {service_name, Name}};

%%TODO: These two transactions should happen at once.
handle_call({checkin, Instance}, _From, {service_name, Name}) ->
{atomic, [{_, _, C, O}|_]} = transaction(Name, Instance, read, read),
Row = #service_instance{location=Instance, capacity=C, out=O-1},
{atomic, ok} = transaction(Name, Row, write, write),
{reply, ok, {service_name, Name}};

handle_call(checkout, _From, {service_name, Name}) ->
  {reply, case bound(Name) of
    false ->
      case unbound(Name) of
        false -> unavailable;
        Instance -> Instance
      end;
    Instance -> Instance
  end, {service_name, Name}}.

bound(Name) ->
  B = do(qlc:q([I || I <- mnesia:table(binary_to_atom(Name, utf8)),
  I#service_instance.capacity =/= infinity,
  I#service_instance.capacity =/= I#service_instance.out])),
  Sort = fun(X,Y) -> X#service_instance.out < Y#service_instance.out end,
  case lists:sort(Sort, B) of
    [] -> false;
    [Instance|_] -> remove(Name, Instance)
  end.

unbound(Name) ->
  B = do(qlc:q([I || I <- mnesia:table(binary_to_atom(Name, utf8)),
  I#service_instance.capacity == infinity])),
  Sort = fun(X,Y) -> X#service_instance.out < Y#service_instance.out end,
  case lists:sort(Sort, B) of
    [] -> false;
    [Instance|_] -> remove(Name, Instance)
  end.

remove(Name, Instance) ->
  transaction(Name,
  #service_instance{
    location = Instance#service_instance.location,
    capacity = Instance#service_instance.capacity,
    out = Instance#service_instance.out + 1
  }, write, write),
  Instance#service_instance.location.

handle_info(_, Name) -> {noreply, Name}.

handle_cast({_, _}, Name) -> {noreply, Name}.

terminate(_Reason, {service_name, Name}) ->
lager:info("Service worker ~s terminated.", [Name]),
ok.

code_change(_LastVsn, State, _Opt) -> {ok, State}.

%% Helpers

transaction(Name, Target, Action, Mode) ->
  T = fun() ->
    apply(mnesia, Action, [binary_to_atom(Name, utf8), Target, Mode])
  end,
  mnesia:transaction(T).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
