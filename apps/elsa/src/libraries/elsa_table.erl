
-module(elsa_table).

-export([
         create/3,
         clear/1,
         action/4,
         do/1]).

-include_lib("stdlib/include/qlc.hrl").

create(Table, Record, Info) ->
  case mnesia:create_table(Table,
    [{attributes, Info},
    {record_name, Record},
    {disc_copies, [node()]}]) of
    {atomic, ok} -> ok;
    {aborted, {already_exists, _}} -> exists
  end.

  clear(Table) ->
    case mnesia:clear_table(Table) of
      {atomic, ok} -> ok;
      {aborted, _} -> aborted
    end.

action(Table, Target, Action, Mode) ->
  T = fun() ->
    apply(mnesia, Action, [Table, Target, Mode])
  end,
  {atomic, Value} = mnesia:transaction(T),
  Value.

do(QLC) ->
    F = fun() -> qlc:e(QLC) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
