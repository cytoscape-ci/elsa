-module(elsa_task_database).

-export([
         load/0,
         clear/0,
         create/1,
         store/2,
         info/1,
         retreive/1,
         delete/1,
         all/0,
         complete/0,
         incomplete/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(task, {id,
               completed,
               status,
               headers,
               response}).

load() ->
  Table = elsa_tasks,
  case mnesia:create_table(Table,
    [{attributes, record_info(fields, task)},
    {record_name, task},
    {disc_copies, [node()]}]) of
    {atomic, ok} -> lager:info("Task table created.");
    {aborted, {already_exists, _}} -> lager:error("Task table already exists.")
  end,
  ok.

clear() ->
  case mnesia:clear_table(elsa_tasks) of
    {aborted, _} -> lager:info("Task table could not be cleared.");
    {atomic, ok} -> lager:info("Task table cleared.")
  end.

create(ID) ->
  Row = #task{id=ID,
              completed=false,
              status= <<"Not ready">>,
              headers = <<"Not ready">>,
              response = <<"Not ready">>},
  {atomic, ok} = transaction(Row, write, write),
  ok.

store(ID, {Status, Headers, Response}) ->
  Row = #task{id=ID, completed=true, status=Status, headers=Headers, response=Response},
  {atomic, ok} = transaction(Row, write, write),
  ok.

info(ID) ->
  case retreive(ID) of
    {task, ID, Completed, _, _, _} -> {completed, Completed};
    not_found -> not_found
  end.

retreive(ID) ->
  case transaction(ID, read, read) of
    {atomic, [Task]} -> Task;
    {atomic, []} -> not_found
  end.

delete(ID) ->
  {atomic, ok} = transaction(ID, delete, write),
  ok.

all() ->
  do(qlc:q([{T#task.id, T#task.completed} || T <- mnesia:table(elsa_tasks)])).

complete() ->
  tasks(true).

incomplete() ->
  tasks(false).

tasks(Complete) ->
  do(qlc:q([T#task.id || T <- mnesia:table(elsa_tasks), T#task.completed == Complete])).

transaction(Target, Action, Mode) ->
  T = fun() ->
    apply(mnesia, Action, [elsa_tasks, Target, Mode])
  end,
  mnesia:transaction(T).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
