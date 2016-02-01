-module(elsa_task_database).

-export([
         load/0,
         clear/0,
         create/5,
         store/2,
         info/1,
         retreive/1,
         delete/1,
         all/0,
         complete/0,
         incomplete/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(task, {id :: binary(),
               completed :: atom(),
               service :: binary(),
               version :: binary(),
               method :: binary(),
               endpoint :: binary(),
               created :: {integer(), integer(), integer()},
               status :: binary() | any(),
               headers :: binary() | any(),
               response :: binary() | any()
}).

-spec load() -> ok.
load() ->
  case elsa_table:create(elsa_tasks, task, record_info(fields, task)) of
    ok -> lager:info("Task table created.");
    exists -> lager:error("Task table already exists.")
  end,
  ok.

-spec clear() -> ok.
clear() ->
  case elsa_table:clear(elsa_tasks) of
    aborted -> lager:info("Task table could not be cleared.");
    ok -> lager:info("Task table cleared.")
  end,
  ok.

-spec create(binary(), binary(), binary(), binary(), binary()) -> ok.
create(ID, Service, Version, Method, <<"">>) ->
  create(ID, Service, Version, Method, <<"/">>);
create(ID, Service, Version, Method, Endpoint) ->
  Row = #task{id=ID,
              completed=false,
              service=Service,
              version=Version,
              method=Method,
              endpoint=Endpoint,
              created=os:timestamp(),
              status= <<"Not ready">>,
              headers = <<"Not ready">>,
              response = <<"Not ready">>},
  elsa_table:action(elsa_tasks, Row, write, write).

-spec store(binary(), {any(), any(), any()}) -> ok | not_found.
store(ID, {Status, Headers, Response}) ->
  case info(ID) of
    not_found -> not_found;
    _ ->
      {task, I, _, S, V, M, E, C, _, _, _} = retreive(ID),
      Row = #task{id=I,
                  completed=true,
                  service=S,
                  version=V,
                  method=M,
                  endpoint=E,
                  created=C,
                  status=Status,
                  headers=Headers,
                  response=Response},
      elsa_table:action(elsa_tasks, Row, write, write)
  end.

-spec info(binary()) -> not_found | {completed, true | false}.
info(ID) ->
  case retreive(ID) of
    {task, ID, Completed, _, _, _, _, _, _, _, _} -> {completed, Completed};
    not_found -> not_found
  end.

-spec retreive(binary()) -> not_found | #task{}.
retreive(ID) ->
  case elsa_table:action(elsa_tasks, ID, read, read) of
    [Task] -> Task;
    [] -> not_found
  end.

-spec delete(binary()) -> ok.
delete(ID) ->
  elsa_table:action(elsa_tasks, ID, delete, write).

-spec all() -> [] | [#task{}].
all() ->
  elsa_table:do(qlc:q([{T#task.id,
                        T#task.completed,
                        T#task.service,
                        T#task.version,
                        T#task.method,
                        T#task.endpoint} || T <- mnesia:table(elsa_tasks)])).

-spec complete() -> [] | [#task{}].
complete() ->
  tasks(true).

-spec incomplete() -> [] | [#task{}].
incomplete() ->
  tasks(false).

tasks(Complete) ->
  elsa_table:do(qlc:q([T#task.id || T <- mnesia:table(elsa_tasks), T#task.completed == Complete])).
