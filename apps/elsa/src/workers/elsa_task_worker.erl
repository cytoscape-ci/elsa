
-module(elsa_task_worker).

-behaviour(gen_server).

-export([start_link/1,
         find/1,
         get_params/1,
         store_params/2,
         get_data/1,
         store_data/2
         ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(task, {params, data}).

start_link(Task) when is_pid(Task) ->
  ID = elsa_task:get_id(Task),
  gen_server:start_link({global, {task_worker, ID}},
                        ?MODULE,
                        ID, []).

find(ID) when is_binary(ID) ->
  global:whereis_name({task_worker, ID}).

get_params(ID) ->
  gen_server:call(find(ID), params).

store_params(ID, Params) ->
  gen_server:call(find(ID), {params, Params}).

get_data(ID) ->
  gen_server:call(find(ID), data).

store_data(ID, Data) ->
  gen_server:call(find(ID), {data, Data}).

init(ID) ->
  lager:info("Task ~s created.", [ID]),
  {ok, {task, ID, #task{params=none, data=incomplete}}}.

handle_call(params, _From, {task, ID, Task}) ->
  {reply, Task#task.params, {task, ID, Task}};

handle_call({params, Params}, _From, {task, ID, Task}) ->
  {reply, ok, {task, ID, Task#task{params = Params}}};

handle_call(data, _From, {task, ID, Task}) ->
  {reply, Task#task.data, {task, ID, Task}};

handle_call({data, Data}, _From, {task, ID, Task}) ->
  {reply, ok, {task, ID, Task#task{data = Data}}}.

handle_info(_, Name) -> {noreply, Name}.

handle_cast({_, _}, Name) -> {noreply, Name}.

terminate(_Reason, {task, ID, _}) ->
lager:notice("Task worker ~s terminated.", [ID]),
ok.

code_change(_LastVsn, State, _Opt) -> {ok, State}.
