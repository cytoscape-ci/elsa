
-module(elsa_task_monitor).

-export([start/6,
         request/5
        ]).

start(Method, Name, Version, Endpoint, Body, Timeout) ->
  lager:info("Request method: ~s, service: ~s, version: ~s, endpoint: ~s created.", [Method, Name, Version, Endpoint]),
  Service = concat(Version, Name),
  timeout(Method, Service, Endpoint, Body, Timeout).

timeout(Method, Service, Endpoint, Body, Timeout) ->
  Conn = spawn_link(?MODULE, request, [self(), Method, Service, Body, Endpoint]),
  receive
    Msg -> Msg,
    {200, Msg}
  after Timeout ->
    elsa_task_worker_sup:start_child(self()),
    Conn ! {timeout, self()},
    {300, concat(<<"http://localhost:8080/task/">>, elsa_task:get_id(self()))}
  end.

request(Monitor, Method, Service, Body, Endpoint) ->
  Instance = wait_for_instance(Service),
  URL = validate(concat(Instance, Endpoint)),
  _Header = [],
  Type = "application/json",
  DATA = request(Method, URL, Body, Type),
  receive
    {timeout, PID} -> elsa_task:store_data(elsa_task:get_id(PID), DATA)
  after 0 ->
    Monitor ! DATA,
    elsa_registry:checkin(Service, Instance)
  end.

request(Method, URL, Body, Type) ->
  {ok, {{_, _, _},_, DATA}} = case Method of
    get ->
      httpc:request(Method, {URL, []}, [], []);
    _ ->
      httpc:request(Method, {URL, [], Type, Body}, [], [])
  end,
  list_to_binary(DATA).

wait_for_instance(Service) ->
  case elsa_registry:checkout(Service) of
    unavailable ->
      timer:sleep(5000),
      wait_for_instance(Service);
    Instance ->
      Instance
  end.

concat(X, Y) ->
  <<X/binary, Y/binary>>.

validate(URL_binary) ->
  URL = binary_to_list(URL_binary),
  case string:sub_string(URL, 1, 4) of
    "http" -> URL;
    _ -> "http://" ++ URL
  end.
