
-module(elsa_task_monitor).

-export([start/7,
         request/7
        ]).

start(Method, Service, Version, Endpoint, Headers, Body, Timeout) ->
  lager:info("Request method: ~s, service: ~s, version: ~s, endpoint: ~s created.", [Method, Service, Version, Endpoint]),
  Conn = spawn_link(?MODULE, request, [self(), Method, Service, Version, Headers, Body, Endpoint]),
  receive
    RESPONSE -> RESPONSE
  after Timeout ->
    elsa_task_worker_sup:start_child(self()),
    Conn ! {timeout, self()},
    elsa_handler:task(Service, Version, elsa_task:get_id(self()))
  end.

request(Monitor, Method, Service, Version, Headers, Body, Endpoint) ->
  RESPONSE = call(Method, Service, Version, Headers, Body, Endpoint),
  receive
    {timeout, PID} -> elsa_task:store_data(elsa_task:get_id(PID), RESPONSE)
  after 0 ->
    Monitor ! RESPONSE
  end.

call(Method, Service, Version, Headers, Body, Endpoint) ->
  {URL, Instance} = wait_for_instance(Service, Version, Endpoint),
  case elsa_http_client:call(Method, URL, Headers, Body) of
    {ok, Status, RespHeaders, RespBody} ->
      elsa_registry:checkin(Service, Version, Instance),
      {Status, Headers, RespBody};
    retry ->
      elsa_registry:checkin(Service, Version, Instance),
      call(Method, Service, Version, Headers, Body, Endpoint)
  end.

wait_for_instance(Service, Version, Endpoint) ->
  case elsa_registry:checkout(Service, Version) of
    unavailable ->
      timer:sleep(5000),
      wait_for_instance(Service, Version, Endpoint);
    Instance ->
      {validate(concat(Instance, Endpoint)), Instance}
  end.

concat(X, Y) ->
  <<X/binary, Y/binary>>.

validate(URL_binary) ->
  URL = binary_to_list(URL_binary),
  case string:sub_string(URL, 1, 4) of
    "http" -> URL;
    _ -> "http://" ++ URL
  end.
