
-module(elsa_kernel).

-export([init/3,
         handle/2,
         request/7,
         terminate/3
         ]).

init({tcp, http}, Req, _Opts) ->
  lager:info("Service request received: ~w", [Req]),
  {ok, Req, undefined}.

handle(Req, State) ->
  {Version, Req1} = cowboy_req:binding(version, Req),
  {Service, Req2} =  cowboy_req:binding(service, Req1),
  {Endpoint, Req3} = cowboy_req:path(Req2),
  {Timeout, Req4} = cowboy_req:header(<<"x-elsa-timeout">>, Req3, <<"5000">>),
  {Headers, Req5} = cowboy_req:headers(Req4),
  {Method, Req6} = cowboy_req:method(Req5),
  {ok, Body, Request} = cowboy_req:body(Req6, [{length, infinity}]),
  {Status, RespHeaders, Response} = start(Method,
                                          Service,
                                          Version,
                                          elsa_handler:truncate(Version, Service, Endpoint),
                                          Headers,
                                          Body,
                                          binary_to_integer(Timeout)),
  {ok, elsa_handler:reply(Status, RespHeaders, Response, Request), State}.

start(Method, Service, Version, Endpoint, Headers, Body, Timeout) ->
  lager:info("Request method: ~s, service: ~s, version: ~s, endpoint: ~s created.", [Method, Service, Version, Endpoint]),
  Conn = spawn_link(?MODULE, request, [self(), Method, Service, Version, Headers, Body, Endpoint]),
  receive
    RESPONSE -> RESPONSE
  after Timeout ->
    ID = elsa_task:new(self(), Service, Version, Method, Endpoint),
    Conn ! {timeout, ID},
    elsa_handler:task(Service, Version, ID)
  end.

request(Monitor, Method, Service, Version, Headers, Body, Endpoint) ->
  RESPONSE = call(Method, Service, Version, Headers, Body, Endpoint),
  receive
    {timeout, ID} -> elsa_task:store(ID, RESPONSE)
  after 0 ->
    Monitor ! RESPONSE
  end.

call(Method, Service, Version, Headers, Body, Endpoint) ->
  {URL, Instance} = wait_for_instance(Service, Version, Endpoint),
  case elsa_http_client:call(Method, URL, Headers, Body) of
    {ok, Status, RespHeaders, RespBody} ->
      elsa_service:checkin(Service, Version, Instance),
      {Status, RespHeaders, RespBody};
    retry ->
      elsa_service:checkin(Service, Version, Instance),
      lager:info("Connection to ~s for service: ~s version: ~s method: ~s endpoint: ~s failed", [Instance, Service, Version, Method, URL]),
      call(Method, Service, Version, Headers, Body, Endpoint)
  end.

wait_for_instance(Service, Version, Endpoint) ->
  case elsa_service:checkout(Service, Version) of
    unavailable ->
      timer:sleep(5000),
      wait_for_instance(Service, Version, Endpoint);
    Instance ->
      {validate(<<Instance/binary, Endpoint/binary>>), Instance}
  end.

validate(URL_binary) ->
  URL = binary_to_list(URL_binary),
  case string:sub_string(URL, 1, 4) of
    "http" -> URL;
    _ -> "http://" ++ URL
  end.

terminate(_Reason, Req, State) ->
  ok.
