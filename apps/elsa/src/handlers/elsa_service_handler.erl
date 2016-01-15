
-module(elsa_service_handler).

-export([init/3,
         handle/2,
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
  {ok, Body, Request} = cowboy_req:body(Req6),
  {Status, RespHeaders, Response} = elsa_task_monitor:start(Method,
                                                          Service,
                                                          Version,
                                                          elsa_handler:truncate(Version, Service, Endpoint),
                                                          Headers,
                                                          Body,
                                                          binary_to_integer(Timeout)),
  {ok, elsa_handler:reply(Status, RespHeaders, Response, Request), State}.

terminate(_Reason, _Req, _State) ->
  ok.
