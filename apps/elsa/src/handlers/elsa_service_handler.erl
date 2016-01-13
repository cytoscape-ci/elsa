
-module(elsa_service_handler).

-export([init/3,
         handle/2,
         terminate/3
         ]).

init({tcp, http}, Req, _Opts) ->
  lager:info("Service request received: ~p", [Req]),
  {ok, Req, undefined}.

handle(Req, State) ->
  {Version, Req1} = cowboy_req:binding(version, Req),
  {Name, Req2} =  cowboy_req:binding(service, Req1),
  {Endpoint, Req3} = cowboy_req:path(Req2),
  {Timeout, Req4} = cowboy_req:header(<<"x-elsa-timeout">>, Req3, <<"5000">>),
  {Method, Req5} = cowboy_req:method(Req4),
  {Body, Request} = body(Req5),
  {Status, Response} = elsa_task_monitor:start(convert(Method), Name, Version, truncate(Version, Name, Endpoint), Body, binary_to_integer(Timeout)),
  {ok, elsa_handler:reply(Status, Response, Request), State}.

terminate(_Reason, _Req, _State) ->
  ok.

body(Request) ->
  case elsa_handler:has_body(Request) of
    true ->
      {ok, JSON, Req} = cowboy_req:body(Request),
      {JSON, Req};
    false ->
      {"", Request}
  end.

truncate(Version, Name, Endpoint) ->
  Vlength = length(binary_to_list(Version)),
  Nlength = length(binary_to_list(Name)),
  list_to_binary(string:sub_string(binary_to_list(Endpoint), (3 + Vlength + Nlength))).

convert(<<"GET">>) ->
  get;
convert(<<"POST">>) ->
  post;
convert(<<"PUT">>) ->
  put;
convert(<<"DELETE">>) ->
  delete;
convert(<<"PATCH">>) ->
  patch;
convert(<<"OPTIONS">>) ->
  options.
