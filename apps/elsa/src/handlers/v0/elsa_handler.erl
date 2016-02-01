
-module(elsa_handler).

-export([error/3,
         message/3,
         task/3,
         reply/4,
         from_json/1,
         check_content_type/1,
         to_json/1,
         has_body/1,
         check_json/1,
         headers_to_binary_headers/1,
         schema/1,
         truncate/3
         ]).

error(Code, Message, Req) ->
  R = [
  {<<"status">>, Code},
  {<<"reason">>, Message}
  ],
 json_reply(Code, R, Req).

 message(Code, Message, Req) ->
   R = [
   {<<"status">>, Code},
   {<<"message">>, Message}
   ],
  json_reply(Code, R, Req).

task(Service, Version, ID) ->
  {300,
    [{<<"content-type">>, <<"application/json">>}],
    elsa_handler:to_json([
      {<<"status">>, 300},
      {<<"service">>, Service},
      {<<"version">>, Version},
      {<<"task_id">>, ID}
    ])
  }.

 json_reply(Code, Response, Req) ->
   {ok, Reply} = cowboy_req:reply(Code,
    [{<<"content-type">>, <<"application/json">>}],
    to_json(Response),
    Req),
   Reply.

 reply(Code, Headers, Body, Req) ->
   {ok, Reply} = cowboy_req:reply(Code, Headers, Body, Req),
   Reply.

from_json(Request) ->
  {ok, JSON, Req} = cowboy_req:body(Request),
  Body = try jsx:decode(JSON)
    catch error:badarg -> invalid_json
  end,
  {Req, Body}.

check_content_type(Request) ->
  {Header, _} = cowboy_req:header(<<"content-type">>, Request),
  case Header of
    <<"application/json">> -> valid_content_type;
    _ -> invalid_content_type
  end.

check_json(Request) ->
  {_Req, Body} = from_json(Request),
  case Body of
    invalid_json -> invalid_json;
    _ -> valid_json
  end.

to_json(Response) ->
  jsx:encode(Response).

has_body(_Req) ->
  true.

headers_to_binary_headers(Headers) ->
  [{list_to_binary(K), list_to_binary(V)} || {K,V} <- Headers].

%% HERE IS WHERE WE CAN ADD SCHEMA VALIDATION.
schema(_Req) ->
  valid_schema.

truncate(Version, Name, Endpoint) ->
  Vlength = length(binary_to_list(Version)),
  Nlength = length(binary_to_list(Name)),
  list_to_binary(string:sub_string(binary_to_list(Endpoint), (3 + Vlength + Nlength))).
