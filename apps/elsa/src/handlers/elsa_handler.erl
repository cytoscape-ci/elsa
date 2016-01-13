
-module(elsa_handler).

-export([reply/3,
         from_json/1,
         to_json/1,
         has_body/1,
         check_json/1,
         schema/1
         ]).

reply(Code, Body, Req) ->
 {ok, Reply} = cowboy_req:reply(Code, [], to_json(Body), Req),
 Reply.

from_json(Request) ->
  {ok, JSON, Req} = cowboy_req:body(Request),
  Body = try jsx:decode(JSON)
    catch error:badarg -> invalid_json
  end,
  {Req, Body}.

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

%% HERE IS WHERE WE CAN ADD SCHEMA VALIDATION.
schema(_Req) ->
  valid_schema.
