
-module(elsa_registration_handler).

-export([init/3,
         handle/2,
         terminate/3
         ]).

init({tcp, http}, Req, State) ->
  lager:info("Received registration request: ~p", [Req]),
  {ok, Req, State}.

handle(Req, State) ->
  process(Req, State).

terminate(_Reason, _Req, _State) ->
  ok.

%% Validate and Process the Update.

process(Req, State) ->
  validate(Req, State, elsa_handler:check_content_type(Req)).

validate(Req, State, valid_content_type) ->
  validate(Req, State, elsa_handler:has_body(Req));
validate(Req, State, invalid_content_type) ->
  lager:error("Client sent bad content type in request: ~p", [Req]),
  {ok, elsa_handler:reply(400, <<"Content type must be application/json">>, Req), State};

validate(Req, State, true) ->
  validate(Req, State, elsa_handler:check_json(Req));
validate(Req, State, false) ->
  lager:error("Client sent empty registration request: ~p", [Req]),
  {ok, elsa_handler:reply(400, <<"Missing Body">>, Req), State};

validate(Req, State, valid_json) ->
  validate(Req, State, elsa_handler:schema(Req));
validate(Req, State, invalid_json) ->
  lager:info("Client sent invalid JSON in registration request: ~p", [Req]),
  {ok, elsa_handler:reply(422, <<"Not valid JSON">>, Req), State};

validate(Req, State, valid_schema) ->
  update(Req, State);
validate(Req, State, invalid_schema) ->
  lager:info("Client sent an invalid registration format in registration request: ~p", [Req]),
  {ok, elsa_handler:reply(422, <<"Schema does not match">>, Req), State}.

update(Req, State) ->
  {Request, Body} = elsa_handler:from_json(Req),
  case cowboy_req:method(Request) of
    {<<"POST">>, _} ->
        elsa_registry:register(Body);
    {<<"DELETE">>, _} ->
      elsa_registry:unregister(Body)
  end,
  {ok, elsa_handler:reply(200, <<"OK.">>, Request), State}.
