
-module(elsa_agent_monitor).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).

init(Agent) ->
lager:info("Agent ~s initialized", [Agent]),
{ok, Agent}.

handle_event({service_registered, Registration}, Agent) ->
  lager:info("Agent handler ~s received register broadcast", [Agent]),
  update(<<"POST">>, Agent, Registration),
  lager:info("Agent ~s sent registration", [Agent]),
  {ok, Agent};

handle_event({service_unregistered, Registration}, Agent) ->
  lager:info("Agent handler ~s received unregister broadcast", [Agent]),
  update(<<"DELETE">>, Agent, Registration),
  lager:info("Agent ~s sent unregistration", [Agent]),
  {ok, Agent}.

handle_call(_, Agent) ->
  {ok, ok, Agent}.

handle_info(_, Agent) ->
  {ok, Agent}.

terminate(_Args, _State) ->
  ok.

code_change(_LastVsn, State, _Opt) -> {ok, State}.

update(Method, Agent, Registration) ->
  elsa_http_client:call(Method,
                        registration_url(Agent),
                        [{<<"content-type">>,<<"application/json">>}],
                        elsa_handler:to_json(Registration)).

modify_instances(Registration) ->
  lists:keyreplace(<<"instances">>,
                   1,
                   Registration,
                   {<<"instances">>, [
                    {<<"location">>, <<"location">>},
                    {<<"capacity">>, <<"infinity">>}
                   ]}).

registration_url(Agent) ->
  erlang:iolist_to_binary([Agent, <<"/registration">>]).
