
-module(elsa_agent_manager).

-export([start_link/0,
         add_agent/1,
         remove_agent/1,
         service_registered/1,
         service_unregistered/1]).

start_link() ->
  lager:info("Elsa agent manager started."),
  gen_event:start_link({global, agent_manager}).

add_agent(Agent) ->
  lager:info("Agent manager added agent: ~s", [Agent]),
  gen_event:add_handler({global, agent_manager}, elsa_agent_monitor, [Agent]).

remove_agent(Agent) ->
  lager:info("Agent manager removed agent: ~s", [Agent]),
  gen_event:delete_handler({global, agent_manager}, elsa_agent_monitor, [Agent]).

service_registered(Registration) ->
  lager:info("Boadcasting service unregistered: ~p", [Registration]),
  gen_event:notify({global, agent_manager}, {service_registered, Registration}).

service_unregistered(Registration) ->
  lager:info("Broadcasting service unregistered: ~p", [Registration]),
  gen_event:notify({global, agent_manager}, {service_unregistered, Registration}).
