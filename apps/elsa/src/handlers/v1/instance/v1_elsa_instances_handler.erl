
-module(v1_elsa_instances_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         json_response/2]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {Service, Request} = cowboy_req:binding(service_name, Req),
  {Version, Request2} = cowboy_req:binding(version, Request),
  {ok, Request2, {instance, Service, Version}}.

allowed_methods(Req, Instance) ->
  {[<<"GET">>], Req, Instance}.

content_types_provided(Req, Instance) ->
  {[{<<"application/json">>, json_response}], Req, Instance}.

resource_exists(Req, {instance, Service, Version}) ->
  {elsa_service:existed(Service, Version), Req, {instance, Service, Version}}.

json_response(Req, {instance, Service, Version}) ->
  Instances = elsa_json:to([[
    {<<"location">>, Location},
    {<<"capacity">>, Capacity},
    {<<"total_checked_out">>, Out}
  ] || {instance, Location, Registered, Capacity, Out} <- elsa_service:instances(Service, Version)]),
  {Instances, Req, {instance, Service, Version}}.
