

-module(v1_elsa_service_handler).

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
  {ok, Request, {service, Service}}.

allowed_methods(Req, Service) ->
  {[<<"GET">>], Req, Service}.

content_types_provided(Req, Service) ->
  {[{<<"application/json">>, json_response}], Req, Service}.

resource_exists(Req, {service, Service}) ->
  {elsa_service:exists(Service), Req, {service, Service}}.

json_response(Req, {service, Service}) ->
  S = elsa_json:to([
    {<<"service">>, Service},
    {<<"versions">>, [[
      {<<"version">>, Version},
      {<<"instances">>, Instances},
      {<<"bound_capacity">>, Bound},
      {<<"unbound_capacity">>, Unbound},
      {<<"total_checked_out">>, Out}
    ] || {service, {Service, Version}, Timestamp, Instances, {Bound, Unbound}, Out} <- elsa_service:versions(Service)]}]),
  {S, Req, {service, Service}}.
