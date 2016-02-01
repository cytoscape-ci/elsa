

-module(v1_elsa_services_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         json_request/2,
         delete_resource/2,
         content_types_provided/2,
         json_response/2]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {ok, Req, service}.

allowed_methods(Req, service) ->
  {[<<"GET">>,<<"POST">>,<<"PUT">>,<<"DELETE">>], Req, service}.

content_types_accepted(Req, service) ->
  {[{<<"application/json">>, json_request}], Req, service}.

json_request(Req, service) ->
  {Request, Body} = elsa_handler:from_json(Req),
  elsa_service:register(Body),
  {true, Request, service}.

delete_resource(Req, service) ->
  {Request, Body} = elsa_handler:from_json(Req),
  elsa_service:unregister(Body),
  {true, Request, service}.

content_types_provided(Req, service) ->
  {[{<<"application/json">>, json_response}], Req, service}.

json_response(Req, service) ->
  Services = elsa_json:to([[
    {<<"service">>, Service},
    {<<"version">>, Version},
    {<<"instances">>, Instances},
    {<<"bound_capacity">>, Bound},
    {<<"unbound_capacity">>, Unbound},
    {<<"total_checked_out">>, Out}
  ] || {service, {Service, Version}, Timestamp, Instances, {Bound, Unbound}, Out} <- elsa_service:all()]),
  {Services, Req, service}.
