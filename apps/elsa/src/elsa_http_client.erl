
-module(elsa_http_client).

-export([call/4]).

call(Method, Url, Headers, Body) ->
  case request(Method, Url, Headers, Body) of
    {ok, Status, RespHeaders, Client} ->
      {ok, RespBody} = hackney:body(Client),
      {ok, Status, RespHeaders, RespBody};
    {error, Reason} ->
      retry
  end.

request(<<"GET">>, URL, Headers, Body) ->
  hackney:get(URL, Headers, Body, []);
request(<<"POST">>, URL, Headers, Body) ->
  hackney:post(URL, Headers, Body, []);
request(<<"PUT">>, URL, Headers, Body) ->
  hackney:put(URL, Headers, Body, []);
request(<<"PATCH">>, URL, Headers, Body) ->
  hackney:patch(URL, Headers, Body, []);
request(<<"DELETE">>, URL, Headers, Body) ->
  hackney:delete(URL, Headers, Body, []);
request(<<"OPTIONS">>, URL, Headers, Body) ->
  hackney:options(URL, Headers, Body, []).
