
-module(elsa_json).

-export([to/1,
         from/1]).

-spec to(any()) -> binary().
to(Term) ->
  jsx:prettify(jsx:encode(Term)).

-spec from(binary()) -> any().
from(JSON) ->
  jsx:decode(JSON).
