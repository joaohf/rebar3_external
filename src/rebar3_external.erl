-module(rebar3_external).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, rebar_state:add_resource(State, {external, rebar_external_resource})}.
