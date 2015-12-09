-module(eql_config).
-export([load/2]).

load(Env, Config) ->
    parse(scan(proplists:get_value(Env, Config))).

scan(undefined) ->
    undefined;
scan(String) ->
    erl_scan:string(String).

parse({ok, Tokens, _}) ->
    erl_parse:parse_term(Tokens);
parse(_) ->
    undefined.
