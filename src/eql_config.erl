-module(eql_config).

-export([load/2, load/3]).

load(Env, Config) ->
    case eql:get_query(Env,Config) of
        {ok,ResolvedConfig} ->
        case is_bitstring(ResolvedConfig) of
            true -> parse(scan(ResolvedConfig));
            false -> {error,"Missing variables"}
        end;
        {error,Error} -> {error,Error}
    end.

load(Env, Config,Variables) ->
    case eql:get_query(Env,Config,Variables) of

        {ok,ResolvedConfig} ->
            ScanResult = scan(ResolvedConfig),
            parse(ScanResult);
        {error,Error} -> {error,Error}
    end.

scan(undefined) ->
    undefined;
scan(IoList) ->
    erl_scan:string(lists:flatten(io_lib:format("~s", [IoList]))).

parse({ok, Tokens, _}) ->
    erl_parse:parse_term(Tokens);
parse(_) ->
    undefined.
