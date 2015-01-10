-module(eql_compiler).
-export([compile/1]).

compile(Source) when is_binary(Source) ->
    compile(binary_to_list(Source));
compile(Source) when is_list(Source) ->
    parse(tokenize(Source)).

%%
%% Internal
%%

tokenize(Source) ->
    eql_scanner:scan(Source).

parse({ok, Tokens, _}) ->
    case eql_parser:parse(Tokens) of
        {ok, Code} -> {ok, sanitize(Code, [])};
        {error, {_, _, Error}} -> {error, lists:flatten(Error)}
    end.

sanitize([], Acc) ->
    Acc;
sanitize([{query_scope, Scope}|T], Acc) ->
    sanitize(T, [query_from_scope(Scope) | Acc]);
sanitize(_, Acc) ->
    Acc.

query_from_scope({{name, _, Name}, Queries}) ->
    {Name, join_queries(Queries)}.

join_queries(Queries) ->
    lists:flatten(lists:reverse(join_queries(Queries, []))).
join_queries([], Acc) ->
    Acc;
join_queries([{query, _, Content}|T], []) ->
    join_queries(T, [Content]);
join_queries([{query, _, Content}|T], Acc) ->
    join_queries(T, [Content | [" " | Acc]]).
