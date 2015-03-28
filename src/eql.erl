-module(eql).
-export([ compile/1
        , get_query/2
        ]).

compile(File) ->
    case file:read_file(File) of
        {ok, Source} -> eql_compiler:compile(Source);
        Error        -> Error
    end.

get_query(Name, Proplist) ->
    case lists:keyfind(Name, 1, Proplist) of
        {Name, Value} -> Value;
        false -> undefined
    end.
