-module(eql).
-export([ compile/1
        , get_query/2
        ]).

compile(File) ->
    {ok, Source} = file:read_file(File),
    eql_compiler:compile(Source).

get_query(Name, Queries) ->
    proplists:get_value(Name, Queries).
