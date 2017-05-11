-module(eql).
-export([ compile/1
        , compile/2
        , compile/3
        , new_tab/1
        , get_query/2
        ]).

compile(File) ->
    case file:read_file(File) of
        {ok, Source} -> eql_compiler:compile(Source);
        Error        -> Error
    end.

compile(Tab, File, []) ->
    compile(Tab, File);
compile(Tab, File, [flush]) ->
    ets:delete_all_objects(Tab),
    compile(Tab, File).

compile(Tab, File) ->
    case compile(File) of
        {ok, Queries} ->
            ets:insert(Tab, Queries),
            ok;
        Error ->
            Error
    end.

new_tab(Name) ->
    ets:new(Name, [named_table, set, {read_concurrency, true}]).

get_query(Name, Tid) when is_reference(Tid)
                        ; is_atom(Tid) ->
    case ets:lookup(Tid, Name) of
        [] ->
            not_found;
        [{_, Query}] ->
            Query
    end;
get_query(Name, Proplist) ->
    case lists:keyfind(Name, 1, Proplist) of
        {Name, Value} -> Value;
        false -> undefined
    end.
