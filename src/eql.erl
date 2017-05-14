-module(eql).
-export([ compile/1
        , compile/2
        , compile/3
        , new_tab/1
        , get_query/2
        , get_query/3
        ]).

%% a query is either a binary string or a list of binary strings and atoms
%% the atoms are replaced during `get_query` with the provided parameters
%% resulting in an iolist
-type query() :: binary() | [binary() | atom()].
-type query_list() :: [{atom(), query()}].

%% returns a list of
-spec compile(file:filename_all()) -> {ok, query_list()} | any().
compile(File) ->
    eql_parse:file(File).

compile(Tab, File, []) ->
    compile(Tab, File);
compile(Tab, File, [flush]) ->
    ets:delete_all_objects(Tab),
    compile(Tab, File).

compile(Tab, File) ->
    case compile(File) of
        {ok, Queries} when is_list(Queries) ->
            ets:insert(Tab, Queries),
            ok;
        Error ->
            Error
    end.

new_tab(Name) ->
    ets:new(Name, [named_table, set, {read_concurrency, true}]).

-spec get_query(atom(), query_list() | ets:tid(), [iolist()]) -> {ok, iolist()} | undefined.
get_query(Name, TidOrList, Params) ->
    case get_query(Name, TidOrList) of
        {ok, Query} ->
            {ok, lists:map(fun(Key) when is_atom(Key) ->
                               proplists:get_value(Key, Params);
                              (S) ->
                               S
                           end, Query)};
        Other ->
            Other
    end.

-spec get_query(atom(), query_list() | ets:tid()) -> {ok, iolist()} | undefined.
get_query(Name, Tid) when is_reference(Tid)
                        ; is_atom(Tid) ->
    case ets:lookup(Tid, Name) of
        [] ->
            undefined;
        [{_, Query}] ->
            {ok, Query}
    end;
get_query(Name, Proplist) ->
    case lists:keyfind(Name, 1, Proplist) of
        {Name, Value} -> {ok, Value};
        false -> undefined
    end.
