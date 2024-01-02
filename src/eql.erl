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

-type compile_opt() :: flush | %% delete all (or for this namespace) queries first
                       namespace. %% store queries in ets table keyed on a 2-tuple of
                                  %% the filename as an atom and the name of the query

%% returns a list of
-spec compile(file:filename_all()) -> {ok, query_list()} | any().
compile(File) ->
    eql_parse:file(File).

compile(Tab, File) ->
    compile(Tab, File, []).

-spec compile(atom() | ets:tid(), file:filename_all(), [compile_opt()]) -> ok | any().
compile(Tab, File, Opts) ->
    maybe_flush(Tab, File, Opts),
    case compile(File) of
        {ok, Queries} when is_list(Queries) ->
            case proplists:get_value(namespace, Opts, false) of
                true ->
                    Namespace = file_to_namespace(File),
                    true = ets:insert(Tab, [{{Namespace, Name}, Query} || {Name, Query} <- Queries]),
                    ok;
                false ->
                    true = ets:insert(Tab, Queries),
                    ok
            end;
        Error ->
            Error
    end.

new_tab(Name) ->
    ets:new(Name, [named_table, set, {read_concurrency, true}]).

-spec get_query(atom(), query_list() | ets:tab(), proplists:proplist()) ->
    {ok, iolist()} | undefined.
get_query(Name, TidOrList, Params) ->
    case get_query(Name, TidOrList) of
        {ok, Query} ->
            case is_bitstring(Query) of
                true -> {ok,Query};
                false -> {ok, lists:map(fun(Key) when is_atom(Key) ->
                               proplists:get_value(Key, Params);
                              (S) ->
                               S
                           end, Query)}
            end;
        Other ->
            Other
    end.

-spec get_query(atom(), query_list() | ets:tab()) -> {ok, iolist()} | undefined.
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

%% internal functions

-spec file_to_namespace(file:filename_all()) -> atom().
file_to_namespace(File) ->
    list_to_atom(filename:rootname(filename:basename(File))).

maybe_flush(Tab, File, Opts) ->
    case proplists:get_value(flush, Opts, false) of
        true ->
            case proplists:get_value(namespace, Opts, false) of
                true ->
                    Namespace = file_to_namespace(File),
                    ets:match_delete(Tab, {{Namespace, '_'}, '_'});
                false ->
                    ets:delete_all_objects(Tab)
            end;
        false ->
            ok
    end.
