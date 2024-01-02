-module(eql_compiler_tests).
-include_lib("eunit/include/eunit.hrl").

eql_compiler_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun test_compile/0
    , fun test_named_params/0
    , fun test_file/0
    , fun test_file_namespace/0
    , fun test_file_named_params/0
    , fun test_eql_config/0
    ]}.

start() -> ok.
stop(_) -> ok.

%%
%% Tests
%%

test_compile() ->
    ?assertMatch({ok, [{test, <<"SELECT * FROM users">>}, {qwe, <<"asd">>}]},
        eql_parse:parse("-- :test\nSELECT * FROM users\n-- name:qwe\nasd\n")),
    ?assertMatch({ok, [{test,<<"SELECT * FROM users;">>}]},
        eql_parse:parse("-- name: test\nSELECT * FROM users;")),
    ?assertMatch({ok, [{test,<<"SELECT * FROM users;">>}, {qwe, <<"asd;">>}]},
        eql_parse:parse("-- name: test\nSELECT * FROM users;\n-- name:qwe\nasd;")).

test_named_params() ->
    {ok, Queries1} = eql_parse:parse("-- :test\nSELECT :id FROM :table\n-- name:qwe\nasd\n"),
    ?assertMatch([{test, [<<"SELECT ">>, id, <<" FROM ">>, table]}, {qwe, <<"asd">>}],
                 Queries1),
    ?assertMatch({ok, [<<"SELECT ">>, <<"1234">>, <<" FROM ">>, <<"users">>]},
                 eql:get_query(test, Queries1, [{id, <<"1234">>}, {table, <<"users">>}])).

test_file() ->
    {ok, Source}  = file:read_file(from_examples_dir("queries.sql")),
    {ok, Queries} = eql_parse:parse(Source),
    ?assertMatch([
        {get_all_users, <<"SELECT * FROM users">>}
        , {get_user_by_id, <<"SELECT * FROM users WHERE id = ?">>}
        , {get_all_schema_users, [<<"SELECT * FROM ">>, schema, <<".users">>]}
        , {accept_type_casts, <<"select '[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]'::json->2">>}
        ], Queries).

test_eql_config() ->
    {ok, DynamicConfigs}  = file:read_file(from_examples_dir("dynamic.conf.in")),
    {ok, Queries} = eql_parse:parse(DynamicConfigs),
    {ok, DynamicConfig} = eql_config:load(dynamic_config,Queries,[{value_a,"foo"},{value_b,"bar"},{value_c,"baz"}]),
    ?assertMatch([{"a","foo"},{"b","bar"},{"c","baz"}],DynamicConfig).

test_file_namespace() ->
    eql:new_tab(test_tab),
    eql:compile(test_tab, from_examples_dir("queries.sql"), [namespace]),
    ?assertEqual({ok, <<"SELECT * FROM users">>}, eql:get_query({queries, get_all_users}, test_tab)).

test_file_named_params() ->
    eql:new_tab(test_tab2),
    eql:compile(test_tab2, from_examples_dir("queries.sql")),
    ?assertEqual({ok, [<<"SELECT * FROM ">>, <<"public">>, <<".users">>]},
                 eql:get_query(get_all_schema_users, test_tab2, [{schema, <<"public">>}])).

%%
%% Helpers
%%

from_examples_dir(File) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "examples", File]).
