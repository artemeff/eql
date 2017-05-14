-module(eql_compiler_tests).
-include_lib("eunit/include/eunit.hrl").

eql_compiler_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun test_compile/0
    , fun test_named_params/0
    , fun test_file/0
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
    ?assertMatch([ {get_all_users, <<"SELECT * FROM users">>}
                 , {get_user_by_id, <<"SELECT * FROM users WHERE id = ?">>}
                 ], Queries).

%%
%% Helpers
%%

from_examples_dir(File) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "examples", File]).
