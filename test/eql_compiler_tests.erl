-module(eql_compiler_tests).
-include_lib("eunit/include/eunit.hrl").

eql_compiler_test_() -> {setup,
    fun start/0, fun stop/1,
    [ fun test_compile/0
    , fun test_file/0
    ]}.

start() -> ok.
stop(_) -> ok.

%%
%% Tests
%%

test_compile() ->
    ?assertMatch({ok, [{qwe, "asd"}, {test, "SELECT * FROM users"}]},
        eql_compiler:compile("-- :test\nSELECT * FROM users\n-- :qwe\nasd\n")),
    ?assertMatch({ok, [{test,"SELECT * FROM users;"}]},
        eql_compiler:compile("-- :test\nSELECT * FROM users;")),
    ?assertMatch({ok, [{qwe, "asd;"}, {test,"SELECT * FROM users;"}]},
        eql_compiler:compile("-- :test\nSELECT * FROM users;\n--:qwe\nasd;")).

test_file() ->
    {ok, Source}  = file:read_file(from_examples_dir("queries.sql")),
    {ok, Queries} = eql_compiler:compile(Source),
    ?assertMatch([ {get_user_by_id, "SELECT * FROM users WHERE id = ?"}
                 , {get_all_users, "SELECT * FROM users"}
                 ], Queries).

%%
%% Helpers
%%

from_examples_dir(File) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "examples", File]).
