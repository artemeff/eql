### eql [![Hex.pm](https://img.shields.io/hexpm/v/eql.svg)](eql)

---

Erlang with SQL, inspired by [yesql](https://github.com/krisajenkins/yesql).

---

### description

Suppose we have a database with a list of users, in Erlang we can use simple ORM ([boss_db](https://github.com/ChicagoBoss/boss_db), [texas](https://github.com/emedia-project/texas)), SQL builders ([sqerl](https://github.com/devinus/sqerl), [mekao](https://github.com/ddosia/mekao)) or write SQL queries in Erlang code like this:

```erlang
get_users(Conn) ->
    pgsql:squery(Conn, "SELECT * FROM users;").
```

Or something like that. __yesql__ proposes to write SQL queries by your hands, but do it comfortably. Because most of ORM and SQL builders adds some [accidental complexity](http://en.wikipedia.org/wiki/No_Silver_Bullet) to your software, ORM or builder should parse your code into SQL and then your RDBMS should parse SQL, also write a complex query in ORM - is pure hell, sometimes. That's why you should use pure SQL with some handy features, like that:

```sql
-- Get all users from database
-- :get_all_users
SELECT *
FROM users;

-- Just some description here
-- :get_user_by_id
SELECT *
FROM users
WHERE id = ?

-- Just some description here
-- :get_by_id
SELECT *
FROM :table
WHERE id = ?
```

In Erlang you can use this queries like functions:

```erlang
> {ok, Queries} = eql:compile("queries/user.sql"). % with path to your queries file
> {ok, Q1} = eql:get_query(get_all_users, Queries).
> {ok, Q2} = proplists:get_value(get_user_by_id, Queries).
> {ok, Q3} = eql:get_query(get_by_id, Queries, [{table, "some_table"}]).
> Q1.
%> <<"SELECT * FROM users;">>
> Q2.
%> <<"SELECT * FROM users WHERE id = ?">>
> Q3.
%> [<<"SELECT * FROM ">>,"some_table",<<" WHERE id = ?">>]
```

---

### Not only for SQL

This library can provide anything you want with separated sections in file, eg for ENV-specific configuration:

```erlang
-- ./env.config file
-- :dev
[{host, "app.dev"}].

-- :prod
[{host, "app.com"}].
```

We can parse this file like SQL queries (but comments with `%` isn't supported yet, you can create PR for this feature :)).

```erlang
> {ok, Config} = eql:compile("./env.config").
%> {ok,[{prod,<<"[{host, \"app.com\"}].">>},
        {dev,<<"[{host, \"app.dev\"}].">>}]}
```

`eql:compile/1` returns proplist of defined env configurations and we can parse each of them with simple helper:

```erlang
-module(config_helper).
-export([load/2]).

load(Env, Config) ->
    parse(scan(proplists:get_value(Env, Config))).

scan(undefined) ->
    undefined;
scan(IoList) ->
    erl_scan:string(lists:flatten(io_lib:format("~s", [IoList]))).

parse({ok, Tokens, _}) ->
    erl_parse:parse_term(Tokens);
parse(_) ->
    undefined.
```

And use it like:

```erlang
> config_helper:load(prod, Config).
%> {ok,[{host,"app.com"}]}
> config_helper:load(dev, Config).
%> {ok,[{host,"app.dev"}]}
```

This module already exist and named [`eql_config`](/src/eql_config.erl) for you.

---

### Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

If you make changes to the neotoma PEG file, `src/eql_parse.peg` you must run `rebar3 as dev compile` to rebuild `src/eql_parse.erl`, then run `rebar3` commands like `compile` and `eunit` as normal.
