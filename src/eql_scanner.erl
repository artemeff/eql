-module(eql_scanner).
-export([scan/1]).

scan(Content) ->
    scan(string:tokens(Content, "\n"), [], 1).

scan([], Tokens, Pos) ->
    {ok, lists:reverse(Tokens), Pos};
scan([Line|T], Tokens, Pos) ->
    case scan_line(Line, Pos) of
        skip_token -> scan(T, Tokens, Pos + 1);
        Token      -> scan(T, [Token|Tokens], Pos + 1)
    end.

scan_line([$-, $-|T], Pos) ->
    scan_comment(T, Pos);
scan_line(Query, Pos) ->
    {query, Pos, Query}.

scan_comment([$:|Name], Pos) ->
    name(Name, Pos);
scan_comment([$ , $:|Name], Pos) ->
    name(Name, Pos);
scan_comment(_Comment, _Pos) ->
    skip_token.

name(Name, Pos) ->
    {name, Pos, list_to_atom(Name)}.
