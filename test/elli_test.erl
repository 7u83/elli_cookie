%%%===================================================================
%%% @author aj heller <aj@drfloob.com>
%%% @copyright (C) 2012, aj heller
%%% @copyright (C) 2016, Eric Bailey
%%% @doc elli_cookie unit tests.
%%% @end
%%% Created :  25 Mar 2016 by Eric Bailey
%%%===================================================================
-module(elli_test).

-compile(export_all).

%% Basic Cookie Management
-import(elli_cookie, [parse/1, get/2, get/3, new/2, new/3, delete/1, delete/2]).

%% Cookie Options
-import(elli_cookie, [expires/1, path/1, domain/1,
                      secure/0, http_only/0, max_age/1]).

-include_lib("elli/include/elli.hrl").

-include_lib("eunit/include/eunit.hrl").


parse_test_() ->
    [ ?_assertError(function_clause, parse(#req{}))
    , ?_assertError({badmatch, _}, parse(#req{headers=[{<<"Cookie">>, <<"1=">>}]}))

    , ?_assertEqual(no_cookies, parse(#req{headers=[]}))
    , ?_assertEqual([{<<"1">>, <<"2">>}], parse(#req{headers=[{<<"Cookie">>, <<"1=2">>}]}))
    , ?_assertEqual([{<<"1">>, <<"2">>}, {<<"3">>, <<"4">>}], parse(#req{headers=[{<<"Cookie">>, <<"1=2; 3=4">>}]}))
    , ?_assertEqual([{<<"1">>, <<"2">>}, {<<"3">>, <<"4">>}, {<<"five">>, <<"six">>}], parse(#req{headers=[{<<"Cookie">>, <<"1=2; 3=4; five   =    six">>}]}))
    ].

get_test_() ->
    Cookies = [{<<"1">>, <<"two">>}, {<<"three">>, <<"4">>}],
    [ ?_assertEqual(undefined, get("nope", []))
    , ?_assertEqual(undefined, get("nope", Cookies))
    , ?_assertEqual(<<"two">>, get("1", Cookies))
    , ?_assertEqual(<<"two">>, get(<<"1">>, Cookies))
    , ?_assertEqual(undefined, get(<<"4">>, Cookies))
    , ?_assertEqual(nope, get(<<"4">>, Cookies, nope))
    , ?_assertError({badmatch, {invalid_cookie_name, <<"4=">>}}, get(<<"4=">>, Cookies, nope))
    ].

get_noCookies_test_() ->
    [ ?_assertEqual(undefined, get("x", no_cookies))
    , ?_assertEqual(undefined, get("x", no_cookies, undefined))
    , ?_assertEqual(bort, get("x", no_cookies, bort))
    ].

new_test_() ->
    [ ?_assertMatch({<<"Set-Cookie">>, <<"name=val">>}, new("name", "val"))
    , ?_assertMatch({<<"Set-Cookie">>, <<"name=val">>}, new(<<"name">>, <<"val">>))
    , ?_assertError({badmatch, {invalid_cookie_value, bork}}, new(<<"name">>, bork))
    , ?_assertError({badmatch, {invalid_cookie_name, bork}}, new(bork, "val"))
    , ?_assertMatch({<<"Set-Cookie">>, <<"name=val=">>}, new("name", "val="))
    , ?_assertMatch({<<"Set-Cookie">>, <<"name=val=">>}, new("name", <<"val=">>))
    , ?_assertError({badmatch, {invalid_cookie_name, "name="}}, new("name=", "val"))
    , ?_assertError({badmatch, {invalid_cookie_name, <<"name=">>}}, new(<<"name=">>, "val"))

      %% be careful: binaries are not checked for stringyness
    , ?_assertMatch(_, new(<<1>>, "val"))

    , ?_assertThrow({error, {invalid_cookie_attribute, domain}}, new("n", "v", [domain, "/"]))
    , ?_assertMatch({_, <<"n=v;Domain=www.example.com">>}, new("n", "v", [domain("www.example.com")]))
    , ?_assertMatch({_, <<"n=v;Path=/">>}, new("n", "v", [path("/")]))
    , ?_assertMatch({_, <<"n=v;Secure">>}, new("n", "v", [secure()]))
    , ?_assertMatch({_, <<"n=v;HttpOnly">>}, new("n", "v", [http_only()]))

      %% expires tests
    , ?_assertMatch({_, <<"n=v;Expires=", _/binary>>}, new("n", "v", [expires({2,seconds})]))
    , ?_assertMatch({_, <<"n=v;Expires=", _/binary>>}, new("n", "v", [expires({2,minutes})]))
    , ?_assertMatch({_, <<"n=v;Expires=", _/binary>>}, new("n", "v", [expires({2,hours})]))
    , ?_assertMatch({_, <<"n=v;Expires=", _/binary>>}, new("n", "v", [expires({2,days})]))
    , ?_assertMatch({_, <<"n=v;Expires=", _/binary>>}, new("n", "v", [expires({2,weeks})]))

      %% max_age tests
    , ?_assertMatch({_, <<"n=v;Max-Age=2", _/binary>>}, new("n", "v", [max_age({2,seconds})]))
    , ?_assertMatch({_, <<"n=v;Max-Age=120", _/binary>>}, new("n", "v", [max_age({2,minutes})]))
    , ?_assertMatch({_, <<"n=v;Max-Age=7200", _/binary>>}, new("n", "v", [max_age({2,hours})]))
    , ?_assertMatch({_, <<"n=v;Max-Age=172800", _/binary>>}, new("n", "v", [max_age({2,days})]))
    , ?_assertMatch({_, <<"n=v;Max-Age=1209600", _/binary>>}, new("n", "v", [max_age({2,weeks})]))
    , ?_assertMatch({_, <<"n=v;Max-Age=69", _/binary>>}, new("n", "v", [max_age(69)]))

    , ?_assertMatch({_, <<"n=v;Expires=", _/binary>>}, new("n", "v", [expires(calendar:local_time())]))
    , ?_assertMatch({_, <<"n=v;Expires=Fri, 21 Mar 2014", _/binary>>}, new("n", "v", [expires({{2014,03,21},{16,20,42}})]))

      %% be careful: cookie options are not thoroughly sanity checked.
    , ?_assertMatch({_, <<"n=v;Domain=/">>}, new("n", "v", [domain("/")]))
    ].

delete_test_() ->
    [ ?_assertError({badmatch, {invalid_cookie_name, bork}}, delete(bork))
    , ?_assertError({badmatch, {invalid_cookie_name, 1}}, delete(1))
    , ?_assertError({badmatch, {invalid_cookie_name, "="}}, delete("="))

    , ?_assertEqual({<<"Set-Cookie">>, <<"test=;Expires=Thu, 01 Jan 1970 08:00:00 GMT">>}, delete("test"))
    , ?_assertMatch({_, <<"test=;Expires=Thu, 01 Jan 1970 08:00:00 GMT", _/binary>>}, delete(<<"test">>))
    , ?_assertError({badmatch, {invalid_cookie_name, <<"=">>}}, delete(<<"=">>))

      %% with Options
    , ?_assertError({badmatch, {invalid_cookie_name, bork}}, delete(bork, [domain("/")]))
    , ?_assertError({badmatch, {invalid_cookie_name, 1}}, delete(1, [domain("/")]))
    , ?_assertError({badmatch, {invalid_cookie_name, "="}}, delete("=", [domain("/")]))

    , ?_assertMatch({_, <<"test=;Expires=Thu, 01 Jan 1970 08:00:00 GMT;Domain=/", _/binary>>}, delete("test", [domain("/")]))
    , ?_assertMatch({_, <<"test=;Expires=Thu, 01 Jan 1970 08:00:00 GMT;Domain=/", _/binary>>}, delete(<<"test">>, [domain("/")]))
    , ?_assertMatch({_, <<"test=;Expires=Thu, 01 Jan 1970 08:00:00 GMT;Domain=example.com;Path=/hork", _/binary>>}, delete(<<"test">>, [domain("example.com"), path("/hork")]))
    , ?_assertError({badmatch, {invalid_cookie_name, <<"=">>}}, delete(<<"=">>, [domain("/")]))
    ].

valueHasEqual_test_() ->
    [ ?_assertMatch({<<"Set-Cookie">>, <<"name=val=3">>}, new("name", "val=3"))
    , ?_assertEqual([{<<"name">>, <<"val=3">>}], parse(#req{headers=[{<<"Cookie">>, <<"name=val=3">>}]}))
    , ?_assertMatch(<<"val=3">>, get("name", [{<<"name">>, <<"val=3">>}]))
    , ?_assertMatch(<<"val=3==">>, get("name", [{<<"name">>, <<"val=3==">>}]))
    , ?_assertError({badmatch, {invalid_cookie_name, "name="}}, get("name=", [{<<"name">>, <<"val=3==">>}]))
    ].
