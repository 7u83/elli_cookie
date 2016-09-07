# elli_cookie

[![Build Status][Travis badge]][Travis link]
[![Hex Badge][Hex badge]][Hex link]

[Travis badge]: https://travis-ci.org/elli-lib/elli_cookie.svg?branch=master
[Travis link]: https://travis-ci.org/elli-lib/elli_cookie
[Hex badge]: https://img.shields.io/hexpm/v/elli_cookie.svg
[Hex link]: https://hex.pm/packages/elli_cookie

A library application for reading, setting, and otherwise managing cookies in
[Elli](https://github.com/knutin/elli).

## Usage

See the large test set in [elli_cookie_test](test/elli_cookie_test.erl) for more
thorough usage examples.

### Basic Cookie Management and Cookie Option Settings

In an Elli callback module:

``` erlang
handle(Req, _Config) ->
  Cookies = elli_cookie:parse(Req),

  %% retrieve a cookie value ...
  _PublicKey = elli_cookie:get(<<"key">>, Cookies),
  %% ... and do something with it

  %% create new cookie for domain www.example.com that expires in two weeks
  FizzCookie = elli_cookie:new(<<"fizz">>, <<"buzz">>,
                               [elli_cookie:domain(<<"www.example.com">>),
                                elli_cookie:expires({2, weeks})]),

  %% delete key cookie
  DeleteKeyCookie = elli_cookie:delete(<<"key">>),

  %% return response with cookies
  {ok, [DeleteKeyCookie, FizzCookie], <<"key deleted; fizz set">>}.
```
