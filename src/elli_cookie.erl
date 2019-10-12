%%%===================================================================
%%% @author aj heller <aj@drfloob.com>
%%% @author Eric Bailey <eric@ericb.me>
%%% @copyright 2012, aj heller; 2016, Eric Bailey
%%% @end
%%% @doc A library application for reading and managing cookies in elli.
%%% @end
%%% Created :  3 Oct 2012 by aj heller <aj@drfloob.com>
%%%===================================================================
-module(elli_cookie).

%% Basic Cookie Management
-export([parse/1, get/2, get/3, new/2, new/3, delete/1, delete/2]).

%% Cookie Options
-export([expires/1, path/1, domain/1, secure/0, http_only/0, max_age/1]).

-export_type([cookie/0, cookie_list/0, expiration/0, expires/0, max_age/0]).

%% @type cookie(). A tuple of name and value where both are binary strings.
-type cookie()        :: {binary(), binary()}.

%% @type cookie_list(). A list of {@link cookie(). cookies}.
-type cookie_list()   :: [cookie()].

%% @type cookie_option(). Either a supported atom or a tuple of supported atom
%% and binary/string value.
%%
%% Supported options are currently:
%% <dl>
%%  <dt>`{expires, Date :: binary() | string()}'</dt>
%%  <dd>
%%    `Date' should be the current date in RFC
%%    <a href="https://tools.ietf.org/html/rfc1123">1123</a>/
%%    <a href="https://tools.ietf.org/html/rfc822">822</a> format.
%%    </dd>
%%  <dt>`{max_age, MaxAge :: non_neg_integer()}'</dt>
%%  <dd>`MaxAge' is a Unix timestamp.</dd>
%%  <dt>`{path, Path}'</dt>
%%  <dd>
%%    According to <a href="https://tools.ietf.org/html/rfc2965">RFC 2965</a>,
%%    "the value of the `Path' attribute specifies the subset of URLs on the
%%    origin server to which this cookie applies."
%%  </dd>
%%  <dt>`{domain, Domain}'</dt>
%%  <dd>
%%    According to <a href="https://tools.ietf.org/html/rfc2965">RFC 2965</a>,
%%    "the value of the `Domain' attribute specifies the domain for which the
%%    cookie is valid. If an explicitly specified value does not start with a
%%    dot, the user agent supplies a leading dot."
%%  </dd>
%%  <dt>`secure'</dt>
%%  <dd>
%%    According to <a href="https://tools.ietf.org/html/rfc2965">RFC 2965</a>,
%%    "the Secure attribute... directs the user agent to use only... secure
%%    means to contact the origin server whenever it sends back this cookie..."
%%  </dd>
%%  <dt>`http_only'</dt>
%%  <dd>Make an HTTP-only cookie.</dd>
%% </dl>
-type cookie_option() :: {expires, binary() | string()}
                       | {max_age, non_neg_integer()}
                       | {path, binary() | string()}
                       | {domain, binary() | string()}
                       | secure
                       | http_only.

%% @type expiration(). For convience, {@type expires()} can be expressed as a
%% non-negative number of `seconds | minutes | hours | days | weeks' from now.
%% {@link expires/1} will convert an {@type expiration()} to an {@type
%% expires()}.
-type expiration()    :: {non_neg_integer(),
                          seconds | minutes | hours | days | weeks}
                       | non_neg_integer().

%% @type expires(). See description under {@type cookie_option()}.
-type expires()       :: {expires, binary() | string()}.

%% @type max_age(). Per <a href="https://tools.ietf.org/html/rfc2965">RFC
%% 2965</a>, "the value of the Max-Age attribute is delta-seconds,
%% the lifetime of the cookie in seconds, a decimal non-negative integer."
-type max_age()       :: {max_age, non_neg_integer()}.


%% @doc Return a proplist made from the submitted cookies.
-spec parse(Req :: elli:req()) -> no_cookies | cookie_list().
parse(Req) -> tokenize(elli_request:get_header(<<"Cookie">>, Req)).

%% @equiv get(Key, Cookies, undefined)
-spec get(Key, Cookies) -> binary() | undefined when
    Key     :: binary() | string(),
    Cookies :: no_cookies | cookie_list().
get(Key, Cookies) -> get(Key, Cookies, undefined).

%% @doc Retrieve a specific cookie value from the set of parsed cookies.
%% If there is not a value for `Key' in `Cookies', return `Default'.
-spec get(Key, Cookies, Default) -> binary() when
    Key     :: binary() | string(),
    Cookies :: no_cookies | cookie_list(),
    Default :: binary() | undefined.
get(_, no_cookies, Default) -> Default;
get(Key, Cookies, Default) ->
  ok = valid_cookie_name(Key),
  proplists:get_value(to_bin(Key), Cookies, Default).

%% @equiv new(Name, Value, [])
-spec new(Name, Value) -> cookie() when
    Name  :: binary() | string(),
    Value :: binary() | string().
new(Name, Value) -> new(Name, Value, []).

%% @doc Create a new cookie in a format appropriate for a server response.
-spec new(Name, Value, Options) -> cookie() when
    Name    :: binary() | string(),
    Value   :: binary() | string(),
    Options :: [cookie_option()].
new(Name, Value, Options) ->
  ok       = valid_cookie_name(Name),
  ok       = valid_cookie_value(Value),
  BName    = to_bin(Name),
  BValue   = to_bin(Value),
  Bin      = <<BName/binary,"=",BValue/binary>>,
  FinalBin = lists:foldl(fun set_cookie_attribute/2, Bin, Options),
  {<<"Set-Cookie">>, FinalBin}.

%% @equiv delete(Name, [])
-spec delete(Name :: binary() | string()) -> cookie().
delete(Name) -> delete(Name, []).

%% @doc Create a header that will delete a specific cookie on the client.
-spec delete(Name, Options) -> cookie() when
    Name    :: binary() | string(),
    Options :: [cookie_option()].
delete(Name, Options) ->
  ok = valid_cookie_name(Name),
  new(Name, "", [expires({{1970,1,1},{0,0,0}}) | Options]).


%%%===================================================================
%%% Cookie Option helpers
%%%===================================================================

%% @doc Set a path for a cookie.
-spec path(Path) -> {path, Path} when Path :: binary() | string().
path(P) -> {path, P}.

%% @doc Set a domain for a cookie.
-spec domain(Domain) -> {domain, Domain} when Domain :: binary() | string().
domain(P) -> {domain, P}.

%% @doc Make a cookie secure (SSL).
-spec secure() -> secure.
secure() -> secure.

%% @doc Make an HTTP-only cookie.
-spec http_only() -> http_only.
http_only() -> http_only.

%% @doc Set cookie expiration.
-spec expires(Expiration :: expiration()) -> expires().
expires({S, seconds}) -> expires_plus(S);
expires({M, minutes}) -> expires_plus(M*60);
expires({H, hours})   -> expires_plus(H*60*60);
expires({D, days})    -> expires_plus(D*24*60*60);
expires({W, weeks})   -> expires_plus(W*7*24*60*60);
expires(Date)         -> {expires, httpd_util:rfc1123_date(Date)}.

%% @doc Set cookie `Max-Age'.
-spec max_age(Expiration :: expiration()) -> max_age().
max_age({S, seconds}) -> {max_age, (S)};
max_age({M, minutes}) -> {max_age, (M*60)};
max_age({H, hours})   -> {max_age, (H*60*60)};
max_age({D, days})    -> {max_age, (D*24*60*60)};
max_age({W, weeks})   -> {max_age, (W*7*24*60*60)};
max_age(Seconds)      -> {max_age, Seconds}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec to_bin(binary() | string()) -> binary().
to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L)   -> list_to_binary(L);
to_bin(X)                   -> throw({error, {not_a_string, X}}).

-spec strip_bin(binary()) -> binary().
strip_bin(B) -> list_to_binary(string:strip(binary_to_list(B))).

tokenize(<<>>) -> [];
tokenize(CookieStr) when is_binary(CookieStr) ->
  Cookies = binary:split(CookieStr, <<";">>, [trim, global]),
  lists:map(fun do_tokenize/1, Cookies);
tokenize(_) -> no_cookies.

-spec do_tokenize(Cookie :: binary()) -> {Name :: binary(), Value :: binary()}.
do_tokenize(NVP) ->
  case binary:split(NVP, <<"=">>, [trim]) of
    [N,V] -> {strip_bin(N), strip_bin(V)};
    [N]   -> {strip_bin(N), <<>>}
  end.

set_cookie_attribute({expires, Exp}, Bin) ->
  BExp = to_bin(Exp),
  <<Bin/binary, ";Expires=", BExp/binary>>;
set_cookie_attribute({max_age, Exp}, Bin) ->
  BExp = to_bin(integer_to_list(Exp)),
  <<Bin/binary, ";Max-Age=", BExp/binary>>;
set_cookie_attribute({path, Path}, Bin) ->
  BPath = to_bin(Path),
  <<Bin/binary, ";Path=", BPath/binary>>;
set_cookie_attribute({domain, Domain}, Bin) ->
  BDomain = to_bin(Domain),
  <<Bin/binary, ";Domain=", BDomain/binary>>;
set_cookie_attribute(secure,    Bin) -> <<Bin/binary, ";Secure">>;
set_cookie_attribute(http_only, Bin) -> <<Bin/binary, ";HttpOnly">>;
set_cookie_attribute(X, _) -> throw({error, {invalid_cookie_attribute, X}}).

expires_plus(N) ->
  UT   = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  UTE  = UT + N,
  Date = calendar:gregorian_seconds_to_datetime(UTE),
  {expires, httpd_util:rfc1123_date(Date)}.


%%%===================================================================
%%% Predicates
%%%===================================================================

%% TODO: implement cookie spec checking: https://tools.ietf.org/html/rfc6265
-spec valid_cookie_name(Name) -> ok | {invalid_cookie_name, Name}.
valid_cookie_name(B) when is_binary(B) ->
  Str = binary_to_list(B),
  do_valid_cookie_name(string:str(Str, "="), B);
valid_cookie_name(N) when is_list(N) ->
  do_valid_cookie_name(string:str(N, "="), N);
valid_cookie_name(X) -> {invalid_cookie_name, X}.

-spec do_valid_cookie_name(Index, Name) -> ok | {invalid_cookie_name, Name} when
    Index :: non_neg_integer().
do_valid_cookie_name(0, _) -> ok;
do_valid_cookie_name(_, N) -> {invalid_cookie_name, N}.

%% TODO: implement cookie spec checking: https://tools.ietf.org/html/rfc6265
-spec valid_cookie_value(Value) -> ok | {invalid_cookie_value, Value}.
valid_cookie_value(B) when is_binary(B); is_list(B) -> ok;
valid_cookie_value(X) -> {invalid_cookie_value, X}.
