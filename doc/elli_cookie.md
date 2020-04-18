

# Module elli_cookie #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A library application for reading and managing cookies in elli.

Copyright (c) 2012, aj heller; 2016, Eric Bailey; 2016-2020, elli-lib team

__Authors:__ aj heller ([`aj@drfloob.com`](mailto:aj@drfloob.com)), Eric Bailey ([`eric@ericb.me`](mailto:eric@ericb.me)).

<a name="types"></a>

## Data Types ##




### <a name="type-cookie">cookie()</a> ###


__abstract datatype__: `cookie()`

A tuple of name and value where both are binary strings.



### <a name="type-cookie_list">cookie_list()</a> ###


__abstract datatype__: `cookie_list()`

A list of [cookies](#type-cookie).



### <a name="type-cookie_option">cookie_option()</a> ###


__abstract datatype__: `cookie_option()`

Either a supported atom or a tuple of supported atom
and binary/string value.

Supported options are currently:



<dt><code>{expires, Date :: binary() | string()}</code></dt>




<dd>
<code>Date</code> should be the current date in RFC
<a href="https://tools.ietf.org.md/rfc1123">1123</a>/
<a href="https://tools.ietf.org.md/rfc822">822</a> format.
</dd>




<dt><code>{max_age, MaxAge :: non_neg_integer()}</code></dt>




<dd><code>MaxAge</code> is a Unix timestamp.</dd>




<dt><code>{path, Path}</code></dt>




<dd>
According to <a href="https://tools.ietf.org.md/rfc2965">RFC 2965</a>,
"the value of the <code>Path</code> attribute specifies the subset of URLs on the
origin server to which this cookie applies."
</dd>




<dt><code>{domain, Domain}</code></dt>




<dd>
According to <a href="https://tools.ietf.org.md/rfc2965">RFC 2965</a>,
"the value of the <code>Domain</code> attribute specifies the domain for which the
cookie is valid. If an explicitly specified value does not start with a
dot, the user agent supplies a leading dot."
</dd>




<dt><code>secure</code></dt>




<dd>
According to <a href="https://tools.ietf.org.md/rfc2965">RFC 2965</a>,
"the Secure attribute... directs the user agent to use only... secure
means to contact the origin server whenever it sends back this cookie..."
</dd>




<dt><code>http_only</code></dt>




<dd>Make an HTTP-only cookie.</dd>





### <a name="type-expiration">expiration()</a> ###


__abstract datatype__: `expiration()`

For convience, <code><a href="#type-expires">expires()</a></code> can be expressed as a
non-negative number of `seconds | minutes | hours | days | weeks` from now.
[`expires/1`](#expires-1) will convert an <code><a href="#type-expiration">expiration()</a></code> to an <code><a href="#type-expires">expires()</a></code>.



### <a name="type-expires">expires()</a> ###


__abstract datatype__: `expires()`

See description under <code><a href="#type-cookie_option">cookie_option()</a></code>.



### <a name="type-max_age">max_age()</a> ###


__abstract datatype__: `max_age()`

Per [RFC
2965](https://tools.ietf.org.md/rfc2965), "the value of the Max-Age attribute is delta-seconds,
the lifetime of the cookie in seconds, a decimal non-negative integer."

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Equivalent to <a href="#delete-2"><tt>delete(Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Create a header that will delete a specific cookie on the client.</td></tr><tr><td valign="top"><a href="#domain-1">domain/1</a></td><td>Set a domain for a cookie.</td></tr><tr><td valign="top"><a href="#expires-1">expires/1</a></td><td>Set cookie expiration.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Equivalent to <a href="#get-3"><tt>get(Key, Cookies, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>Retrieve a specific cookie value from the set of parsed cookies.</td></tr><tr><td valign="top"><a href="#http_only-0">http_only/0</a></td><td>Make an HTTP-only cookie.</td></tr><tr><td valign="top"><a href="#max_age-1">max_age/1</a></td><td>Set cookie <code>Max-Age</code>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(Name, Value, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create a new cookie in a format appropriate for a server response.</td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>Return a proplist made from the submitted cookies.</td></tr><tr><td valign="top"><a href="#path-1">path/1</a></td><td>Set a path for a cookie.</td></tr><tr><td valign="top"><a href="#secure-0">secure/0</a></td><td>Make a cookie secure (SSL).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Name::binary() | string()) -&gt; <a href="#type-cookie">cookie()</a>
</code></pre>
<br />

Equivalent to [`delete(Name, [])`](#delete-2).

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Name, Options) -&gt; <a href="#type-cookie">cookie()</a>
</code></pre>

<ul class="definitions"><li><code>Name = binary() | string()</code></li><li><code>Options = [<a href="#type-cookie_option">cookie_option()</a>]</code></li></ul>

Create a header that will delete a specific cookie on the client.

<a name="domain-1"></a>

### domain/1 ###

<pre><code>
domain(Domain) -&gt; {domain, Domain}
</code></pre>

<ul class="definitions"><li><code>Domain = binary() | string()</code></li></ul>

Set a domain for a cookie.

<a name="expires-1"></a>

### expires/1 ###

<pre><code>
expires(Expiration::<a href="#type-expiration">expiration()</a> | <a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; <a href="#type-expires">expires()</a>
</code></pre>
<br />

Set cookie expiration.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Key, Cookies) -&gt; binary() | undefined
</code></pre>

<ul class="definitions"><li><code>Key = binary() | string()</code></li><li><code>Cookies = no_cookies | <a href="#type-cookie_list">cookie_list()</a></code></li></ul>

Equivalent to [`get(Key, Cookies, undefined)`](#get-3).

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Key, Cookies, Default) -&gt; binary() | Default
</code></pre>

<ul class="definitions"><li><code>Key = binary() | string()</code></li><li><code>Cookies = no_cookies | <a href="#type-cookie_list">cookie_list()</a></code></li></ul>

Retrieve a specific cookie value from the set of parsed cookies.
If there is not a value for `Key` in `Cookies`, return `Default`.

<a name="http_only-0"></a>

### http_only/0 ###

<pre><code>
http_only() -&gt; http_only
</code></pre>
<br />

Make an HTTP-only cookie.

<a name="max_age-1"></a>

### max_age/1 ###

<pre><code>
max_age(Expiration::<a href="#type-expiration">expiration()</a>) -&gt; <a href="#type-max_age">max_age()</a>
</code></pre>
<br />

Set cookie `Max-Age`.

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Name, Value) -&gt; <a href="#type-cookie">cookie()</a>
</code></pre>

<ul class="definitions"><li><code>Name = binary() | string()</code></li><li><code>Value = binary() | string()</code></li></ul>

Equivalent to [`new(Name, Value, [])`](#new-3).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Name, Value, Options) -&gt; <a href="#type-cookie">cookie()</a>
</code></pre>

<ul class="definitions"><li><code>Name = binary() | string()</code></li><li><code>Value = binary() | string()</code></li><li><code>Options = [<a href="#type-cookie_option">cookie_option()</a>]</code></li></ul>

Create a new cookie in a format appropriate for a server response.

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Req::<a href="elli.md#type-req">elli:req()</a>) -&gt; no_cookies | <a href="#type-cookie_list">cookie_list()</a>
</code></pre>
<br />

Return a proplist made from the submitted cookies.

<a name="path-1"></a>

### path/1 ###

<pre><code>
path(Path) -&gt; {path, Path}
</code></pre>

<ul class="definitions"><li><code>Path = binary() | string()</code></li></ul>

Set a path for a cookie.

<a name="secure-0"></a>

### secure/0 ###

<pre><code>
secure() -&gt; secure
</code></pre>
<br />

Make a cookie secure (SSL).

