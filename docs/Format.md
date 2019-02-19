# Format

`httptest` files use a Markdown syntax. The basic layout of a file is:

1. An optional "header 1" naming the test, eg: `# My first test`.

2. A "header 2" introducing a request / response pair, eg: `## Login`.

3. The text `### Request`.

4. A block indented by four spaces containing the HTTP request.

5. The text `### Response`.

6. A block indented by four spaces containing the HTTP response.



## HTTP messages

The four-space indent for HTTP requests / responses makes them Markdown code
blocks.

Messages are written in the format they go over the wire, with a few optional
differences:

1. You can leave out the HTTP version.

2. A request can specify a full URL -- including protocol, host and auth info,
   if desired -- instead of just path and query string.



## Variables

You can use variables within HTTP messages, much like a templating engine, eg
`GET /user/${userId}`.  Variable identifiers are alphanumeric, and you can
include whitespace inside the braces if you want, eg `${ userId42 }`.

You can supply variable values as input when you invoke `httptest`.

_TODO: example of supplying input variable values_

`httptest` can also extract variable values from messages it receives and
matches against.  For example, if the response body is

    { userId : 42 }

and the response message is specified in the file as

    { userId : ${uid} }

then `httptest` can extract a `uid` variable with the value `42`.

To do this, you need to specify an extraction pattern for the variable.  After
the message code block, you include, for example:

    Extract:
    * `uid ~ /\d+/`

Here you specify the variable name which you used in the message and a regular
expression which the variable will match.  You should try to provide a fairly
precise regular expression: `httptest` will allow the regular expression to
consume as much input as it wants, even if that means it's consumed input that
you expected to match with the literal message content following the variable.
Note, though, that by default the regular expression will _not_ be allowed to
consume newlines.

_TODO: syntax for allowing a multi-line regex._

You can only extract a variable the first time it's used in the file.  After
that, subsequent usages will expect the value where it's used to be the same as
the value which was first extracted -- that is, they'll behave as though they're
performing template substitution of the existing variable's value.



## Matching received messages

While the messages which `httptest` sends (HTTP requests, when testing a server
API) are constructed directly from the template provided, the messages it
receives (HTTP responses, when testing a server API) need not be exactly
identical to the over-the-wire message:

1. You don't have to specify all HTTP headers: `httptest` will verify that the
   received message has the headers specified in the file, but won't complain if
   there are other additional headers.  (In particular, you're unlikely to want
   to have to specify what value the `Content-Length` header should have,
   especially if there are variables in the message definition.)

2. Header order is not important: the response will match if the header names
   specified in the file have the given header values, regardless of the order
   they appear.

3. Bodies which arrive with a `Content-Encoding: gzip` header will be
   automatically decompressed, and the match will be against the decompressed
   payload.


## Comments / paragraph text

You can include arbitrary text anywhere except:

* between `### Request` and the request code block
* between `### Response` and the response code block
* between `Extract:` and the bulleted list of variable extractions

It will be ignored, so you can use this for comments.

You can't use headers as part of your comments, since they're already used to
name the test (h1) and to delineate request/response pairs (h2).

You can use code blocks if you want.


## Line endings

You can use *nix or Windows line endings.  Your HTTP messages will still have
CRLF where the spec says they should have CRLF.  Newlines in the body will be
sent with the line ending used in the file: if you're using *nix line endings
for the file and want to send a CRLF in the body, you can get your text editor
to insert a carriage return into the file just before the newline in those
places where you want it.
