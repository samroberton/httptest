# httptest

_Literate HTTP for integration testing_


## Status

Unfinished, very work-in-progress, experimental, and very subject to change!


## What?

`httptest` allows you to express HTTP request / response interactions in a plain
text file which is executable as a test of the API (or a client of it). The file
format renders as Markdown, so your tests are also readable on Github /
BitBucket / etc.

See the [examples] directory to get an idea.


## Why?

1. Plain text HTTP requests and responses give you easily-readable
   machine-checkable documentation of API behaviour, independent of the API's
   implementation language.

2. Pull request diffs clearly express the over-the-wire changes they implement.

3. Carefully-written request/response interactions can be used to test both
   server (by sending the given request and matching against the resulting
   response) and client (by listening for a matching request and sending back
   the given response).


## Why not?

This isn't the right tool for you if:

1. Your API tests have complex control flow.  That's best expressed in a
   programming language.

2. Your API payloads are regularly multiple pages, and reading a Markdown
   document containing them might as well require infinite scroll.

3. You are only interesting in verifying small parts of the API payload, and
   want to allow the rest of it to vary freely.

4. Your API comes with a client library/SDK, and you want people to use that to
   call you rather than constructing HTTP payloads themselves.


## More details

See the [Format description](docs/Format.md).
