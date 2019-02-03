## Call 1

Request:

    GET /status/200
    Accept: */*
    User-Agent: httptest


Response:

    200 OK
    Content-Type: text/plain; charset=utf-8

    Hello, world!


## Call 2

Request:

    GET /status/404
    Accept: */*
    User-Agent: httptest


Response:

    404 Not Found
    Content-Type: text/plain; charset=utf-8

    Nothing here!
