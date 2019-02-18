## Login

### Request

    POST /login
    Accept: */*
    Content-Type: application/json
    User-Agent: httptest

    {
        "username": "sam",
        "password": "p@ssw0rd"
    }


### Response

    200 OK
    Content-Type: application/json; charset=utf-8
    Set-Cookie: auth=${ authCookie }

    {
        "userId": ${ userId }
    }


Extract:

* `authCookie ~ /([^;]+); .*/`
* `userId ~ /\d+/`


## Get User

### Request

    GET /user/${ userId }
    Accept: */*
    User-Agent: httptest
    Cookie: auth=${ authCookie }


### Response

    200 OK
    Content-Type: application/json; charset=utf-8

    {
        "id": ${ userId },
        "username": "sam",
        "fullName": "Sam Roberton"
    }
