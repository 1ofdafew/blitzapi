blitzapi
=====

A HTTP API for Blitzcoin Wallet

## How To Build
```
$ git clone git@github.com:1ofdafew/blitzapi.git
$ rebar3 compile
$ rebar3 release
```

# API

The API shall follow JSON data for request and response, with HTTP verbs for specific action required.

| No. | Method | URL                     | Description        | Header           | JSON Data          |
|:----|--------|-------------------------|--------------------|------------------|--------------------|
| 1   | POST   | /api/v1/users           | Create new user    | none             | username, password |
| 2   | POST   | /api/v1/auth            | Get auth token     | none             | username, password |
| 3   | GET    | /api/v1/users/:username | Get user record    | X-Blitzcoin-Key  | password           |
| 4   | PUT    | /api/v1/users/:username | Update user record | X-Blitzcoin-Key  | password, active   |
| 5   | DELETE | /api/v1/users/:username | Delete user record | X-Blitzcoin-Key  | password           |
| 6   | POST   | /api/v1/wallet/         | Create user wallet | X-Blitzcoin-Key  | wallet, password   |
| 7   | GET    | /api/v1/wallet/:wallet  | Get wallet details | X-Blitzcoin-Key  | password           |
| 8   | PUT    | /api/v1/wallet/:wallet  | Transfer coins     | X-Blitzcoin-Key  | password, mixin, address, amount, fee |
| 9   | DELETE | /api/v1/wallet/:wallet  | Delete wallet      | X-Blitzcoin-Key  | password           |

# Example

## 1. Creating new user

```
curl -X POST -H 'Content-Type: application/json' \
  http://localhost:8443/api/v1/users/ \
  -d '{"username": "bar", "password": "secret"}' \
  -vvv
```

> Positive Results

```
> POST /api/v1/users/ HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> Content-Length: 37
>
* upload completely sent off: 37 out of 37 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 17:56:45 GMT
< content-length: 164
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: text/html
<
* Connection #0 to host localhost left intact
{"user":{"username":"bar","password":"******","created":"2016-10-23T17:56:45Z","updated":"2016-10-23T17:56:45Z","active":true},"server_time":"2016-10-23T17:56:45Z"}
```

> Negative Results

```
> POST /api/v1/users/ HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> Content-Length: 37
>
* upload completely sent off: 37 out of 37 bytes
< HTTP/1.1 400 Bad Request
< server: Cowboy
< date: Sun, 23 Oct 2016 17:54:59 GMT
< content-length: 60
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: text/html
<
* Connection #0 to host localhost left intact
{"error":"User exists","server_time":"2016-10-23T17:54:59Z"}

```

## 2. Authentication

```
curl -X POST -H 'Content-Type: application/json' \
  http://localhost:8443/api/v1/auth/ \
  -d '{"username": "bar", "password": "secret"}' \
  -vvv
```

> Positive Results

```
> POST /api/v1/auth/ HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> Content-Length: 37
>
* upload completely sent off: 37 out of 37 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 17:59:37 GMT
< content-length: 154
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: text/html
<
* Connection #0 to host localhost left intact
{"token":"2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm","server_time":"2016-10-23T17:59:37Z","valid_until":"2016-10-23T21:59:37Z"}
```

> Negative Results

```
> POST /api/v1/auth/ HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> Content-Length: 38
>
* upload completely sent off: 38 out of 38 bytes
< HTTP/1.1 400 Bad Request
< server: Cowboy
< date: Sun, 23 Oct 2016 18:01:09 GMT
< content-length: 41
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: text/html
<
* Connection #0 to host localhost left intact
{"error":"Invalid username, or password"}
```

## 3. Get user record
```
curl -X GET -H 'Content-Type: application/json' \
  -H 'X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm' \
  http://localhost:8443/api/v1/users/bar \
  -d '{"password": "secret"}' \
  -vvv
```

> Positive Results

```
> GET /api/v1/users/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
>
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 18:38:27 GMT
< content-length: 164
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"user":{"username":"bar","password":"******","created":"2016-10-23T17:56:45Z","updated":"2016-10-23T17:56:45Z","active":true},"server_time":"2016-10-23T18:38:28Z"}
```

> Negative Results

```
>   -H 'X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm' \
>   http://localhost:8443/api/v1/users/bar \
>   -vvv
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 8443 (#0)
> GET /api/v1/users/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
>
< HTTP/1.1 401 Unauthorized
< server: Cowboy
< date: Sun, 23 Oct 2016 18:40:22 GMT
< content-length: 0
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< www-authenticate: Basic Realm="Blitzcoin API V1.0.0"
<
```

## 4. Update user record
```
curl -X PUT -H 'Content-Type: application/json' \
  -H 'X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm' \
  http://localhost:8443/api/v1/users/bar \
  -d '{"password":"new_password","active":true}' \
  -vvv
```

> Positive Results

```
> PUT /api/v1/users/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 41
>
* upload completely sent off: 41 out of 41 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 19:04:34 GMT
< content-length: 164
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"user":{"username":"bar","password":"******","created":"2016-10-23T17:56:45Z","updated":"2016-10-23T19:04:35Z","active":true},"server_time":"2016-10-23T19:04:35Z"}
```

> Negative Results

```
> PUT /api/v1/users/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 41
>
* upload completely sent off: 41 out of 41 bytes
< HTTP/1.1 400 Bad Request
< server: Cowboy
< date: Sun, 23 Oct 2016 19:02:10 GMT
< content-length: 66
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"error":"Missing json data","server_time":"2016-10-23T19:02:10Z"}
```

## 5. Delete user
```
curl -X DELETE -H 'Content-Type: application/json' \
  -H 'X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm' \
  http://localhost:8443/api/v1/users/bar \
  -d '{"password":"new_password"}' \
  -vvv
```

> Positive Results

```
> DELETE /api/v1/users/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 27
>
* upload completely sent off: 27 out of 27 bytes
< HTTP/1.1 204 No Content
< server: Cowboy
< date: Sun, 23 Oct 2016 19:21:45 GMT
< content-length: 0
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
```

> Negative Results

```
> DELETE /api/v1/users/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 29
>
* upload completely sent off: 29 out of 29 bytes
< HTTP/1.1 204 No Content
< server: Cowboy
< date: Sun, 23 Oct 2016 19:12:41 GMT
< content-length: 0
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
```

## 6. Create user wallet.
```
curl -X POST -H 'Content-Type: application/json' \
  -H 'X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm' \
  http://localhost:8443/api/v1/wallets/ \
  -d '{"wallet":"bar", "password":"secret"}' \
  -vvv
```

> Positive Results

```
> POST /api/v1/wallets/ HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 45
>
* upload completely sent off: 45 out of 45 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 19:38:20 GMT
< content-length: 148
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: text/html
<
* Connection #0 to host localhost left intact
{"address":"bxdMFooDNui9FVYws4qEjJHya4tFp9ReX4oxb6nJ2E8hWwqphvmZfx3Df4MMzYCGsCUNjHQMi1sKpXbcJAjvhDdE17oGPVT58","server_time":"2016-10-23T19:38:19Z"}
```

> Negative Results

```
> POST /api/v1/wallets/ HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 44
>
* upload completely sent off: 44 out of 44 bytes
< HTTP/1.1 400 Bad Request
< server: Cowboy
< date: Sun, 23 Oct 2016 19:31:38 GMT
< content-length: 62
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: text/html
<
* Connection #0 to host localhost left intact
{"error":"Wallet exists","server_time":"2016-10-23T19:31:39Z"}
```

## 7. Get wallet details.
```
curl -X GET -H 'Content-Type: application/json' \
  -H 'X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm' \
  http://localhost:8443/api/v1/wallets/bar \
  -d '{"password":"secret"}' \
  -vvv
```

> Positive Results

```
> GET /api/v1/wallets/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 21
>
* upload completely sent off: 21 out of 21 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 20:40:15 GMT
< content-length: 196
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"address":"bxdMFooDNui9FVYws4qEjJHya4tFp9ReX4oxb6nJ2E8hWwqphvmZfx3Df4MMzYCGsCUNjHQMi1sKpXbcJAjvhDdE17oGPVT58","balance":"0.00000000,","locked":"100.00000000","server_time":"2016-10-23T20:40:15Z"}
```

> Negative Results

```
> GET /api/v1/wallets/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 22
>
* upload completely sent off: 22 out of 22 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 20:06:08 GMT
< content-length: 80
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"error":"Invalid wallet, or bad password","server_time":"2016-10-23T20:06:05Z"}
```

## 8. Transfer coins
```
curl -X PUT -H 'Content-Type: application/json' \
  -H 'X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm' \
  http://localhost:8443/api/v1/wallets/bar \
  -d '{"password":"secret", "mixin": 0, "address": "bxdpREJ5vHCXn5GjXwU2xnBtMr3ziFVbM28ua6heaWTsiz6YsY3LZrQFrA1Qu7zsm7Y6aPbsuffNCJuLhFViEdB11boskgDJU", "amount": 1.0, "fee": 0.001}' \
  -vvv
```

> Positive Results

```
> PUT /api/v1/wallets/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 174
>
* upload completely sent off: 174 out of 174 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 20:46:49 GMT
< content-length: 170
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"result":["[wallet bxdMFo]: Money successfully sent, transaction a2cde9e7ba89d78a35aeea4a27e3d88ea946e47f261d3bbb1773b4acfcd6e892"],"server_time":"2016-10-23T20:46:49Z"}
```

> Negative Results

```
> PUT /api/v1/wallets/bar HTTP/1.1
> Host: localhost:8443
> User-Agent: curl/7.49.1
> Accept: */*
> Content-Type: application/json
> X-Blitzcoin-Key: 2oYhFwnZttHGVHu4iFs5M4wTYQ4pCKX4LDCEqhi8hYMm
> Content-Length: 174
>
* upload completely sent off: 174 out of 174 bytes
< HTTP/1.1 200 OK
< server: Cowboy
< date: Sun, 23 Oct 2016 20:32:04 GMT
< content-length: 88
< access-control-allow-origin: *
< access-control-allow-headers: origin, x-requested-with, content-type, accept, authorization
< content-type: application/json
<
* Connection #0 to host localhost left intact
{"result":["[wallet bxdMFo]: Error: Wrong amount"],"server_time":"2016-10-23T20:32:04Z"}
```
