# fwt: face with time

## How to run

```sh
$ echo 'FWT_USERS=[{"name":"user1","password":"pass1"}]' > .env
$ docker build --tag bouzuya/fwt .
$ docker run --publish 3000:3000 --env-file .env bouzuya/fwt
```
