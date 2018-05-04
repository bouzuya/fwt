# fwt: face with time

## How to run

```sh
$ echo 'FWT_USERS=[{"name":"user1","password":"pass1"}]' > .env
$ echo 'PORT=3000' >> .env
$ docker build --tag bouzuya/fwt .
$ docker run --publish 3000:3000 --env-file .env bouzuya/fwt
```

## Note

```
$ cp _env .env  # update .env
$ npm i         # run `bower install`, `clean` `build` by `prepare` script.
$ npm run watch # watch files and run dev server
```
