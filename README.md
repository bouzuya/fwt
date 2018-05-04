# fwt : face with time

A small web application using PureScript to share face and time.

Inspired by ~~[Sqwiggle](https://sqwiggle.com/)~~ and [sneek](https://sneek.io/).

Roughly speaking, `setInterval(() => takePhotoOfYourFaceAndShare(), 300000);`. In other words, this is sneek.io without talking feature.

## How to run

```sh
$ cp _env .env
$ docker build --tag bouzuya/fwt .
$ docker run --publish 3000:3000 --env-file .env bouzuya/fwt
```

## Note

```
$ cp _env .env  # update .env
$ npm i         # run `bower install`, `clean` `build` by `prepare` script.
$ npm run watch # watch files and run dev server
```
