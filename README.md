# My Blog

## Local Development

```
docker-compose up web
docker-compose run web stack exec migrate-db
```

## Deploy

[deploy to heroku via docker registry](https://devcenter.heroku.com/articles/container-registry-and-runtime#building-and-pushing-image-s)

```
heroku container:push web -a haskell-blog
heroku container:release web -a haskell-blog
heroku open -a haskell-blog
```

## TODO

[ ] look into using [release phase](https://devcenter.heroku.com/articles/release-phase) to run migrations as part of deploy
