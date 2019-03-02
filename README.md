# My Blog

## Local Development

```
docker-compose up web
docker-compose run web stack exec migrate-db
```

## Tests

Right now I'm using test-framework and HUnit but I've read good things about using [tasty](https://github.com/feuerbach/tasty)

## Adding Packages

- edit `stack.yml` dependencies list
- run `stack install`

Note: do not edit the generated `.cabal` file

## Deploy

[deploy to heroku via docker registry](https://devcenter.heroku.com/articles/container-registry-and-runtime#building-and-pushing-image-s)

```
heroku container:push web -a haskell-blog
heroku container:release web -a haskell-blog
heroku open -a haskell-blog
```

## TODO

[ ] look into using [release phase](https://devcenter.heroku.com/articles/release-phase) to run migrations as part of deploy
