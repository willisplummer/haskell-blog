# My Blog

## Local Dev

```
stack build
stack exec run-server
```

## Docker Development

```
docker-compose up web
docker-compose run web stack exec migrate-db
```

when you make changes just run `docker-compose build`

access the container postgres instance: `docker exec -it myblog_db_1 psql -U myblog`

## Dropping the database

```
docker-compose down
docker-compose build
docker-compose up web
```

## Tests

Right now I'm using test-framework and HUnit but I've read good things about using [tasty](https://github.com/feuerbach/tasty)

## Adding Packages

- edit `stack.yml` dependencies list
- run `stack install`

Note: do not edit the generated `.cabal` file

NOTE: once you add packages, you'll probably want to publish a new docker image so that you don't have to go through the install step everytime you run `docker-compose build`:

```
docker-compose build
docker tag myblog_web willisplummer/haskell-blog:latest
docker push willisplummer/haskell-blog:latest
```

## Deploy

[deploy to heroku via docker registry](https://devcenter.heroku.com/articles/container-registry-and-runtime#building-and-pushing-image-s)

```
heroku container:login
heroku container:push web -a haskell-blog
heroku container:release web -a haskell-blog
heroku open -a haskell-blog
```

## TODO

[ ] look into using [release phase](https://devcenter.heroku.com/articles/release-phase) to run migrations as part of deploy

- login emails shouldnt be case sensitive

## API

_unauthenticated routes_
POST `/signup` - create new user
POST `/login` - authenticate

_authenticated routes_
GET `/judgeables` - get judgeables index
GET `/judgeables/id` - get judgeable by id
POST `/judgeables` - create new judgeable
POST `/judgeables/id/judgements` - create judgement for judgeable item
GET `/users` - get users
GET `/users/id/judgements` - get a user's judgements
POST `/users/id/subscribe` - create a subscription to a user's judgements

## Example Curl Commands

### signup

curl --header "Content-Type: application/json; charset=utf-8" --verbose --data '{"email": "willisplummer@gmail.com", "password": "password", "username": "wmp224"}' https://e6cdfc21.ngrok.io/signup

### login

curl --header "Content-Type: application/json; charset=utf-8" --verbose --data '{"email": "willisplummer@gmail.com", "password": "password"}' https://e6cdfc21.ngrok.io/login
