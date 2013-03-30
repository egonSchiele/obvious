setup:
	heroku create --stack=cedar --buildpack https://github.com/pufuwozu/heroku-buildpack-haskell.git
	heroku addons:add heroku-postgresql:dev
	heroku config:add ENV=production
	dbname=$(heroku config | grep POSTGRES | cut -d: -f1)
	heroku pg:promote $dbname
all:
	cabal install
run:
	PORT=3012 .virthualenv/cabal/bin/obvious
