# Obvious
Obvious is a Svbtle clone written in Haskell.

# Advantages
- Speed
- Easy 3-step deployment to Heroku
- Writing Haskell!

# Deploy
1. `gem install heroku`
2. `make setup`
3. `git push heroku master`

# Technology
- Server: Warp
- Framework: Scotty
- Templating: blaze-html
- Database: sqlite on development, postgres on production
- heroku-buildpack-haskell

# Why Warp?
[It's fast](http://www.yesodweb.com/blog/2011/03/preliminary-warp-cross-language-benchmarks).

<insert benchmarks with ab here>

# Why Scotty?
Low learning curve, easy to modify
Easy install -- I've had issues with Yesod ([this bug in particular](https://github.com/yesodweb/yesod/issues/373)). Yesod also installs a lot of packages and might not finish in Heroku's 15 minute
window.

# Why Svbtle? Why not a static site generator?
Familiarity. The goal of this project is to lower the bar for using Haskell for a personal site.
