# Piano Reps

Super-simple API for tracking piano exercises, using Servant and ~sqlite-simple~ Persistent

## Writeups

I've written a couple blog posts involving this code.

- [Building a nicely-typed lightweight database-backed API with Haskell, Servant, and Sqlite
](https://vitez.me/light-db-api) (a mouthful!) is about putting together the sqlite-simple version of this. You can see that code in `BlogPost.hs`
- [Using Persistent with Servant](https://vitez.me/persistent-servant) is about supporting Persistent instead of sqlite-simple, and demonstrates a bit of monad transformer madness. That code is currently in `Main`.

## Usage

Run `stack run` to run the server, default port is 1810

Test that it's working with `curl localhost:1810/list`

Populate a random new record with `curl localhost:1810/populate`
