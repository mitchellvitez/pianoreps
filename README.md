# Piano Reps

Super-simple API for tracking piano exercises, using Servant and ~sqlite-simple~ Persistent

Wrote a quick [blog post](https://vitez.me/light-db-api) about putting together the sqlite-simple version of this. You can see that code in `BlogPost.hs`

## Usage

Run `stack run` to run the server, default port is 1810

Test that it's working with `curl localhost:1810/list`

Populate a random new record with `curl localhost:1810/populate`
