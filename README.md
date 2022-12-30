# Bayesian classifier

This is quite naїve implementation of the bayesian classifier for the articles. I made it as an exercise to work on servant, and sqlite-simple. I liked it pretty much. The idea is that it could live as a tiny microservice, for projects like Medium. Who knows

## What to do with it
- ~~Run `cabal run bayes-hs -- +RTS -n` to try out servant's server~~(won't do word normalization)
- run `docker compose build && docker compose up`, it will build and run this server, but it also requires [this repo](https://github.com/bopvlk/ua-word-normalizer) to be dockerized on your machine. That project is a temporary solution until I learn to parse text-files from [this project](https://github.com/brown-uk/dict_uk). ua-word-normalizer just parses the web frontend of dict_uk
- Run `cabal run gen-docs` to generate docs(useful after you made some changes)

## What have I learnt

- Sevant is a nice thing to work with
- You don't need to learn about TVar to add db-connection-pool(I did)
- You can easily plug `resource-pool` instead, and it is a great tool and realize previous point
- If you wanna generate docs that look nice, you better wrap every endpoint's input and output into newtype

## What can be done to make it better.

1. It could have another table of blacklisted words, those would not affect the final result. It would be both more accurate and more efficient.
1. ~~It could have some way to put all the words in the same form(maybe extract a root of the word, or something like that), so that "car" and "cars" would count as one word, or "see" and "saw".~~
1. It could use a real database(sqlite doesn't scale well when you need multiple instances, even though the file could be shared in K8s, I think sqlite is not designed for it).
1. ~~It could use a db-connection-pool to make access quicker.~~ Done
