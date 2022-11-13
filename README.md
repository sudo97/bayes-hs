# Bayesian classifier
This is quite naїve implementation of the bayesian classifier for the articles. I made it as an exercise to work on servant, and sqlite-simple. I liked it pretty much. The idea is that it could live as a tiny microservice. Who knows

## What can be done to make it better.

1. It could have another table of blacklisted words, those would not affect the final result. It would be both more accurate and more efficient.
1. It could have some way to put all the words in the same form(maybe extract a root of the word, or something like that), so that "car" and "cars" would count as one word, or "see" and "saw".
1. It could use a real database(sqlite doesn't scale well when you need multiple instances, even though the file could be shared in K8s, I think sqlite is not designed for it).
1. It could use a db-connection-pool to make access quicker.