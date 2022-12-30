FROM haskell:8.10
WORKDIR /usr/bayes-hs

COPY bayes.db.sql ./
RUN apt update
RUN apt install sqlite3
RUN cat bayes.db.sql | sqlite3 bayes.db

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./bayes-hs.cabal ./

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . .
RUN cabal install
RUN cabal build
EXPOSE 8081
CMD ["cabal", "run", "bayes-hs", "--", "+RTS", "-N"]