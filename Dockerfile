FROM haskell:8.10
WORKDIR /usr/bayes-hs
COPY . ./
RUN apt update
RUN apt install sqlite3
RUN cat bayes.db.sql | sqlite3 bayes.db
RUN cabal update
RUN cabal install --dependencies-only +RTS -M500M
RUN cabal build
EXPOSE 8081
CMD ["cabal", "run", "bayes-hs", "--", "+RTS", "-N"]