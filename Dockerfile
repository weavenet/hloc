# Very WIP Docker file
FROM haskell:8.4.3
RUN cabal update
WORKDIR /opt/server
COPY ./hloc.cabal /opt/server/hloc.cabal
COPY . /opt/server
RUN cabal install --only-dependencies -j4
RUN cabal install
ENTRYPOINT ["hloc"]
