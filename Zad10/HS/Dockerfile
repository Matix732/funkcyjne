FROM haskell:9.4 AS builder
WORKDIR /opt/app

COPY zad10-Haskell.cabal .

RUN cabal update && cabal build --only-dependencies

COPY Main.hs .

RUN cabal install exe:zad10-Haskell --install-method=copy --overwrite-policy=always --installdir=.
RUN mv zad10-Haskell server

FROM ubuntu:22.04

WORKDIR /opt/app

RUN apt-get update && apt-get install -y \
    libgmp10 \
    netbase \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /opt/app/server .

ENV PORT=3000
EXPOSE 3000

CMD ["./server"]