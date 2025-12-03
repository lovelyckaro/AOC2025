FROM harbor.lyckaro.se/proxy/library/haskell:9.12.2-slim-bookworm AS build

RUN cabal update

WORKDIR /opt/aoc
COPY ./aoc2025.cabal ./
# Download and build deps
RUN cabal build --dependencies-only -j
COPY . ./
RUN cabal install --install-method=copy --installdir=./out

FROM harbor.lyckaro.se/proxy/library/debian:bookworm-slim
COPY --from=build /opt/aoc/out/aoc /usr/bin/aoc
RUN chmod +x /usr/bin/aoc
ENTRYPOINT [ "/usr/bin/aoc", "--serve" ]
