FROM fpco/stack-build:lts-20.12 as builder
WORKDIR /build
ADD circuit-sim.cabal package.yaml stack.yaml stack.yaml.lock ./
RUN stack setup
ADD ./app ./app
ADD ./src ./src
RUN stack --local-bin-path /build install

FROM alpine
RUN apk add gcompat gmp
COPY --from=builder /build/circuit-sim-exe /app/circuit-sim
ENTRYPOINT ["/app/circuit-sim"]
