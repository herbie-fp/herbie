# Multistage build: https://docs.docker.com/develop/develop-images/multistage-build/
# We build necessary binaries in one image, then COPY them to a second production 
# image to save some space.

# Build image
# Builds output under /herbie/egg-herbie
FROM rust:1.88.0 AS egg-herbie-builder
WORKDIR /herbie
COPY egg-herbie egg-herbie
RUN cargo build --release --manifest-path=egg-herbie/Cargo.toml
RUN cargo install --locked --git https://github.com/egraphs-good/egglog.git --rev 052a330de22d40e9eded19e7f0891c921f7f458c

# Production image
FROM racket/racket:8.17-full AS production
LABEL maintainer="Pavel Panchekha <me@pavpanchekha.com>"
COPY --from=egg-herbie-builder /herbie/egg-herbie /src/egg-herbie
RUN raco pkg install /src/egg-herbie
COPY --from=egg-herbie-builder /usr/local/cargo/bin/egglog /usr/local/bin/egglog
COPY src /src/herbie
RUN raco pkg install --auto /src/herbie
ENTRYPOINT ["racket", "/src/herbie/main.rkt"]
EXPOSE 80
# NOTE --public allows the Docker host to interact with the demo,
# typical users shouldn't need to use it.
CMD ["web", "--public", "--port", "80", "--quiet", "--demo", "--threads", "2"]
