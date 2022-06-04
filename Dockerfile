# Multistage build: https://docs.docker.com/develop/develop-images/multistage-build/
# We build necessary binaries in one image, then COPY them to a second production 
# image to save some space.

# Build image
# Builds output under /herbie/egg-herbie
FROM rust:1.61.0 as egg-herbie-builder
WORKDIR /herbie
COPY . .
RUN cargo build --release --manifest-path=egg-herbie/Cargo.toml

# Production image
FROM racket/racket:8.5-full AS production
MAINTAINER Pavel Panchekha <me@pavpanchekha.com>
COPY --from=egg-herbie-builder /herbie/egg-herbie /src/egg-herbie
RUN raco pkg install /src/egg-herbie
ADD src /src/herbie
RUN raco pkg install --auto /src/herbie
ENTRYPOINT ["racket", "/src/herbie/herbie.rkt"]
EXPOSE 80
# NOTE --public allows the Docker host to interact with the demo,
# typical users shouldn't need to use it.
CMD ["web", "--public", "--port", "80", "--quiet", "--demo"]
