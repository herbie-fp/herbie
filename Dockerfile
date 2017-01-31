FROM jackfirth/racket:6.8
MAINTAINER Pavel Panchekha <me@pavpanchekha.com>
ADD src /src/herbie
RUN raco pkg install --auto /src/herbie
ENTRYPOINT ["raco", "herbie"]
