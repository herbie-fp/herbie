FROM jackfirth/racket
MAINTAINER Pavel Panchekha <me@pavpanchekha.com>
RUN apt-get update \
 && apt-get install -y libcairo2-dev libjpeg62 libpango1.0-dev \
 && rm -rf /var/lib/apt/lists/*
RUN raco pkg install --auto git://github.com/uwplse/herbie?path=herbie
ENTRYPOINT ["raco", "herbie"]
