FROM jackfirth/racket
MAINTAINER Pavel Panchekha <me@pavpanchekha.com>
ADD . /herbie/
VOLUME ["/herbie/graphs/", "/herbie/bench/"]

WORKDIR /herbie/
RUN raco make herbie/reports/run.rkt

ENTRYPOINT ["racket", "herbie/reports/make-report.rkt"]
