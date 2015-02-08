FROM asumu/racket-head
MAINTAINER Pavel Panchekha <me@pavpanchekha.com>
ADD . /herbie/
VOLUME ["/herbie/graphs/", "/herbie/bench/"]

WORKDIR /herbie/
RUN raco make herbie/reports/make-report.rkt

CMD ["bench/hamming"]
ENTRYPOINT ["racket", "herbie/reports/make-report.rkt"]