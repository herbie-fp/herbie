FROM racket/racket:8.2-full
MAINTAINER Pavel Panchekha <me@pavpanchekha.com>
ADD src /src/herbie
RUN raco pkg install --auto /src/herbie
ENTRYPOINT ["racket", "/src/herbie/herbie.rkt"]
CMD ["web", "--port", "80", "--quiet", "--demo"]
