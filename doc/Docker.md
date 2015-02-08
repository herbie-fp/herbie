
Running Herbie in Docker
========================

Herbie can be run in a [Docker](https://docker.org) container,
which simplifies the task of running Herbie.

First, install Docker on your machine,
following [instructions](https://docs.docker.com/installation/)
on the official Docker site.
Docker supports Windows, OS X, and Linux.

Depending on how you installed Docker,
you may need to prefix all `docker` invocations with `sudo`
or otherwise execute them as root.

Then, download the `uwplse/herbie` image:

    docker pull uwplse/herbie

You can now run Herbie:

    docker run -it uwplse/herbie

This runs Herbie’s default configuration,
which executes Herbie on the NMSE benchmarks
and produces a report, which is hidden away in the bowels of Docker.

More useful is to use your own benchmarks and view the graphs.
To do this, create two folders on your host machine:

    mkdir /path/to/graphs /path/to/bench

and mount them as Docker volumes:

    docker run -it -v /path/to/graphs:/herbie/graphs/ -v /path/to/bench/:/herbie/bench/ uwplse/herbie bench

Note that two volumes are mounted,
and `bench` is passed as a parameter
(otherwise Herbie will search for a `bench/hamming` folder).
