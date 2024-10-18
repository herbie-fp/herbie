# Target-Aware Implementation of Real Expressions

This is the artifact for our paper "Target-Aware Implementation of Real Expressions".
In our paper, we presented Chassis,
  a target-aware numerical compiler,
  that compiles mathematical expressions to
  a particular target, including hardware ISAs,
  programming languages, and software libraries.
If you wish to evaluate the artifact,
  please start with the "Getting Started" section
  of this file.
The evaluation takes a couple of hours to run.

## Getting Started

We recommend an x86-64 machine with at least 32 GB of RAM,
  running some Linux distribution.
The machine we used for our evaluation
  used an AMD EPYC 7702 CPU, with 512 GB of RAM,
  running Ubuntu 20.04 LTS.
You will need to install a number of tools and libraries.
We ran our evaluation using `bash`.

### Racket

Install [Racket](https://download.racket-lang.org/) from the official download page.
We recommend at least Racket 8.12.
The `snap` versions of Racket are strongly discouraged
  since they are known to be broken.

### Rust

Install Rust using the [rustup](https://rustup.rs/) installer.
By default, `rustup` installs the newest stable version of Rust.
We used Rust 1.77.2, so it is guaranteed to be newer.
Updating Rust occasionally breaks the Rust dependency for Chassis.
If you experience any problems, you can follow `rustup` documentation
  to install a specific version of Rust.

### Python

Python is installed by default on Linux distributions.
We recommend at least Python 3.10.
If you wish to install a specific version of Python,
  we recommend using [pyenv](https://github.com/pyenv/pyenv)
  as an installer.
We recommend creating a virtual environment
  by running the following series of commands.
Create the virtual environment under `.env` with
```
bash> python -m venv .env/
```
Activate the virtual environment using
```
bash> source .env/bin/activate
```
and install `numpy`, `matplotlib`, and `scipy` packages
```
bash> pip install numpy matplotlib scipy
```

### Julia

Install Julia using the [juliaup](https://julialang.org/downloads/) installer.
Like `rustup`, `juliaup` installs the newest stable version of Rust.
We recommend at least Julia 1.10.

### Clang

We used Clang 14 as our C/C++ compiler.
We recommend installing Clang through your package manager.

### libvdt

The vdt library is a vectorized math library developed at CERN.
Building vdt requires CMake which is usually available through your package manager.
To install, clone the [repo](https://github.com/dpiparo/vdt).
Then navigate to the `vdt` directory and run
```
bash> cmake .
bash> make
bash> make install
```
The final step possibly requires root.

## Testing installed software

To ensure that your environment is properly set up.
Run the following commands and ensure they do not print any errors.

Check that Racket is installed.
```
bash> racket -v
```
Check that Rust is installed.
```
bash> cargo --version
```
Check that Clang is installed.
```
bash> clang -v
```
Check that Python is installed with the proper libraries.
```
bash> python3
>>> import numpy, matplotlib, scipy
>>> exit()
```
Check that Julia is installed.
```
bash> julia -v
```
Check that `libvdt` is installed.
On Linux, run the following command.
It should print a single entry with the path of `libvdt.so`.
```
bash> ldconfig -p | grep libvdt
```

Check that Herbie builds properly.
In this directory, run
```
make install
```
To test that Herbie works, run
```
racket src/herbie.rkt shell
```
Assuming every command works above,
  your system should be set up to run the evaluation.

## Running the evaluation

To start the evaluation, run
```
bash> bash infra/platforms-eval.sh reports 1
```
This command runs the entire evaluation.
The whole process takes 2-3 hours depending the machine.
We recommend using `tmux` so that the process can be detached.
Since the evaluation measures real time,
  we recommend not using any other application while it is running,
  and closing any open application before running it.

## Analyzing the results

If the evaluation runs to completion,
  the `reports` directory should have the following structure
```
reports
|-- platforms
    |-- baseline
    |-- cache
    |-- drivers
    |-- herbie-2.0
    |-- output
        |-- hamming-1
            |-- c-pareto.pdf
        |-- mathematics-1
            |-- c-pareto.pdf
        |-- cost-vs-time.pdf
        |-- baseline-pareto2.pdf
```
All plots are rendered under `reports/platforms/output`.
The evaluation of Chassis contains 3 figures.
Figure 7 corresponds to `hamming-1/c-pareto.pdf` (left)
  and `mathematics-1/c-pareto.pdf` (right).
Figure 8 corresponds to `baseline-pareto2.pdf`.
Figure 9 corresponds to `cost-vs-time.pdf`.
