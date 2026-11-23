# Installing `cmdstanr` and CmdStan

This guide explains how to install the R interface **`cmdstanr`**,
install the underlying **CmdStan** software, and verify that both are
working.

> **Important:** Installing the R package `cmdstanr` does not, by
> itself, install CmdStan. You need both components.

## 1. Prerequisites

You need:

-   A recent installation of **R**.
-   **RStudio** (optional, but convenient).
-   A working C++ compiler toolchain.
-   An internet connection for downloading the package and CmdStan.

### Windows

On Windows, install the current version of **Rtools** that matches your
R version. Rtools supplies the compiler tools required to build Stan
models.

Download Rtools from:

<https://cran.r-project.org/bin/windows/Rtools/>

After installation, restart R or RStudio. In a fresh R session, check
whether the build tools are available:

``` r
pkgbuild::check_build_tools(debug = TRUE)
```

If `pkgbuild` is not installed yet, install it first:

``` r
install.packages("pkgbuild")
```

Follow any instructions printed by the check if the compiler is not
detected.

### macOS

Install the Xcode Command Line Tools if they are not already installed.
In Terminal, run:

``` bash
xcode-select --install
```

### Linux

Install the system compiler and development tools required by your
distribution. For example, on Ubuntu or Debian:

``` bash
sudo apt update
sudo apt install build-essential
```

Some distributions may also require additional development libraries.
Consult the CmdStan installation documentation if the build reports a
missing system dependency.

## 2. Install the R package `cmdstanr`

Open R or RStudio and run:

``` r
install.packages("cmdstanr",
                 repos = c("https://stan-dev.r-universe.dev",
                           getOption("repos")))
```

Load the package:

``` r
library(cmdstanr)
```

If R reports that the package is unavailable from the selected
repository, check the official installation page for the current
recommended repository and installation instructions.

## 3. Install CmdStan

`cmdstanr` is the R interface; CmdStan is the backend that compiles and
runs Stan programs. Install CmdStan from R:

``` r
library(cmdstanr)

install_cmdstan()
```

This downloads and builds the current CmdStan release. The first
installation can take several minutes because CmdStan must be compiled.

To install a particular version, specify it explicitly, for example:

``` r
install_cmdstan(version = "2.XX.X")
```

Replace `2.XX.X` with a version listed on the official CmdStan releases
page. Do not copy the placeholder literally.

To choose a custom installation directory:

``` r
install_cmdstan(dir = "C:/Users/your_user_name/.cmdstan")
```

Use a directory that exists or that you have permission to create. The
default location is generally suitable for most users.

## 4. Check the installation

After installation, run:

``` r
cmdstan_version()
cmdstan_path()
```

`cmdstan_version()` should return the installed CmdStan version, and
`cmdstan_path()` should return the directory in which CmdStan was
installed.

You can also run:

``` r
check_cmdstan_toolchain(fix = FALSE)
```

This checks whether the compiler toolchain needed by CmdStan is
configured correctly. If the check reports a problem, follow the
platform-specific instructions in the official CmdStan documentation.

## 5. Compile and run a small test model

The following example writes a small Stan model to a temporary file,
compiles it, and samples from it. It is a practical end-to-end test of
the installation.

``` r
library(cmdstanr)

stan_code <- "
data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  theta ~ beta(1, 1);
  y ~ bernoulli(theta);
}
"

stan_file <- write_stan_file(stan_code)
model <- cmdstan_model(stan_file)

data_list <- list(
  N = 10,
  y = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1)
)

fit <- model$sample(
  data = data_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 123
)

fit$summary()
```

The first compilation may take a little time. If the model compiles and
the sampling run completes, the basic `cmdstanr` setup is working.

## 6. Common installation issues

### CmdStan is not found

Check the installation:

``` r
cmdstan_path()
cmdstan_version()
```

If CmdStan has not been installed, run:

``` r
install_cmdstan()
```

If it is installed in a non-default directory, set the path explicitly:

``` r
set_cmdstan_path("path/to/cmdstan")
```

Use the actual path returned by your installation.

### Compiler or toolchain errors

-   On Windows, check that the Rtools version matches your R version.
-   On macOS, confirm that Xcode Command Line Tools are installed.
-   On Linux, confirm that the compiler and required system development
    libraries are installed.
-   Restart R/RStudio after changing compiler or environment
    configuration.
-   Run `check_cmdstan_toolchain()` again and read the full diagnostic
    output.

### Installation or compilation is interrupted

Try the installation again in a fresh R session. If a previous
incomplete installation is causing problems, inspect the CmdStan
installation directory before removing or replacing anything.

### Package loads, but Stan compilation fails

A successfully loaded `cmdstanr` package does not prove that the C++
toolchain or CmdStan installation is working. Run:

``` r
check_cmdstan_toolchain()
cmdstan_version()
```

Then inspect the compiler output from `cmdstan_model()` for the specific
failure.

## 7. Updating `cmdstanr` and CmdStan

Update the R package using the installation instructions on the official
`cmdstanr` page. To install a newer CmdStan release, use:

``` r
install_cmdstan()
```

If you need to keep an older CmdStan version for reproducibility, record
the version used in your project and avoid replacing it without checking
whether your existing analyses depend on it.

For a reproducible research project, record the versions:

``` r
packageVersion("cmdstanr")
cmdstan_version()
R.version.string
```

## 8. Official documentation

-   `cmdstanr` documentation: <https://mc-stan.org/cmdstanr/>
-   CmdStan installation guide:
    <https://mc-stan.org/docs/cmdstan-guide/installation.html>
-   CmdStan releases: <https://github.com/stan-dev/cmdstan/releases>
-   Rtools for Windows: <https://cran.r-project.org/bin/windows/Rtools/>

------------------------------------------------------------------------

## Quick installation checklist

-   [ ] R is installed.
-   [ ] The platform's C++ build tools are installed.
-   [ ] `cmdstanr` is installed in R.
-   [ ] CmdStan is installed using `install_cmdstan()`.
-   [ ] `cmdstan_version()` returns a version.
-   [ ] `check_cmdstan_toolchain()` passes.
-   [ ] The example Stan model compiles and samples successfully.

**Remember:** `cmdstanr` is the R interface; CmdStan is the software
that compiles and executes Stan programs. Both are required.
