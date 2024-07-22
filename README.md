
<!-- README.md is generated from README.Rmd. Please edit that file -->

# btbr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of {btbr} is to provide a reproducible package, model pipeline
and documentation for the Western Montana Bull Trout Baseline (BTB)
model.
<figure>
<img src='https://hagadone.media.clients.ellingtoncms.com/img/photos/2023/05/11/MM7783_090918_23706_t1170.jpg?5cc718665ab672dba93d511ab4c682bb370e5f86' alt="Photo of a few Bull Trout underwater.">
<figcaption>
Bull trout. (Photo courtesy of the U.S. Geological Survey)
</figcaption>
</figure>
<h2 style="color:red;">
Attention!
</h2>

**Please read disclaimer and understand that this is in heavy
development. Thank you!**

## Installation

You can install the development version of btbr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("soilwaterfish/btbr")
```

The package has some functionality beyond the BTB but is mostly for
reproducing in a data pipeline using the
[{targets}](https://github.com/ropensci/targets) framework. The data
pipeline provides reproducible workflows while in combination with the
{btbr} package to generate a Bayesian Network model output.

- You can see the {targets} style pipeline in the `_targets.R` script.

<figure>
<img src='inst/www/bn.png' alt="Photo of a few Bull Trout Baseline Bayesian Network">
<figcaption>
Bull Trout Baseline Bayesian Network with previous model logic.
</figcaption>
</figure>

# Disclaimer

This information is preliminary or provisional and is subject to
revision. It is being provided to meet the need for timely best science.
The information has not received final approval by the U.S. Department
of Agriculture (USDA) and is provided on the condition that neither the
USDA nor the U.S. Government shall be held liable for any damages
resulting from the authorized or unauthorized use of the information.

Although this software program has been used by the USDA, no warranty,
expressed or implied, is made by the USDA or the U.S. Government as to
the accuracy and functioning of the program and related program material
nor shall the fact of distribution constitute any such warranty, and no
responsibility is assumed by the USDA in connection therewith. This
software is provided “AS IS.”

[![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)](https://creativecommons.org/publicdomain/zero/1.0/)
