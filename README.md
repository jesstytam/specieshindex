
<img src="README_files/figure-gfm/stickerfile.png" alt="hexsticker" height="250px" align="right" />

# specieshindex

[![CRAN
status](https://www.r-pkg.org/badges/version/specieshindex)](https://CRAN.R-project.org/package=specieshindex)
[![R build
status](https://github.com/jessicatytam/specieshindex/workflows/R-CMD-check/badge.svg)](https://github.com/jessicatytam/specieshindex/actions)
[![](https://codecov.io/gh/jessicatytam/specieshindex/branch/master/graph/badge.svg)](https://codecov.io/gh/jessicatytam/specieshindex)
[![](https://img.shields.io/badge/download-NA/total-blue.svg)](https://bioconductor.org/packages/stats/bioc/specieshindex)

`specieshindex` is a package that aims to gauge scientific influence of
different species mainly using the h-index.

## Installation

To get this package to work, make sure you have the following packages
installed.

``` r
install.packages("rscopus")
install.packages("taxize")
install.packages("XML")
install.packages("httr")
install.packages("dplyr")
install.packages("rlang")
devtools::install_github("jessicatytam/specieshindex", force = TRUE, build_vignettes = TRUE)

library(rscopus)
library(taxize)
library(XML)
library(httr)
library(specieshindex)
```

## Getting Scopus API key

To connect and download citation information from Scopus legally, you
will absolutely need an API key. Here are the steps to obtain the key.

1.  Go to <https://dev.elsevier.com/> and click on the button `I want an
    API key`.
2.  Create an account and log in.
3.  Go to the `My API Key` tab on top of the page and click `Create API
    Key`.
4.  Read the legal documents and check the boxes.

## Simple example

Here is a quick demonstration of how the package works. Let’s say you
want to compare the species h-index of a few marsupials. First, you
would need to download the citation information using either
`FetchSpT()` for title only or `FetchSpTAK()` for
title+abstract+keywords. Remember to use binomial names.

``` r
Woylie <- FetchSpTAK("Bettongia", "penicillata", myAPI)
Quokka <- FetchSpTAK("Setonix", "brachyurus", myAPI)
Platypus <- FetchSpTAK("Ornithorhynchus", "anatinus", myAPI)
Koala <- FetchSpTAK("Phascolarctos", "cinereus", myAPI)
```

Now that you have the data, you can use the `Allindices()` function to
create a dataframe that shows their indices.

``` r
W <- Allindices(Woylie, genus = "Bettongia", species = "penicillata")
Q <- Allindices(Quokka, genus = "Setonix", species = "brachyurus")
P <- Allindices(Platypus, genus = "Ornithorhynchus", species = "anatinus")
K <- Allindices(Koala, genus = "Phascolarctos", species = "cinereus")

CombineSp <- rbind(W, Q, P, K) #combining the citation records
CombineSp
```

    ##              genus_species     species           genus publications citations
    ## 1    Bettongia_penicillata penicillata       Bettongia          113      1903
    ## 2       Setonix_brachyurus  brachyurus         Setonix          242      3427
    ## 3 Ornithorhynchus_anatinus    anatinus Ornithorhynchus          321      6365
    ## 4   Phascolarctos_cinereus    cinereus   Phascolarctos          773     14291
    ##   journals articles reviews years_publishing  h     m i10 h5
    ## 1       55      110       3               43 26 0.605  54  7
    ## 2      107      237       5               66 29 0.439 121  4
    ## 3      153      308      13               67 41 0.612 177  7
    ## 4      227      744      29              139 53 0.381 427 14

Once you are happy with your dataset, you can make some nice plots.
Using `ggplot2`, we can compare the h-index and the total citations.

``` r
#h-index
library(ggplot2)
ggplot(CombineSp, aes(x = species)) +
  geom_point(aes(y = h,
                 colour = "H-index"),
             size = 3) +
  labs(x = "Species",
       y = "Index Score",
       colour = "Index") +
  scale_x_discrete(labels = c("Woylie", "Quokka", "Platypus", "Koala")) +
  scale_colour_manual(values = c("H-index" = "#3498DB")) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")
```

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

**Figure 1.** The *h*-index of the Woylie, Quokka, Platypus, and Koala.

``` r
#total citations
ggplot(CombineSp, aes(x = species)) +
geom_point(aes(y = citations,
               colour = "Citations"),
           size = 3) +
labs(x = "Species",
     y = "Total citations",
     colour = "Index") +
scale_x_discrete(labels = c("Woylie", "Quokka", "Platypus", "Koala")) + 
scale_colour_manual(values = c("Citations"  = "#2874A6")) +
theme(plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "none")
```

<img src="README_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

**Figure 2.** The total number of citations of the publications on the
Woylie, Quokka, Platypus, and Koala.
