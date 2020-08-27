---
output: github_document
---

![](C:/Users/iamje/Jess/UNSW/BEES0006/Animals/Banner.png)

# specieshindex

`specieshindex` is a package that aims to guage scientific influence mainly using the h-index.

## Installation

To get this package to work, make sure you have the following packages installed.

```{r, eval=FALSE}
install.packages("rscopus")
install.packages("taxize")
install.packages("XML")
install.packages("httr")
install.packages("dplyr")
install.packages("rlang")
devtools::install_github("jessicatytam/specieshindex", force = TRUE, build_vignettes = TRUE)

library(taxize)
library(httr)
library(XML)
library(rscopus)
library(specieshindex)
```

## Getting Scopus API key

To connect and download citation information from Scopus legally, you will absolutely need an API key. Here are the steps to obtain the key.
1. Go to https://dev.elsevier.com/ and click on the button `I want an API key`.
2. Create an account and log in.
3. Go to the `My API Key` tab on top of the page and click `Create API Key`.
4. Read the legal documents and check the boxes. 

## Simple example

Here is a quick demonstration of how the package works. Let's say you want to compare the species h-index of a few marsupials. First, you would need to download the citation information using either `FetchSpT()` for title only or `FetchSpTAK()` for title+abstract+keywords. Remember to use binomial names.

```{r, eval=FALSE}
Woylie <- FetchSpTAK("Bettongia", "penicillata", myAPI)
Quokka <- FetchSpTAK("Setonix", "brachyurus", myAPI)
Platypus <- FetchSpTAK("Ornithorhynchus", "anatinus", myAPI)
Koala <- FetchSpTAK("Phascolarctos", "cinereus", myAPI)
```

Now that you have the data, you can use the `Allindices()` function to create a dataframe that shows their indices.

```{r}
W <- Allindices(Woylie, genus = "Bettongia", species = "penicillata")
Q <- Allindices(Quokka, genus = "Setonix", species = "brachyurus")
P <- Allindices(Platypus, genus = "Ornithorhynchus", species = "anatinus")
K <- Allindices(Koala, genus = "Phascolarctos", species = "cinereus")

CombineSp <- rbind(W, Q, P, K) #combining the citation records
CombineSp
```

If you have a much larger dataset and want to add classification information, you can use `Addranks()` to do this.

```{r}
CombineSp <- Addranks(CombineSp)
CombineSp
```

Once you are happy with your dataset, you can make some nice plots. Here, I will being using `ggplot2` to plot and compare the h-index and the total citations.

```{r, fig.height=6, fig.width=7, fig.align='center'}
library(ggplot2)
#h-index
ggplot(CombineSp, aes(x = species)) +
  geom_point(aes(y = h,
                 colour = "H-index"),
             size = 6) +
  labs(x = "Species",
       y = "Index Score",
       colour = "Index",
       title = "h-index") +
  scale_x_discrete(labels = c("Woylie", "Quokka", "Platypus", "Koala")) +
  scale_colour_manual(values = c("H-index" = "#3498DB")) +
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

#total citations
ggplot(CombineSp, aes(x = species)) +
geom_point(aes(y = citations,
               colour = "Citations"),
           size = 6) +
labs(x = "Species",
     y = "Total citations",
     colour = "Index",
     title = "Citations") +
scale_x_discrete(labels = c("Woylie", "Quokka", "Platypus", "Koala")) + 
scale_colour_manual(values = c("Citations"  = "#2874A6")) +
theme(plot.title = element_text(size = 24, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16))
```
