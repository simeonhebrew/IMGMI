# Immunomodulatory Gut Microbiota Index (IMGMI)

The IMGM Index is a sample-based scoring metric that allows for estimation of the immunomodulatory properties of microbiota from 
[IgA-SEQ](https://microbiomejournal.biomedcentral.com/articles/10.1186/s40168-020-00992-w) or [MIG-Seq](https://www.nature.com/articles/s41564-024-01887-4) data.

Users can infer both a species' immunomodulatory score and an individual's global immunomodulatory index by considering the relative abundance, prevalence and binding nature of a microbe to an immunoglobulin 
type.


## Installation

IMGMI is available through as an R package. If you haven't yet, you can install R and R Studio [here](https://posit.co/downloads/) before proceeding with installation.

a) Download the `IMGMIndex_0.1.0.tar.gz` file that is provided in the repository

c) In R or R Studio, run:

```
install.packages("IMGMIndex_0.1.0.tar.gz", repos = NULL, type = "source")

```

d) Load the installed package 

```
library(IMGMI)

```

## Usage

