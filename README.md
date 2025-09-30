# PiProteline: Pipeline analysis for Proteomics

**PiProteline** is an R package designed to simplify the analysis of proteomics data.  
It provides a set of modular functions for **protein mapping**, **quantitative, network and functional analysis**, as well as a **fully automated pipeline** that streamlines the entire workflow from raw input data to annotated outputs in Excel.

---

## ✨ Main features
- Functions for:
  - **Protein mapping and annotation**: retrieve functional descriptions, biological processes, and pathway information.
  - **Preprocessing data**: retrieve functional descriptions, biological processes, and pathway information.
  - **Quantitative analysis**: perform Multi Dimensional Scaling and Multivariate ANOVA.
  - **Network analysis**: search for critical nodes important in the topology of Protein-Protein interaction network.
  - **Functional analysis**: analyze your key proteins using enrichment analysis.
  - **Export utilities**: generate visual outputs and Excel reports.
- **Complete pipeline**.
- Publication-ready outputs.

---

## 🔧 Installation

Install the development version from GitHub:

```r

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("ggtreeExtra")

# install.packages("devtools")
devtools::install_github("lomi95/PiProteline")
```

---

## ⚙️STRING-db interactome (required before network analysis)
Before running network analysis or the `pipeline()` that uses a protein interaction network, you must download the species interaction network from **STRING-db**.

1. Go to: https://string-db.org/cgi/download  
2. Choose the species (e.g., **Homo sapiens**)  
3. Download **“protein network data (full network, incl. subscores per channel)”**  
Example direct link for downloading [Homo sapiens interactome](https://stringdb-downloads.org/download/protein.links.detailed.v12.0/9606.protein.links.detailed.v12.0.txt.gz)


```r
library(PiProteline)
library(tidyverse)

# Path to the downloaded STRING file (.gz)
destfile <- "downloads/9606.protein.links.detailed.v12.0.txt.gz"

# Build interactome and convert to igraph object
interactome_hs <- build_interactome(directory_interactome = destfile, tax_ID = 9606) %>%
  filter_interactome(scores_threshold = c(experimental = 150, database = 300)) %>%
  dplyr::select(3, 4) %>%
  igraph::graph_from_data_frame(directed = FALSE)

# Assign to g_interactome (required by pipeline)
g_interactome <- interactome_hs

```

## 🔁 Running the pipeline
### Read your dataset
your_dataset <- openxlsx::read.xlsx("dataset_directory.xlsx")

### Define your groups 
#### (must match the names in your dataset, e.g. each colnames should have Group1, Group2 or Group3 within the name)
names_of_myGroups <- c("Group1", "Group2", "Group3")


### Run the pipeline

```r

pipelineResults <- pipeline(
  dataset = your_dataset,
  names_of_groups = names_of_myGroups,
  g_interactome = g_interactome,
  gene_column = 1,                 # index (or name) of the gene/protein column
  save_results_as = "pipelineTest" # folder that will be created with all results
)
```

**⚠️ For big datasets the waiting time could be 20-30 minutes, especially for network analysis**


