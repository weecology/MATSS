{{{preamble}}}
# {{{package}}}

<!-- badges: start -->
<!-- badges: end -->

{{{package}}} is an example research compendium, created by [MATSS](https://weecology.github.io/MATSS/), version {{{version}}}; ({{{citation_txt}}}).

## File/Folder Structure

The organization of this project follows the recommendations for a research compendium (Marwick, Boettiger, Mullen 2018 "Packaging Data Analytical Work Reproducibly Using R (and Friends)").

The initial contents are:

```
{{{package}}}
├── {{{package}}}.Rproj
├── DESCRIPTION
├── LICENSE.md (if created)
├── LICENSE    (if created)
├── NAMESPACE
├── README.md
├── R
│   └── analysis_functions.R
└── analysis
    ├── pipeline.R
    ├── references.bib
    └── report.Rmd
```

with the `R` subfolder containing function definitions for the data analysis, and the `analysis` subfolder containing the files for the analysis itself:
* `pipeline.R` defines the [drake](https://docs.ropensci.org/drake/) workflow
* `report.Rmd` is the Rmarkdown file containing the results of the analysis
* `references.bib` contains the references for `report.Rmd` and is instantiated with one reference for the `MATSS` package.

## Instructions

0. (optional) **Choose an alternative license.**  
The default option with a new compendium is to use the permissive MIT License. However, if you declined to go that route during compendiumc reation or want to use a different license, see https://usethis.r-lib.org/reference/licenses.html for descriptions and other examples:
```r
?usethis::use_mit_license()
```

1. **Install the package.**  
If you're in RStudio, you can use the interactive "Build" pane: https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio  
Otherwise, using the **`devtools`** package:
```r
devtools::install(".") # in the working directory of this compendium
```

2. **Source the analysis script.**
As described above, `analysis/pipeline.R` contains the main workflow. Within RStudio, you can use the "Source" button and/or the keyboard shortcut (<kbd>Control</kbd> + <kbd>Shift</kbd> + <kbd>s</kbd>; or <kbd>Command</kbd> + <kbd>Shift</kbd> + <kbd>s</kbd> on Mac).
Otherwise, within an R console:
```r
source("analysis/pipeline.R")
```

3. **Examine the compiled report.**  
As described above, `analysis/report.Rmd` contains the Rmarkdown source for the results. As a result of running the pipeline script, the report should be compiled.  
There should be an html version (`analysis/report.html`) that is viewable within a web browser directly, and a markdown version (`analysis/report.md`) that is viewable within a text editor, but shows formatting when uploaded to GitHub.

## Downloading additional datasets

Access to most time series is provided through [retriever](https://www.data-retriever.org/), which downloads the datasets directly to your machine. Depending on your con

1. Activate Python and make it available within R (via the `reticulate` R package).  
```r
library(reticulate)
py_available(initialize = TRUE)
```

2. Install `retriever`.  
```r
if (!py_module_available("retriever"))
{
    py_install("retriever")
}
```

If this fails, then it may be easier to setup a miniconda environment for installing `retriever` instead.  
```r
install_miniconda()
py_install("retriever")
```

3. Update scripts for downloading retriever datasets.  
```r
rdataretriever::get_updates()
```

4. Configure a default location to store datafiles.  
```r
MATSS::use_default_data_path(<path>) # e.g. <path> = "~/data"
```

5. Download datasets.

If you perfomed the setting in step 4, be sure to restart R!

```r
MATSS::download_datasets()
```

## Adding your own analyses

1. **Write your own analysis functions.**  
You can add these to the file `R/analysis_functions.R` or add additional `.R` files in that folder.
To add documentation, you can follow the examples, or read more in detail at http://r-pkgs.had.co.nz/man.html#man-functions

2. **Update the pipeline script.**
In `analysis/pipeline.R`, you will want to modify the `methods` drake plan to include the new functions you just added. (possibly replacing the existing ones)

3. **Add code to process and plot the results.**
In `analysis/report.Rmd`, you will want to modify the code to read in the new results, process the output, and construct plots.
