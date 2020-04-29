{{{preamble}}}
# {{{package}}}

<!-- badges: start -->
<!-- badges: end -->

{{{package}}} is an example research compendium, created by [`MATSS`](https://weecology.github.io/MATSS/), version {{{version}}}; ({{{citation_txt}}}).

## File/Folder Structure

The organization of this project follows the recommendations for a research compendium (Marwick, Boettiger, Mullen 2018 "Packaging Data Analytical Work Reproducibly Using R (and Friends)").

The initial contents are:

```
{{{package}}}
├── {{{package}}}.Rproj
├── DESCRIPTION
{{{license_file_contents}}}├── NAMESPACE
├── README.md
├── R
│   └── analysis_functions.R
└── analysis
    ├── pipeline.R
    ├── references.bib
    └── report.Rmd
```

with the `R` subfolder containing function definitions for the data analysis, and the `analysis` subfolder containing the files for the analysis itself:
* `analysis/pipeline.R` defines the [`drake`](https://docs.ropensci.org/drake/) workflow
* `analysis/report.Rmd` is the Rmarkdown file containing the results of the analysis
* `analysis/references.bib` contains the references for `report.Rmd` and is instantiated with one reference for the `MATSS` package.

## Instructions

0. (optional) **Choose an alternative license.**  
The default option with a new compendium is to use the permissive MIT License. However, if you declined to go that route during compendiumc reation or want to use a different license, see https://usethis.r-lib.org/reference/licenses.html for descriptions and other examples:
```r
?usethis::use_mit_license()
```

1. **Install the package.**  
If you're in RStudio, you can use the "Build and Install" button, found in the "Build" pane: https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio  
Otherwise, using the **`devtools`** package:
```r
devtools::install(".") # in the working directory of this compendium
```

2. **Restart R.**
After installing the new package, you will need to restart R. (If you are working within RStudio, this should happen automatically if you use the "Build and Install" button.)

3. **Source the analysis script.**
`analysis/pipeline.R` contains the main workflow. Within RStudio, you can use the "Source" button and/or the keyboard shortcut (<kbd>Control</kbd> + <kbd>Shift</kbd> + <kbd>s</kbd>; or <kbd>Command</kbd> + <kbd>Shift</kbd> + <kbd>s</kbd> on Mac).
Otherwise, within an R console:
```r
source("analysis/pipeline.R")
```

4. **Examine the compiled report.**  
`analysis/report.Rmd` contains the Rmarkdown source for reporting and plotting the results of the analysis.  
After compilation, There should be an html version (`analysis/report.html`) that is viewable within a web browser directly, and a markdown version (`analysis/report.md`) that is viewable within a text editor, but shows formatting when uploaded to GitHub.

## Downloading additional datasets

`MATSS` provides access to many additional datasets through [retriever](https://www.data-retriever.org/), which downloads the datasets directly to your machine. The below steps are intended to provide the shortest path to a working setup, though you may need additional configurtion if you have an existing python setup.

1. **Activate Python** and make it available within R (via the `reticulate` R package).  
```r
library(reticulate)
py_available(initialize = TRUE)
```

2. **Install `retriever`.**  
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
# development version on github:
# py_install("git+git://github.com/weecology/retriever@master", pip = TRUE)
```

3. **Update retriever scripts for downloading data.**  
```r
rdataretriever::get_updates()
```

4. **Configure a default location to store datafiles.**  
```r
MATSS::use_default_data_path(<path>) # e.g. <path> = "~/data"
```

5. **Download datasets.**  
If you perfomed the setting in step 4, be sure to restart R!  
We provide a function, `download_datasets()`, which can be used to download some or all of the supported datasets:  
```r
MATSS::download_datasets()
```

## Adding your own analyses

1. **Write your own analysis functions.**  
You can add these to the file `R/analysis_functions.R` or add additional `.R` files in that folder.  
If you use other packages in these methods, be sure to include them among the dependencies for your compendium with `usethis::use_package()`.  
To add documentation you can follow the existing examples, or read more in detail at http://r-pkgs.had.co.nz/man.html#man-functions

2. **Update the pipeline script.**
In `analysis/pipeline.R`, you will want to modify the `methods` drake plan to include the new functions you just added. (and possibly replacing the existing ones)

3. **Add code to process and plot the results.**
In `analysis/report.Rmd`, you will want to modify the code to read in the new results, process the output, and construct plots.

## Set up version control

`MATSS` compendia are designed to be easily distributible via code-sharing platforms, such as Github. An example compendium, built using the latest version of `MATSS` can be found at https://github.com/weecology/MATSSdemo.

A guide to setting up Git and getting the compendium uploaded to Github is in [this chapter](http://r-pkgs.had.co.nz/git.html) of Hadley Wickham's book on R packages.

For a more extensive resource on using Git and Github as part of your coding workflow, see [Happy Git and GitHub for the useR](https://happygitwithr.com/).