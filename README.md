
[![Reproducibility](https://github.com/espm-157/fisheries-module-shrestha-chang-huynh-liu/actions/workflows/main.yml/badge.svg)](https://github.com/espm-157/fisheries-module-shrestha-chang-huynh-liu/actions/workflows/main.yml)

## Team Members:

- Riya Shrestha, riya-shrestha
- Briana Huynh, brianahuynh
- Claire Chang, clachang
- Kristin Liu, kristinliu17

This module is exploring the collapse of fisheries over the years and the focus is on understanding and replicating fisheries stock assessment data and fisheries collapse.

## assignment

All work for this assignment should be in the `assignment` directory.  You will work in the `.Rmd` notebook, and commit your rendered output files (`.md` and associated files) in the `assignment` directory as well.

## Special files

All team repositories will also include most of the special files found here:

### Common files

- `README.md` this file, a general overview of the repository in markdown format.  
- `.gitignore` Optional file, ignore common file types we don't want to accidentally commit to GitHub. Most projects should use this. 
- `<REPO-NAME>.Rproj` Optional, an R-Project file created by RStudio for it's own configuration.  Some people prefer to `.gitignore` this file.


### Infrastructure for Testing

- `.travis.yml`: A configuration file for automatically running [continuous integration](https://travis-ci.com) checks to verify reproducibility of all `.Rmd` notebooks in the repo.  If all `.Rmd` notebooks can render successfully, the "Build Status" badge above will be green (`build success`), otherwise it will be red (`build failure`).  
- `DESCRIPTION` a metadata file for the repository, based on the R package standard. It's main purpose here is as a place to list any additional R packages/libraries needed for any of the `.Rmd` files to run.
- `tests/render_rmds.R` an R script that is run to execute the above described tests, rendering all `.Rmd` notebooks. 




