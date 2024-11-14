# Multivariate Analysis
This project is a example of various multivariate analysis using R.

#### -- Project Status: [Archieved]

## Project Intro/Objective
The purpose of this project is to perform various approaches in multivariate analysis within R environment.

### Methods Used
* PCA Analysis
* Factor Analysis
* Clustering Analysis
* MANOVA/MANCOVA Analysis
* Discriminant Analysis
* Ordination Analysis
* SEM


### Technologies
* R


## Getting Started

1. Clone this repo (for help see this [tutorial](https://help.github.com/articles/cloning-a-repository/)).
2. Raw Data is being kept [here](https://github.com/cindyangelira/multivariate-analysis/tree/master/dataa) within this repo.
3. Data processing/transformation scripts are being kept [here](https://github.com/cindyangelira/multivariate-analysis/tree/master/src)
4. Installing dependencies
```
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse", "factoMineR")
ipak(packages)

