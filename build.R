library(tidyverse)

source("Parse PYF.R")
source("Generate Function.R")

pyf_files <- list.files(path = "INPUT/", pattern = "*.pyf", full.names = TRUE)
options(warn=1)


for (i in pyf_files)
{
  parse_pyf(i)
}

devtools::document()
devtools::build()
