library(tidyverse)

debugSource("Parse PYF.R")
debugSource("Generate Function.R")

pyf_files <- list.files(path = "INPUT/", pattern = "*.pyf", full.names = TRUE)
options(warn=2)


for (i in pyf_files)
{
  parse_pyf(i)
}

devtools::document()
devtools::build()
