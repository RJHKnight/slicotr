library(tidyverse)

source("Generate Function.R")
source("Parse PYF.R")

pyf_files <- list.files(path = "INPUT/", pattern = "*.pyf")

for (i in pyf_files)
{
  parse_pyf(i)
}

devtools::document()
devtools::build()
