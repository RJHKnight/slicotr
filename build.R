library(tidyverse)

debugSource("Generate Function.R")
debugSource("Parse PYF.R")

pyf_files <- list.files(path = "INPUT/", pattern = "*.pyf", full.names = TRUE)

for (i in pyf_files)
{
  parse_pyf(i)
}

devtools::document()
devtools::build()
