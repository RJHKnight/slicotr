library(tidyverse)

#debugSource("Parse PYF.R")
#debugSource("Generate Function.R")

pyf_files <- list.files(path = "INPUT/", pattern = "transform.pyf", full.names = TRUE)

for (i in pyf_files)
{
  parse_pyf(i)
}

devtools::document()
devtools::build()
