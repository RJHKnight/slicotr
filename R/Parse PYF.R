source("R/Generate Function.R")

START_SUB <- "^subroutine"
END_SUB <- "^end subroutine"
PARAM <- "::"
INTENT <- "intent"
DIMENSION <- "dimension"

parse_pyf <- function(file_name)
{
  raw <- readLines(file(file_name))

  handling_sub <- FALSE
  sub_name <- ""
  file_name <- ""
  params <- NULL

  for (i in 1:length(raw))
  {
    this_line <- raw[i]

    # Handle Sub def
    if (stringr::str_detect(START_SUB, this_line))
    {
      handling_sub <- TRUE
      file_name_raw <- stringr::str_match(this_line, "\\! in \\:new\\:(.*f)|\\! in (.*f)")
      file_name <- if_else(is.na(file_name_raw[1,2]), file_name_raw[1,3], file_name_raw[1,2])
      file_name <- stringr::str_remove_all(file_name, "\\:.*\\:")

      sub_name <- stringr::str_match(this_line, "^subroutine ([^\\(]*)")[1,2]

    }
    else if (stringr::str_detect(PARAM, this_line))
    {
      if (!handling_sub)
      {
        stop(paste("Error - parameter outside of submodule on line", i))
      }

      # Handle params
    }
    else if (stringr::str_detect(END_SUB, this_line))
    {
      # Generate the R file
      generate_function(sub_name, file_name, params)

      # And reset state.
      sub_name <- ""
      file_name <- ""
      params <- NULL
      handling_sub <- FALSE
    }
  }
}
