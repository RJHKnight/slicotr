generate_function <- function(name, file_name, params)
{
  output_name <- paste0(name, ".R")

  param_names <- filter(params, stringr::str_detect(intent, "in") | is.na(intent)) %>% pull(name)

  x <- paste0(

    # Function definition
    name, "<- function(",
    paste(param_names, collapse = ","),
    ")\n",
    "{\n\n",

    # Type coercion for in parameters
    "# In Parameters", "\n",
    paste0(handle_in(params), collapse = "\n"), "\n\n",

    # Define hidden parameters
    "# Hidden Parameters", "\n",
    paste0(handle_hide(params), collapse = "\n"), "\n\n",

    # Define out parameters
    "# Out Parameters", "\n",
    paste0(handle_out(params), collapse = "\n"), "\n\n",

    # Apply checks
    "# Check dimensions of input parameters", "\n",
    paste0(check_in(params), collapse = "\n"), "\n\n",

    # Call Fortran
    create_call(file_name, params), "\n\n",

    # Return list
    create_return(params) , "\n}"
  )

  file_name <- paste0("R/", name, "_tmp.R")
  readr::write_file(x, file = file_name)
  formatR::tidy_file(file_name)
}

check_in <- function(params)
{
  filt_params <- dplyr::filter(params, stringr::str_detect(intent, "in") & !is.na(dimension))

  if (nrow(filt_params) == 0)
    return ("")

  filt_params %>%
    mutate(text = paste0(
      "if (dim (as.array(", name, ")) != c(", dimension, ")) stop(\"Incorrect dimensions for matrix ", name, "\")"
    )) %>%
    pull(text)
}

create_return <- function(params)
{
  out_names <- params %>%
    filter(stringr::str_detect(intent, "out")) %>%
    pull(name)

  return (
    paste0(
      "return (list(",
      paste0(out_names, " = res$", stringr::str_to_upper(out_names), collapse = ","),
      "))"
    )
  )
}

create_call <- function(file_name, params)
{
  param_string <- params %>%
    mutate(text = paste0(stringr::str_to_upper(name), "=", name)) %>%
    pull(text) %>%
    paste(collapse = ",")

  return (
    paste0(
      "res <- .Fortran(\"",
      stringr::str_split_fixed(file_name, "\\.", 2)[1,1],
      "\",", param_string,
      ")"
    )
  )
}

handle_out <- function(params)
{
  out_params <- dplyr::filter(params, intent == "out")

  if (nrow(out_params) == 0)
    return ("")

  out_params %>%
    mutate(text = paste0(
      name, " <- ",
      if_else(is.na(dimension),
              paste0("as.", type, "(0)"),
              paste0("array(as.", type, "(0), c(", dimension, "))"))
    )) %>% pull(text)
}

handle_in <- function(params)
{
  in_params <- dplyr::filter(params, stringr::str_detect(intent, "in") | is.na(intent))

  if (nrow(in_params) == 0)
    return ("")

  text <- in_params %>%
    mutate(text = paste0(name, " <- as.", type, "(", name, ")")) %>%
    pull(text)

  return (text)
}

handle_hide <- function(params)
{
  hide_params <- dplyr::filter(params, stringr::str_detect(intent,"hide"))

  if (nrow(hide_params) == 0)
    return ("")

  hide_params %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = handle_shape_dimension(value, dimension, type)) %>%
    dplyr::mutate(
      text = paste0(name, " <- " , value)
    ) %>%
    pull(text)
}

handle_shape_dimension <- function(value, dimension, type)
{
  if (nchar(value) == 0 & nchar(dimension) == 0)
    return ("")

  if (nchar(value) == 0)
  {
    # Use dimension
    return (paste0("array(as.", type, "(1), c(", dimension, "))"))
  }

  if (!stringr::str_detect(value, "shape"))
  {
    return (value)
  }

  bits <- stringr::str_split_fixed(value, "\\(|\\,|\\)", 4)

  return (paste0("dim(", bits[,2], ")[", as.integer(bits[,3])+1, "]"))
}
