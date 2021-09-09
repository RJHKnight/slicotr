source("Documentation.R")

generate_function <- function(name, file_name, params)
{
  if (is.na(file_name) || str_detect(file_name, ".*Z.f"))
  {
    cat(paste("Skipping complex function", file_name, "\n"))
    return ()
  }

  output_name <- paste0(name, ".R")

  param_names <- filter(params, stringr::str_detect(intent, "in") | is.na(intent)) %>% arrange(original_order) %>% pull(name)

  x <- paste0(

    # Roxygen
    generate_docs(file_name, name),

    # Function definition
    name, "<- function(",
    paste(param_names, collapse = ","),
    ")\n",
    "{\n\n",

    # Type coercion for in parameters
    "# In Parameters", "\n",
    paste0(handle_in(params), collapse = "\n"), "\n\n"
  )

  other_params <- dplyr::filter(params, !(stringr::str_detect(intent, "in") | is.na(intent)))

  for (i in 1:nrow(other_params))
  {
    this_param <- other_params[i,]

    if (is_out(this_param))
    {
      # Define out parameters
      x <- paste0(x, handle_out(this_param), "\n")
    }

    else
    {
      # Define hidden parameters
      x <- paste0(x, handle_hide(this_param), "\n")
    }
  }

  # Apply checks
  #"# Check dimensions of input parameters", "\n",
  #paste0(check_in(params), collapse = "\n"), "\n\n",

  x <- paste0(x, "\n\n",

              # Call Fortran
              create_call(file_name, params), "\n\n",

              # Return list
              create_return(params) , "\n}"
  )

  # Handle reserved keywords
  x <- stringr::str_replace_all(x, "(?<=(\\,|\\s))na(?=(\\,|\\s))", "na_")
  x <- stringr::str_replace_all(x, "(?<=(\\,|\\s|\\$))NA(?=(\\,|\\s|\\=))", "\\`NA\\`")
  x <- stringr::str_remove_all(x, "(?<=complex)\\*[0-9]*")
  x <- stringr::str_replace_all(x, "\\`\\?\\`", "ifelse")

  file_name <- paste0("R/", name, ".R")
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
      "if (dim (", name, ") != c(", dimension, ")) stop(\"Incorrect dimensions for matrix ", name, "\")"
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
    arrange(original_order) %>%
    mutate(text = paste0(stringr::str_to_upper(name), "=", name)) %>%
    pull(text) %>%
    paste(collapse = ",")

  return (
    paste0(
      "res <- suppressWarnings(.Fortran(\"",
      stringr::str_split_fixed(file_name, "\\.", 2)[1,1],
      "\",", param_string,
      "))"
    )
  )
}

is_out <- function(param)
{
  this_intent <- param$intent[1]
  return (!is.na(this_intent) && this_intent == "out")
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
  in_params <- dplyr::filter(params, stringr::str_detect(intent, "in") | is.na(intent)) %>%
    filter(is.na(dimension)) %>%
    arrange(!is.na(depend), name, depend)

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
  if (nchar(value) == 0 & (nchar(dimension) == 0 | is.na(dimension)))
    return ("")

  if (nchar(value) == 0)
  {
    # Use dimension
    return (paste0("array(as.", type, "(1), c(", dimension, "))"))
  }

  if (!stringr::str_detect(value, "shape"))
  {
    # If/else in value.
    if (stringr::str_detect(value, "\\?"))
    {
      bits <- stringr::str_match(value, "\\(([^\\?]*)\\?([^\\:]*)\\:(.*)\\)")
      value <- paste0("ifelse(", bits[1,2], ",", bits[1,3], ",", bits[1,4], ")")
    }

    return (paste0("as.", type, "(", value, ")"))
  }

  obj_index <- str_extract(value, "(?<=shape\\()[^\\)]*")
  bits <- stringr::str_split_fixed(obj_index, ",", 2)
  dim_string <- paste0("dim(", bits[1], ")[", as.integer(bits[2])+1, "]")

  res <- stringr::str_replace(value, "shape\\([^\\)]*\\)", dim_string)

  return (res)
}
