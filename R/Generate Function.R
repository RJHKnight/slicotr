generate_function <- function(name, file_name, params)
{
  output_name <- paste0(name, ".R")

  x <- paste0(

    # Function definition
    name, "<- function(",
    paste(params$name, collapse = ","),
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
    mutate(text = paste0(stringr::str_to_upper(name), "=", name)) %>%
    pull(text) %>%
    paste(collapse = ",")

  return (
    paste0(
      "res <- .Fortran(",
      file_name,
      ",", param_string,
      ")"
    )
  )
}

handle_out <- function(params)
{
  dplyr::filter(params, intent == "out") %>%
    mutate(text = paste0(
      name, " <- ",
      if_else(is.na(dimension),
              paste0("as.", type, "(0)"),
              paste0("array(as.", type, "(0), c(", dimension, "))"))
    )) %>% pull(text)
}

handle_in <- function(params)
{
  in_params <- dplyr::filter(params, intent == "in")

  text <- in_params %>%
    mutate(text = if_else(type == "integer",
                          paste0(name, " <- as.integer(", name, ")"),
                          paste0(name, " <- as.double(", name, ")"))) %>%
    pull(text)

  return (text)
}

handle_hide <- function(params)
{
  dplyr::filter(params, intent == "hide") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = handle_shape(value)) %>%
    dplyr::mutate(
      text = paste0(name, " <- " , value)
    ) %>%
    pull(text)
}

handle_shape <- function(x)
{
  if (!stringr::str_detect(x, "shape"))
    return (x)

  bits <- stringr::str_split_fixed(x, "\\(|\\,|\\)", 4)

  return (paste0("dim(", bits[,2], ")[", as.integer(bits[,3])+1, "]"))
}

# Param:
# - type
# - intent
# - check
# - name
# - shape
type_levels <- c("integer", "double")
intent_levels <- c("in", "inout", "inplace", "out", "hide")

create_param <- function(type, intent, name, check = NA, dimension = NA, value = NA)
{
  return (data.frame(
    name = name,
    type = factor(type, levels = type_levels),
    intent = factor(intent, levels = intent_levels),
    check = check,
    dimension = dimension,
    value = value
  ))
}

sample_params <- rbind(
  create_param("integer", "in", "n", check = "n>=0"),
  create_param("integer", "hide", "p", check ="p>=1", value = "shape(a,2)"),
  create_param("integer", "in", "ilo", check = "1<=ilo && ilo<=max(1,n)",),
  create_param("double", "inout", "a", check = "min(ilo,n)<=ihi && ihi<=n", dimension = "lda1,lda2,p"),
  create_param("integer", "out", "tau", dimension = "max(1,n-1),p"),
  create_param("integer", "out", "info")
)
