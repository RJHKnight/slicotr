START_SUB <- "^(subroutine|function)"
END_SUB <- "^end (subroutine|function)"
PARAM <- "::"
INTENT <- "intent"
DIMENSION <- "dimension"
FORTRAN_NAME <- "fortranname"

parse_pyf <- function(file_name)
{
  this_file <- file(file_name)
  raw <- readLines(this_file)
  close(this_file)

  handling_sub <- FALSE
  sub_name <- ""
  file_name <- ""
  params <- NULL
  param_order <- NULL

  for (i in 1:length(raw))
  {
    this_line <- raw[i]

    # Handle Sub def
    if (stringr::str_detect(this_line, START_SUB))
    {
      handling_sub <- TRUE
      sub_line <- stringr::str_remove_all(this_line, " ")
      file_name_raw <- stringr::str_match(sub_line, "\\!in\\:new\\:(.*f)|\\!in(.*f)|\\!in\\:balred\\:(.*f)")
      file_name <- file_name_raw[1,1+which(!is.na(file_name_raw[1,-1]))]
      file_name <- stringr::str_remove_all(file_name, "\\:.*\\:")

      sub_name <- stringr::str_match(this_line, "^(subroutine|function) ([^\\(]*)")[1,3]
      fn_params <- stringr::str_match(this_line, "(?<=\\().*(?=\\))")
      bits <- stringr::str_split(fn_params, ",")
      param_order <- data.frame(name = bits[[1]], original_order = 1:length(bits[[1]]))
    }
    else if (stringr::str_detect(this_line, FORTRAN_NAME))
    {
      # This overrides the fortran name on the sub line, if present
      file_name <- paste0(stringr::str_to_upper(stringr::str_split_fixed(stringr::str_trim(this_line), " ", 2)[1,2]), ".f")
    }
    else if (stringr::str_detect(this_line, PARAM))
    {
      if (!handling_sub)
      {
        stop(paste("Error - parameter outside of submodule on line", i))
      }

      # Handle params
      params <- rbind(params, parse_param(this_line))
    }
    else if (stringr::str_detect(this_line, END_SUB))
    {
      cat(paste("Generating function:", sub_name, "\n"))

      params <- handle_depends(params, sub_name)
      params <- left_join(params, param_order, by = "name")

      # Generate the R file
      generate_function(sub_name, file_name, filter(unique(params), !is.na(type)))

      # And reset state.
      sub_name <- ""
      file_name <- ""
      params <- NULL
      handling_sub <- FALSE
      param_order <- NULL

    }
  }
}


parse_param <- function(this_line)
{
  #integer optional,check(ldwork>=ihi-ilo+p-1), depend(ihi,ilo,p) :: ldwork=max(1,ihi-ilo+p-1)

  # Get type
  bits <- stringr::str_match(this_line, "(integer|double precision|integer optional|character|logical)(.*)")

  type <- stringr::str_trim(stringr::str_replace(bits[1,2], "precision", ""))
  extra <- bits[1,3]

  # Extract name and other properties
  bits <- stringr::str_split_fixed(extra, "::", 2)
  name_value <- stringr::str_trim(bits[1,2])
  other_props <- stringr::str_trim(bits[1,1])

  # Get param name and default value
  bits <- stringr::str_split_fixed(stringr::str_remove_all(name_value, " \\!.*"), "=", 2)
  param_name <- bits[1,1]
  param_value <- bits[1,2]

  # Handle the intent, check and dimension
  other <- handle_other(other_props)

  return (create_param(type, other$intent, param_name,
                       check = other$check, dimension = other$dimension,
                       value = param_value, depend = other$depend))
}

OPEN_PAREN <- "("
CLOSE_PAREN <- ")"

# Handle the intent, check and dimension
handle_other <- function(other_properties)
{
  intent <- NA
  check <- NA
  dimension <- NA
  depend <- NA

  in_block <- FALSE
  buffer <- NULL

  is_intent <- FALSE
  is_check <- FALSE
  is_dimension <- FALSE
  is_depend <- FALSE

  paren_counter <- 0

  chars <- strsplit(other_properties, "")[[1]]
  i <- 1

  while (i <= length(chars))
  {
    this_char <- chars[i]

    if (in_block)
    {
      i <- i + 1
      buffer <- paste0(buffer, this_char)

      if (this_char == OPEN_PAREN)
      {
        paren_counter <- paren_counter + 1
      }
      if (this_char == CLOSE_PAREN)
      {
        paren_counter <- paren_counter - 1

        if (paren_counter == 0)
        {
           # Remove outer parenthesis
           buffer <- substr(buffer, 2, nchar(buffer)-1)

           if (is_intent) intent <- buffer
           else if (is_check) check <- buffer
           else if (is_dimension) dimension <- buffer
           else depend <- buffer

           in_block <- FALSE
           is_intent <- FALSE
           is_check <- FALSE
           is_dimension <- FALSE
           is_depend <- FALSE
           buffer <- NULL

           next
        }
      }
    }

    else if (i+2 > length(chars))
    {
      break
    }

    # Check
    else if (this_char == "c" & chars[i+1] == "h" & chars[i+2] == "e")
    {
      in_block <- TRUE
      is_check <- TRUE
      i <- i + 5
      next
    }

    # Intent
    else if (this_char == "i" & chars[i+1] == "n" & chars[i+2] == "t")
    {
      in_block <- TRUE
      is_intent <- TRUE
      i <- i + 6
      next
    }

    # Depend
    else if (this_char == "d" & chars[i+1] == "e" & chars[i+2] == "p")
    {
      in_block <- TRUE
      is_depend <- TRUE
      i <- i + 6
      next
    }

    # Dimension
    else if (this_char == "d" & chars[i+1] == "i" & chars[i+2] == "m")
    {
      in_block <- TRUE
      is_dimension <- TRUE
      i <- i + 9
      next
    }

    else
    {
      i <- i + 1
      #stop(paste("Unknown block for:" , this_line, "at position", i))
    }
  }


  return (list(intent = intent, check = check, dimension = dimension, depend = depend))
}

create_param <- function(type, intent, name, check = NA, dimension = NA, value = NA, depend = NA)
{
  return (data.frame(
    name = stringr::str_to_lower(stringr::str_trim(name[1])),
    type = stringr::str_to_lower(stringr::str_trim(type[1])),
    intent = ifelse(is.na(intent[1]), "in", stringr::str_to_lower(stringr::str_trim(intent[1]))),
    check = stringr::str_to_lower(stringr::str_trim(check[1])),
    dimension = stringr::str_to_lower(stringr::str_trim(dimension[1])),
    value = stringr::str_to_lower(stringr::str_trim(value[1])),
    depend = stringr::str_to_lower(stringr::str_trim(depend[1]))
  ))
}

handle_depends <- function(params, name)
{
    # First params without depends
    no_depend <- dplyr::filter(params, is.na(depend))

    # Others need more finesse
    depend_params <- dplyr::filter(params, !is.na(depend))

    depend_details = select(params, name, depend ) %>% rename(other_depend = depend) %>% separate_rows(other_depend)

    depend_params <- depend_params %>%
      separate_rows(depend) %>%
      left_join(depend_details, by = c("depend" = "name"))


    # Anything that depends on a no-depend param can go next
    depend_1 <- dplyr::filter(depend_params, is.na(other_depend)) %>% select(-other_depend) %>%
      group_by(name) %>%
      summarise(name = name[1],
                type = type[1],
                intent = intent[1],
                check = check[1],
                dimension = dimension[1],
                value = value[1],
                depend = paste(depend, collapse = ","))

    # These all depend on a depend param
    depend_res <- dplyr::filter(depend_params, !name %in% depend_1$name)

    # If it depends on anything in depend 1 - ok to add
    depend_2 <- depend_res %>%
      mutate(in_depend_1 = depend %in% depend_1$name) %>%
      group_by(name) %>%
      summarise(name = name[1],
                type = type[1],
                intent = intent[1],
                check = check[1],
                dimension = dimension[1],
                value = value[1],
                depend = paste(depend, collapse = ","),
                include = all(in_depend_1, TRUE)) %>%
      filter(include) %>%
      select(-include)

    depend_3 <- dplyr::filter(depend_params, !name %in% c(depend_1$name, depend_2$name)) %>% select(-other_depend)

    if (nrow(depend_3) > 1)
    {
      warning("Mutliple dependent3 params - check code for: ", name)
    }

    return (
      rbind(
        no_depend,
        depend_1,
        depend_2,
        depend_3
      )
    )
}

