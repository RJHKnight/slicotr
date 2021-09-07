library(httr)
library(xml2)
library(rvest)
library(stringr)

URL_STEM <- 'http://slicot.org/objects/software/shared/doc/'

generate_docs <- function(fortran_name, sub_name)
{
  function_name <- str_split_fixed(fortran_name, "\\.", 2)[1]
  doc_url <- paste0(URL_STEM, function_name, ".html")
  html_source <- read_html(doc_url)

  short_des <- html_source %>%  html_nodes("h3") %>% html_text()
  long_des <- (html_source %>% html_nodes("pre") %>% html_text())[1]

  roxygen_string <- paste0(
    "#\' ", sub_name, "\n",
    "#\'\n",
    "#\' ", str_remove_all(short_des, "\n"), "\n",
    "#\' @examples \n", str_replace_all(long_des, "\n", "\n#\' "),
    "#\'\n",
    "#\' @references \\url{", doc_url,"}\n",
    "#\' @export\n"
  )

  return (roxygen_string)
}
