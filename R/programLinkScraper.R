# Load necessary libraries
library(rvest)
library(xml2)
library(stringr)
library(httr)

# Specify the URL of the webpage
url <- "https://grad.ucdavis.edu/graduate-programs"

# Read the webpage
get_html_data = function(url) {
  data = NA
  tryCatch({
    response = httr::GET(url)
    content = httr::content(response, "text")
    data = xml2::read_html(content)
  }, error = function(e) {
    message("Error: ", e$message)
  })
  data
}

#' Get links from an HTML page
#'
#' @param url The HTML node from which to retrieve links
#' @param only_relative Logical value: whether to return relative links only
#'
#' @return A character vector of links
#'
#' @export
get_links = function(html, only_relative = FALSE) {
  # Parse the HTML and find only those nodes with `a` tags
  links = xml2::xml_find_all(html, "//a")
  
  # Within `a` tags, find the `href` attribute. Filter out NAs and fragment
  # identifiers (`#`)
  links = unique(xml2::xml_attr(links, "href"))
  links = links[!is.na(links) & !grepl("#", links) & nzchar(links)]
  
  # Should we retain only those links that are relative to the URL?
  if (only_relative) {
    # Parse the links and check for schemes (http[s]) and domains
    # (example.com). Those that do not have both of these elements are relative
    # links
    parsed = lapply(links, httr::parse_url)
    mask = sapply(parsed, function(p) is.null(p$scheme) && is.null(p$hostname))
    links = links[mask]
  }
  links
}

#get the html of the site
html <- get_html_data(url)

#get ALL links present on the website
program_urls = get_links(html)

#subset the links to only include programs
program_urls = str_subset(program_urls, "/programs")

#add the beginning of the link to each relative link to allow them to be fully functional
program_urls = paste0("https://grad.ucdavis.edu/", program_urls)

program_homepages = c()

i = 1
for (program_url in program_urls)
{
  wait = sample(1:3, 1)
  Sys.sleep(wait)
  
  cat(i, ": currently scraping ", program_url)
  # Read the HTML content of the first URL in program_urls
  program_html <- read_html(program_url)
  
  class = xml_find_all(program_html, "//*[contains(@class, 'gs-program-website')]")
  
  link = xml_find_all(class, ".//a")
  
  link = link[[1]]
  
  homepage = xml_attr(link, "href")
  cat(": got link - ", homepage, "\n")
  program_homepages[i] = homepage
  
  i = i + 1
}
