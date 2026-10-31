library(xml2)
library(rvest)
library(stringr)

url <- "https://catalog.ucdavis.edu/departments-programs-degrees/"
doc = read_html(url)

# Extract the first div.az_sitemap node
first_target <- html_nodes(doc, "div.az_sitemap")[[1]]

# Extract all 'a' tags within the first_target
links <- html_nodes(first_target, "a")

# Extract the text content from each link (program names)
link_texts <- html_text(links)

# Filter links that contain the words "minor" or "Bachelor"
filtered_links <- links[str_detect(link_texts, regex("minor|Bachelor", ignore_case = TRUE))]

# Extract the href attributes (URLs) from the filtered links
urls <- html_attr(filtered_links, "href")

# Create full URLs
program_urls <- paste0("https://catalog.ucdavis.edu", urls)

# Create an empty vector to store the total units
total_units <- vector("character", length(program_urls))

# Process each program URL
for (i in seq_along(program_urls)) {
  program_url <- program_urls[i]
  cat("Processing:",i,"  ", program_url, "\n")
  program_page <- read_html(program_url)
  
  if (!is.na(program_page)) {
    # Extract all nodes matching the selector for total units
    total_units_nodes <- html_nodes(program_page, "table.sc_courselist:first-of-type tbody:first-of-type tr.listsum td.hourscol")
    if (length(total_units_nodes) == 0) {  # Check if listsum was not found
      # If listsum wasn't found, try to extract from lastrow
      total_units_nodes <- html_nodes(program_page, "table.sc_courselist:first-of-type tbody:first-of-type tr.lastrow td.hourscol")
    }
    if (length(total_units_nodes) > 0) {
      # Get the text content of the last total units node
      total_units[i] <- html_text(total_units_nodes[length(total_units_nodes)])
    } else {
      total_units[i] <- NA
    }
  } else {
    total_units[i] <- NA
  }
}



program_table <- data.frame(
  Program = html_text(filtered_links),
  URL = program_urls,
  TotalUnits = total_units,
  stringsAsFactors = FALSE
)



# manually fix incorrect units
program_table$TotalUnits[10]="18-20"
program_table$TotalUnits[25]="60-73"
program_table$TotalUnits[89]="147-152"
program_table$TotalUnits[102]="23-25"
program_table$TotalUnits[230]="75-84"

#--------------------------------------------------------------

# NOTE: a better way would have been to remove nodes with "toggle-group" and get the last "tr.listsum", and not caring about the first "sc_courselist".
# Because some pages have the total units under the second "sc_courselist" like Agricultural Systems & Environment, Minor [10]
# But getting the second "sc_courselist" could lead to getting the total units from "sc_courselist" that is under "toggle-group" which is just tracks to the major/minor

