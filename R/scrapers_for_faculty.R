# Scrapers:
#   1:
  
  url <- "https://example.ucdavis.edu"
  doc = read_html(url)
  
  # Use html_nodes() to select all <div> elements with class 'vm-teaser__body'
  professors <- html_nodes(doc, "div.vm-teaser__body")
  
  # Initialize empty vectors to store the data
  names <- character()
  titles <- character()
  
  # Iterate through each professor
  for (professor in professors) {
    # Extract professor name
    name <- html_node(professor, "h3.vm-teaser__title") %>% html_text(trim = TRUE)
    
    # Extract professor titles
    title_nodes <- html_nodes(professor, "ul.vm-teaser__position > li.field__item")
    title_text <- html_text(title_nodes, trim = TRUE)
    title <- paste(title_text, collapse = " / ")
    
    # Append data to vectors
    names <- c(names, name)
    titles <- c(titles, title)
  }
  
  # Create a dataframe
  professors_df <- data.frame(Name = names, Title = titles)
  
  #-------------------------------------------------------------------------------
  
  
  # 2:
    names <- html_nodes(doc, "h2.title a")
  titles <- html_nodes(doc, "p.subtitle")
  
  # Filter out professors with both name and title available
  names_text <- html_text(names)
  titles_text <- html_text(titles)
  
  # Filter out any empty elements
  non_empty_indices <- names_text != "" & titles_text != ""
  names_text <- names_text[non_empty_indices]
  titles_text <- titles_text[non_empty_indices]
  
  # Create a data frame
  professors_df <- data.frame(Name = names_text, Title = titles_text)
  
  
  #-------------------------------------------------------------------------------
  
  
  # 4:
    
    #departments - https://anb.ucdavis.edu/people 
    
    title <- html_nodes(doc, "li.list--pipe")
  titles <- html_text(title)
  faculty_title = data.frame(titles)
  #gives position and department name
  
  #-------------------------------------------------------------------------------
  
  
  # 5: 
  #   
  #   For multiple pages:
    
    # Base URL for the people listing
    base_url <- "https://communitydevelopment.ucdavis.edu/people?first=&last=&title=&unit=&field_sf_person_type_target_id%5B0%5D=26"
  
  # Initialize empty vectors to store the data
  names <- character()
  titles <- character()
  departments <- character() 
  
  # Loop through all pages (can be modified to limit pages)
  page_num <- 0
  while (TRUE) {
    Sys.sleep(1/30)
    
    # Construct URL for the current page
    url <- paste0(base_url, "&page=", page_num) # some use ? instead of & with the page
    
    # Download and parse the HTML content
    doc <- read_html(url)
    
    # Check if there are professors on this page
    professors <- html_nodes(doc, "div.vm-teaser__body")
    if (length(professors) == 0) break  # Exit loop if no professors found
    
    # Extract professor data for this page
    for (professor in professors) {
      name <- html_node(professor, "h3.vm-teaser__title") %>% html_text(trim = TRUE)
      
      # Extract professor titles
      title_nodes <- html_nodes(professor, "ul.vm-teaser__position > li.field__item")
      title_text <- html_text(title_nodes, trim = TRUE)
      title <- paste(title_text, collapse = " / ")
      
      # Extract professor departments
      department_node <- html_nodes(professor, "ul.vm-teaser__position > li:nth-child(2)")
      if (length(department_node) > 0) {
        department <- html_text(department_node, trim = TRUE)
      } else {
        department <- NA  # If department information is not available, assign NA
      }
      
      names <- c(names, name)
      titles <- c(titles, title)
      departments <- c(departments, department)
      
    }
    
    # Check for next page link
    next_page_link <- html_nodes(doc, "li.pager__item--next a") %>% html_attr("href")
    if (is.null(next_page_link)) break  # Exit loop if no next page link
    
    # Update page number for next iteration
    page_num <- page_num + 1
  }
  
  # Create a dataframe
  professors_df <- data.frame(Name = names, Title = titles, Department = departments)
  
  #-------------------------------------------------------------------------------
  
  
  # 6: another
  
  url <- "https://health.ucdavis.edu/nursing/ourteam/faculty/faculty_index.html"
  doc = read_html(url)
  professors <- html_nodes(doc, "div.panel-body p")
  professors <- professors[-c(1:2)]
  
  
  # Initialize empty vectors to store the data
  names <- character()
  titles <- character()
  departments <- character() 
  
  # Iterate through each professor
  for (professor in professors) {
    # Extract professor name
    name <- html_text(html_node(professor, xpath = "./a[1]/strong"), trim = TRUE)
    
    # Extract professor title
    title <- html_text(html_nodes(professor, xpath = "./text()[1]"), trim = TRUE)
    
    # Append data to vectors
    names <- c(names, name)
    titles <- c(titles, title)
  }
  
  
  # Create a dataframe
  titles <- titles[1:66]
  names <- names[1:66]
  names <- sub(",.*", "", names)
  
  professors_df <- data.frame(Name = names, Title = titles)
  
  #-------------------------------------------------------------------------------
  
  
  # 7. For embedded faculty webpages
  
  url <- "https://www.math.ucdavis.edu/research"
  doc = read_html(url)
  
  professors <- html_nodes(doc, "nobr")
  professor_text <- html_text(professors)
  professors_df <- data.frame(professor_text)
  
  professor_links <- html_nodes(professors, "a") %>% html_attr("href")
  
  base_url <- "https://www.math.ucdavis.edu/research"
  
  names <- character()
  titles <- character()
  departments <- character()
  
  for(link in professor_links) {
    faculty_url <- paste0("https://www.math.ucdavis.edu", link)
    faculty_page <- read_html(faculty_url)
    name <- faculty_page %>% html_node("article > h1") %>% html_text()
    title <- faculty_page %>% html_node("article > div:nth-child(4)") %>% html_text()
    department <- faculty_page %>% html_node("article > div:nth-child(5)") %>% html_text()
    
    names <- c(names, name)
    titles <- c(titles, title)
    departments <- c(departments, department)
  }
  professors_df <- data.frame(names, titles, departments)
  
  
