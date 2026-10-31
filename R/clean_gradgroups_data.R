library(stringr)
library(tidyr)
library(dplyr)
library(humaniformat)
library(rvest)
library(xml2)
library(httr)

groups <- read.csv("graduate_group.csv") #load data for graduate groups
faculty_ls <- read.csv("faculty_department.csv") #load data for faculty in L & S [on google drive]
grad_centers <- read.csv("grad_centers.csv")
colnames(faculty_ls) <- c('X', 'Name', 'Department', 'Job.code', 'Job.Code.Description', 'FTE') #renaming columns
faculty_ls <- data.frame(faculty_ls$Name, faculty_ls$Department) #make data frame out of name and department columns
colnames(faculty_ls) <- c('Department', 'Name')


names_faculty_parsed = parse_names(faculty_ls$Name) #making the names first-last format in the faculty_ls data

# Splitting the Name column

name_parts <- strsplit(faculty_ls$Name, ",")

# Reformatting names
new_names <- sapply(name_parts, function(x) {
  if (length(x) == 2) {
    # Handle "Last, First" format
    name <- x[2]
    paste(name, x[1], sep = " ")  # Rearrange to "First Last"
  } else if (length(x) == 3) {
    # Handle "Last, First MI" format
    paste(x[2], x[3], x[1], sep = " ")  # Rearrange to "First MI Last"
  } else {
    # Handle other cases based on your data
    NA  # or some default value
  }
})

faculty_ls$Name<- new_names #replace with new names



unknown_faculty <- full_join(groups, faculty_ls, by = "Name") #merge grad groups and faculty_ls data by name

unknown_faculty[unknown_faculty == ""] <- NA #fill empty spaces with NA

unknown_faculty <- unknown_faculty %>%
  mutate(Department.x = ifelse(is.na(Department.x), Department.y, Department.x), ) #if the department in grad groups (department.x) is NA, replace it with the department in faculty_ls -> will work for any faculty that matches

unknown_faculty <- unknown_faculty %>%
  mutate(College = ifelse(!is.na(Department.x) & Department.x == Department.y, "Letters and Sciences", College)) #make the college column "Letters and Sciences" for any departments in grad groups were replaced by faculty_ls departments

groups <- unknown_faculty[!is.na(unknown_faculty$Graduate.Group), ] #remove empty rows

groups <- groups[, -which(names(groups) == 'Department.y')] #remove the faculty_ls department column

#make all names the same
#merge to abbreviations & college
#separate faculty members in multiple departments (one listing per department)
#scrape college names

colnames(groups)[4] <- "Department" 
groups$Department[2:60] <- "Management" #put Management as the department for management grad group
groups <- groups %>%
  mutate(Department = str_trim(Department, side = "left")) #remove leading spaces 

#function that moves the college mentioned in () to college column
move_to_college <- function(column1, column2) {
  indices <- grep("\\(([^)]+)\\)", column1) #finds indices in groups with the (xxx) pattern
  for(i in 1:length(indices)) {
    column2[indices[i]] <- sub(".*\\(([^)]+)\\).*", "\\1", column1[indices[i]]) #replaces the college column with what is inside paratheses
}
  
  return(column2)
}

groups$College <- move_to_college(groups$Department, groups$College)

groups$Department <- str_remove_all(groups$Department, "\\s*\\([^\\)]+\\)") #remove the parenthesis in the department column



extract_college <- function(column1, column2) {
  indices <- grep(", College(.*)", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub(".*?, College(.*)", "College\\1", column1[indices[i]])
  }
  return(column2)
}

groups$College <- extract_college(groups$Department, groups$College)
groups$College <- str_replace_all(groups$College, "College of", "")
groups$Department <- str_replace_all(groups$Department, ", College.*", "")

groups$Department <- str_replace_all(groups$Department, "Department of", "")
groups$Department <- str_replace_all(groups$Department, "Department", "")
groups$Department <- str_replace_all(groups$Department, "Departments of", "")
groups$Department <- str_replace_all(groups$Department, "Graduate School of Management", "Management")

for (i in 1:nrow(groups)) {
  if (!is.na(groups$Department[i]) && groups$Department[i] == "Management") {
    groups$College[i] <- "Management"
  }
}

extract_school <- function(column1, column2) {
  indices <- grep(", School(.*)", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub(".*?, School(.*)", "School\\1", column1[indices[i]])
  }
  return(column2)
}

groups$College <- extract_school(groups$Department, groups$College)
groups$Department <- str_replace_all(groups$Department, ", School.*", "")
groups$College <- str_replace_all(groups$College, "School of Veterinary Medicine, Coastal and Marine Sciences Institute", "Veterinary Medicine")
groups$College <- str_replace_all(groups$College, "School of Medicine Bioethics Program, Clinical and Translational Science Center", "")
groups$Department <- str_replace_all(groups$Department, "UC Davis School of Medicine, Dept of Pediatrics", "Pediatrics")
groups$College[756] <- "School of Medicine"

extract <- function(column1, column2) {
  indices <- grep("School", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub("School(.*)", "School\\1", column1[indices[i]])
  }
  return(column2)
}

groups$College <- extract(groups$Department, groups$College)
groups$College <- str_replace_all(groups$College, "Public Health Sciences; UC Davis School of Medicine", "School of Medicine")
groups$Department <- str_replace_all(groups$Department, " Public Health Sciences; UC Davis School of Medicine", "Public Health Sciences")
groups$Department <- str_replace_all(groups$Department, "School.*", "")
groups$College <- str_replace_all(groups$College, "School of", "")

extract_departments <- function(column1, column2) {
  indices <- grep(", Department of(.*)", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub(".*?, Department of (.*)", "\\1", column1[indices[i]])
  }
  return(column2)
}

groups$Department <- extract_departments(groups$Title, groups$Department)

groups$Department[962] <- "Neurology"
groups$College[962] <- "Medicine"

groups$Department[1326] <- "Emergency Medicine"
groups$College[1326] <- "Medicine"

groups$Department <- str_replace_all(groups$Department, " Sociocultural Wing and", ",")

groups$Department <- str_replace_all(groups$Department, ", and Chair,", ",")

groups$Department <- str_replace_all(groups$Department, " , Kinsella Endowed Chair in Food, Nutrition, and Health", "")

groups$Department <- str_replace_all(groups$Department, " / Assistant Nutritionist in Agricultural Experiment Station", "")

groups$Department <- str_replace_all(groups$Department, " / Professor, Department of", ",")

groups$Department <- str_replace_all(groups$Department, ", School of Veterinary Medicine", "")
groups$College[3218] <- "Veterinary Medicine"

groups$Department <- str_replace_all(groups$Department, ", School of Medicine", "")
groups$College[3208] <- "Medicine"

groups$Department <- str_replace_all(groups$Department, "Veterinary Medicine: ", "")
groups$College[3228] <- "Veterinary Medicine"
groups$Department <- str_replace_all(groups$Department, " / Assistant Visiting Researcher, Department of", ",")

groups$Department <- str_replace_all(groups$Department, "/Department of", ",")
groups$Department <- str_replace_all(groups$Department, " / Kinsella Endowed Chair, Food, Nutrition, and Health", "")
groups$Department <- str_replace_all(groups$Department, " / Director, Didactic Program in Dietetics", "")
groups$Department <- str_replace_all(groups$Department, " / Corinne L. Rustici Endowed Chair in Applied Human Nutrition / Director, Institute for Global Nutrition", "")

groups$Department[groups$Department == ""] <- NA
extract_departments_2 <- function(column1, column2) {
  indices <- grep("Professor of(.*)", column1)
  for(i in 1:length(indices)) {
    if(is.na(column2[indices[i]]) == TRUE)
    column2[indices[i]] <-  sub(".*Professor of (.*)", "\\1", column1[indices[i]])
  }
  return(column2)
}

groups$Department <- extract_departments_2(groups$Title, groups$Department)

groups$Department <- str_replace_all(groups$Department, "Teaching", "")
groups$Department <- str_replace_all(groups$Department, " in", "")
groups$Department <- str_replace_all(groups$Department, " / Director of Undergraduate Study of Comparative Literature / Faculty Undergraduate Advisor", "")
groups$Department <- str_replace_all(groups$Department, " / Affiliated Faculty of Middle East / South Asia Studies / Program Instructor of Arabic Language", "")
groups$Department <- str_replace_all(groups$Department, " / Director of ", ",")
groups$Department <- str_replace_all(groups$Department, "/ Affiliated Faculty of the Graduate Program German Studies and the Program Critical Theory", "")
groups$Department <- str_replace_all(groups$Department, " / Affiliated Faculty of", ",")
groups$Department <- str_replace_all(groups$Department, ", the Study of Religion", "")
groups$Department <- str_replace_all(groups$Department, " Affiliated Faculty", "")
groups$Department <- str_replace_all(groups$Department, "French / Graduate Faculty Advisor for ", "")
groups$Department <- str_replace_all(groups$Department, ", Chair of CANDEL", "")
groups$Department <- str_replace_all(groups$Department, " and Bilingual Coordinator–Spanish", "")
groups$Department <- str_replace_all(groups$Department, " and Bilingual Coordinator–Mandarin", "")
groups$Department <- str_replace_all(groups$Department, ", Chair of Undergraduate Programs, Executive Director of Transformative Justice Education (TJE) Center", "")
groups$Department <- str_replace_all(groups$Department, "	, Dept of ", "")
groups$Department <- str_replace_all(groups$Department, "^, ", "")
groups$Department <- str_replace_all(groups$Department, ", Endowed Chair Fragile X Research, Medical Director of the MIND Institute, Director of the Fragile X Treatment and Research Center", "")
groups$Department <- str_replace_all(groups$Department, "and Vice Provost of Public Scholarship", "")
groups$Department <- str_replace_all(groups$Department, " of Undergraduate Programs, Executive Director of Transformative Justice Education (TJE) Center", "")
groups$Department <- str_replace_all(groups$Department, " and Associate Dean, Graduate Studies", "")
groups$Department <- str_replace_all(groups$Department, "Hydrology & Assistant Aqueous Chemist", "")
groups$Department <- str_replace_all(groups$Department, "Soil Biogeochemistry and Soil Biogeochemist", "")
groups$Department <- str_replace_all(groups$Department, "Remote Sensing and Ecosystem Change", "")
groups$Department <- str_replace_all(groups$Department, "Irrigation Engineering and Agricultural Water Management", "")
groups$Department <- str_replace_all(groups$Department, "Coastal Oceanography", "")
groups$Department <- str_replace_all(groups$Department, "Soil Ecology", "")
groups$Department <- str_replace_all(groups$Department, " and Biometeorologist (in the AES)", "")
groups$Department <- str_replace_all(groups$Department, ", Specialist Biometeorology", "")
groups$Department <- str_replace_all(groups$Department, "and Co-Director, California Environmental Law and Policy Center", "")
groups$Department <- str_replace_all(groups$Department, ", and Director of Clinical Legal Education", "")
groups$Department <- str_replace_all(groups$Department, " and Co-Director, California Environmental Law and Policy Center", "")
groups$Department <- str_replace_all(groups$Department, " and Chancellor's Fellow", "")
groups$Department <- str_replace_all(groups$Department, "and Director", "")
groups$Department <- str_replace_all(groups$Department, "of the ", ",")
groups$Department <- str_replace_all(groups$Department, "(LSOE)", "")
groups$Department <- str_replace_all(groups$Department, ", Director of Feed the Future Innovation Lab for Climate Resilient Chickpea", "")
groups$Department <- str_replace_all(groups$Department, ", Dean ,College of Agricultural and Environmental Sciences", "")
groups$Department <- str_replace_all(groups$Department, ", Director ,Western Plant Diagnostic Network", "")
groups$Department <- str_replace_all(groups$Department, ", Department Chair", "")
groups$Department <- str_replace_all(groups$Department, " /  Faculty Affiliate, UC Davis Global Migration Center /  Media Liaison and Board Member for Humane Borders (Tucson, AZ)", "")
groups$Department <- str_replace_all(groups$Department, " / The Russell F. and Jean H. Fiddyment Chair Latin American Studies", "")
groups$Department <- str_replace_all(groups$Department, " / Chair, Designated Emphasis Second Language Acquisition", "")
groups$Department <- str_replace_all(groups$Department, " / Associate Vice Provost of Academic Programs Global Affairs", "")
groups$Department <- str_replace_all(groups$Department, "/ UC Education Abroad Program Faculty Director, Spain Programs", "")
groups$Department <- str_replace_all(groups$Department, "Classics Chair of ", "")
groups$Department <- str_replace_all(groups$Department, " / Affiliated faculty ,Study of Religion", "")
groups$Department <- str_replace_all(groups$Department, " / Affiliated faculty the Study of Religion", "")
groups$Department <- str_replace_all(groups$Department, " / Advisor for Religious Studies Undergraduates / Director, Medieval and Early Modern Studies Program /  Assistants Supervisor for Religious Studies", "")
groups$Department <- str_replace_all(groups$Department, " / Graduate Program Chair,the First-Year Spanish Program", "")
groups$Department[3912] <- "Comparative Literature"

# merging with grad center data

matches <- full_join(groups, grad_centers, by = "Name")
matches[matches == ""] <- NA #fill empty spaces with NA

matches <- matches %>%
  mutate(Department.x = ifelse(is.na(Department.x), Department.y, Department.x), ) #if the department in grad groups (department.x) is NA, replace it with the department in faculty_ls -> will work for any faculty that matches

matches <- matches %>%
  mutate(College.x = ifelse(is.na(College.x), College.y, College.x)) #make the college column "Letters and Sciences" for any departments in grad groups were replaced by faculty_ls departments

groups <- matches[, setdiff(names(matches), c('Department.y', 'College.y', 'Email', 'Website', 'Title.y', 'Center'))]

groups <- groups[!is.na(groups$Graduate.Group), ]

#getting department abbreviations 

url = "https://catalog.ucdavis.edu/courses-subject-code/"
doc = read_html(url)

names <- html_nodes(doc, "a") #a tags have the department name & abbreviation listed together
Department <- html_text(names)

names_departments = data.frame(Department, 1:351) 

names_departments <- names_departments[-c(1:54, 276:351), ] #removing anything that's not a department

# Extract content within parentheses
names_departments$abb <- str_extract(names_departments$Department, "\\(([^)]+)\\)") #extract the department abbreviation

# Remove parentheses from the extracted content
names_departments$abb <- str_replace_all(names_departments$abb, "[()]", "") #remove parenthesis
names_departments$Department <- str_remove_all(names_departments$Department, "\\s*\\([^\\)]+\\)") #removes the abbreviated (xxx) from the department column

department_abbreviations <- names_departments[ ,c('Department', 'abb')] #rename the columns


# adding college names to department abbreviations
url <- "https://catalog.ucdavis.edu/courses-subject-code/"

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
dep_urls = get_links(html)

#subset the links to only include programs
dep_urls = str_subset(dep_urls, "/courses-subject-code")

#add the beginning of the link to each relative link to allow them to be fully functional
dep_urls = paste0("https://catalog.ucdavis.edu", dep_urls)

#scrape the college names from each link in dep_urls
colleges <- c()
for (dep_url in dep_urls){
  
  dep_html <- read_html(dep_url)
  
  class = xml_find_all(dep_html, "//*[contains(@class, 'title-college')]") #contained in class title-college
  
  college <- html_text(class)
  
  colleges <- c(colleges, college)
}


colleges_df <- data.frame(colleges) #create data frame

# Insertions
insertRow <- function(existingDF, new_row, r) { 
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]  
  existingDF[r,] <- new_row                          
  return(existingDF) 
  
} 

# Insert the new row
colleges_df <- insertRow(colleges_df, "College of Agricultural & Environmental Sciences", 218)
names_departments$count <- c(1:221)
names_departments <- names_departments[-146, ]

department_abbreviations <- names_departments[ ,c('Department', 'abb')] 
department_abbreviations$college <- colleges_df$colleges

department_abbreviations$Department <- str_replace_all(department_abbreviations$Department, "Mechanical & Aeronautical Engineering", "Mechanical & Aerospace Engineering")

write.csv(department_abbreviations, "department_abbreviations.csv", row.names = FALSE)


















groups <- merge(names_departments, groups, by = 'Department')



