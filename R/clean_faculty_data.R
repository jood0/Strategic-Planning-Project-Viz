#load packages
library(stringr)
library(tidyr)
library(dplyr)
library(humaniformat)
library(rvest)
library(xml2)
library(httr)
library(ggplot2)
library(viridis)

#get data

#groups <- read.csv("graduate_group.csv") #load data for graduate groups
#faculty_ls <- read.csv("faculty_department.csv") #load data for faculty in L & S 
#grad_centers <- read.csv("grad_centers.csv")

#change column names
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
    column2[indices[i]] <- sub(".*\\(([^)]+)\\).*", "\\1", column1[indices[i]]) #replaces the college column with what is inside parantheses
}
  
  return(column2)
}

groups$College <- move_to_college(groups$Department, groups$College)

groups$Department <- str_remove_all(groups$Department, "\\s*\\([^\\)]+\\)") #remove the parenthesis in the department column

#function that splits college mentioned after the comma

extract_college <- function(column1, column2) {
  indices <- grep(", College(.*)", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub(".*?, College(.*)", "College\\1", column1[indices[i]])
  }
  return(column2)
}

#cleaning 
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

#extract schools mentioned after ","
extract_school <- function(column1, column2) {
  indices <- grep(", School(.*)", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub(".*?, School(.*)", "School\\1", column1[indices[i]])
  }
  return(column2)
}

groups$College <- extract_school(groups$Department, groups$College)

#more cleaning
groups$Department <- str_replace_all(groups$Department, ", School.*", "")
groups$College <- str_replace_all(groups$College, "School of Veterinary Medicine, Coastal and Marine Sciences Institute", "Veterinary Medicine")
groups$College <- str_replace_all(groups$College, "School of Medicine Bioethics Program, Clinical and Translational Science Center", "")
groups$Department <- str_replace_all(groups$Department, "UC Davis School of Medicine, Dept of Pediatrics", "Pediatrics")
groups$College[756] <- "School of Medicine"

#extracting whatever comes after school to put into college column
extract <- function(column1, column2) {
  indices <- grep("School", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub("School(.*)", "School\\1", column1[indices[i]])
  }
  return(column2)
}

groups$College <- extract(groups$Department, groups$College)

#cleaning
groups$College <- str_replace_all(groups$College, "Public Health Sciences; UC Davis School of Medicine", "School of Medicine")
groups$Department <- str_replace_all(groups$Department, " Public Health Sciences; UC Davis School of Medicine", "Public Health Sciences")
groups$Department <- str_replace_all(groups$Department, "School.*", "")
groups$College <- str_replace_all(groups$College, "School of", "")

#extracting departments function
extract_departments <- function(column1, column2) {
  indices <- grep(", Department of(.*)", column1)
  for(i in 1:length(indices)) {
    column2[indices[i]] <-  sub(".*?, Department of (.*)", "\\1", column1[indices[i]])
  }
  return(column2)
}

groups$Department <- extract_departments(groups$Title, groups$Department)

#cleaning
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

#extracting departments coming after professor of...
extract_departments_2 <- function(column1, column2) {
  indices <- grep("Professor of(.*)", column1)
  for(i in 1:length(indices)) {
    if(is.na(column2[indices[i]]) == TRUE)
    column2[indices[i]] <-  sub(".*Professor of (.*)", "\\1", column1[indices[i]])
  }
  return(column2)
}

groups$Department <- extract_departments_2(groups$Title, groups$Department)

#cleaning
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

colnames(groups)[4] <- "Department"


#getting department abbreviations 

url = "https://catalog.ucdavis.edu/courses-subject-code/"
doc = read_html(url)

names <- html_nodes(doc, "a") #a tags have the department name & abbreviation listed together
Department <- html_text(names)

Department <- Department[1:351]
names_departments = data.frame(Department, 1:351) 

names_departments <- names_departments[-c(1:54, 276:351), ] #removing anything that's not a department

# Extract content within parentheses
names_departments$abb <- str_extract(names_departments$Department, "\\(([^)]+)\\)") #extract the department abbreviation

# Remove parentheses from the extracted content
names_departments$abb <- str_replace_all(names_departments$abb, "[()]", "") #remove parenthesis
names_departments$Department <- str_remove_all(names_departments$Department, "\\s*\\([^\\)]+\\)") #removes the abbreviated (xxx) from the department column

department_abbreviations <- names_departments[ ,c('Department', 'abb')] #rename the columns
write.csv(department_abbreviations, "department_abbreviations.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
#replace departments with abbreviations


#remove & and "and" from both datasets
department_abbreviations$Department <- gsub("& ", "", department_abbreviations$Department)
department_abbreviations$Department <- gsub("and ", "", department_abbreviations$Department)
department_abbreviations$Department <- gsub(", ", "", department_abbreviations$Department)

groups$Department <- gsub("& ", "", groups$Department)
groups$Department <- gsub("and ", "", groups$Department)


groups$Department <- gsub("Department of ", "", groups$Department)
department_abbreviations$abb[department_abbreviations$Department == "Plant Biology"] <- "PLB"

# fix the differences in names
groups$Department <- gsub("Plant Sciences", "Plant Science", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("WFC Biology", "WFC", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("LAir Water Resources", "LAWR", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Land, Air, Water Resources", "LAWR", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("LAND, AIR WATER RESOURCES", "LAWR", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Veterinary Medicine Epidemiology", "Veterinary Medicine, Epidemiology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Entomology/Nematology", "Entomology, Nematology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("APC Biology", "APC", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Entomology Nematology", "Entomology, Nematology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Anatomy, Physiology Cell Biology", "APC", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Internal Medicine/Cardiology", "Internal Medicine, Cardiology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Neurology", "Neurology,", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Chicana/o Studies Department", "Chicano Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Landscape Architecture \\+ Environmental Design", "Landscape Architecture, Environmental Design", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Middle Eastern/South Asian Studies", "Middle East/South Asian Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("German Russian", "German, Russian", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("French Italian", "French, Italian", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Spanish Portuguese", "Spanish, Portuguese", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Chicana/Chicano Studies", "Chicano Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Chicana/o Studies", "Chicano Studies,", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Design American Studies", "Design, American Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Neurology,,", "Neurology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Veterinary Medicine", "School of Veterinary Medicine", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Graduate School of Management", "Management", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Chemical Engineering Material Sciences", "Chemical Engineering, Materials Science Engineering", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Pathology, Microbiology & Immunology", "PathologyMicrobiologyImmunology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("VET MED: Pathology, Microbiology Immunology", "PathologyMicrobiologyImmunology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("GSW Studies", "GSW", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Chicana Chicano Studies", "Chicano Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Art Art History", "Art, Art History", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Agricultural Resource Economics", "Agricultural Resource Econ", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Gender, Sexuality Women's Studies", "GSW", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Graduate Group", "", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("MED: Microbiology Immunology", "Medical Microbiology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Medical Microbiology Immunology", "Medical Microbiology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Microbiology Immunology", "Medical Microbiology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Psychiatry Behavioral Sciences", "Psychiatry", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("VITICULTURE", "Viticulture Enology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Viticulture Enology Enology", "Viticulture Enology", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Population, Health Reproduction", "Population Health Reproduction", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Population, Health Reproduction", "Population Health Reproduction", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Chicano Studies,", "Chicano Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("VET MED:", "", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Epidemiology Biostatistics", "Epidemiology, Biostatistics", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Applied Mathematics", "Mathematics", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Atmospheric Sciences", "Atmospheric Science", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Biomedical Engineering Neurological Surgery", "Biomedical Engineering, Neurological Surgery", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("Middle East/South Asian Studies ", "Middle East/South Asian Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("History Middle East/South Asia Studies", "History, Middle East/South Asian Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("History Middle East/South Asia Studies", "History, Middle East/South Asian Studies", groups$Department, ignore.case = TRUE)
groups$Department <- gsub("LAWR Biological Agricultural Engineering", "LAWR, Biological Agricultural Engineering", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("Adjunct Professor, ", "", groups$Department, ignore.case = TRUE) ###new
groups$Department <- gsub(" the DE in", "", groups$Department, ignore.case = TRUE) ###new
groups$Department <- gsub("Mechanical Aerospace Engineering ", "Mechanical Aerospace Engineering", groups$Department, ignore.case = TRUE) ###new
groups$Department <- gsub("Russian / Comparative Literature", "Russian, Comparative Literature", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("Middle Eastern/South Asia Studies", "Middle East/South Asian Studies", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("ENTOMOLOGY AND NEMATOLOGY", "Entomology, Nematology", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("ENVIRONMENTAL SCIENCE AND POLICY", "Environmental Science Policy", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("PLANT PATHOLOGY", "Plant Pathology", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("PLANT BIOLOGY", "Plant Biology", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("Internal Medicine - Pulmonary", "Pulmonary Medicine", groups$Department, ignore.case = TRUE) 
groups$Department <- gsub("Middle East/South Asia Studies", "MSA", groups$Department, ignore.case = TRUE) 


# to see count of the most frequent  dep.
# department_counts <- table(groups$Department)
# sorted_department_counts <- sort(department_counts, decreasing = TRUE)
# sorted_department_df <- as.data.frame(sorted_department_counts)
# colnames(sorted_department_df) <- c("Department", "Count")
# write.csv(sorted_department_df, "sorted_department_counts.csv", row.names = FALSE)


# make-up abbreviations for the ones that don’t have one
new_departments <- data.frame(
  Department = c("Biological Agricultural Engineering", "Human Ecology", "Computer Science", 
                 "LandAirWater Resources","Earth Planetary Sciences",
                 "Biochemistry Molecular Medicine", "Physiology Membrane Biology",
                 "Pathology Laboratory Medicine", "Pharmacology", "Environmental Design","Theatre Dance",
                 "East Asian Languages Cultures","Rheumatology, Allergy Clinical Immunology", "Mathematical Physical Sciences"),
  abb = c("BAE", "HCL", "CSS","LAWR","EPS","BMM", "PMB","PLM", "PRC", "EVD", "TAD", "EAC","RCI", "MPS")
)

#  Add the new abbreviations to the existing data frame
department_abbreviations <- rbind(department_abbreviations, new_departments)


# Create a mapping between department names and abbreviations
department_mapping <- setNames(department_abbreviations$abb, department_abbreviations$Department)

# Function to replace department names with abbreviations if available
replace_departments <- function(departments, mapping) {
  split_departments <- strsplit(departments, ", ", fixed = TRUE)[[1]]  # Split departments by comma and space
  abbreviated_departments <- sapply(split_departments, function(dep) ifelse(dep %in% names(mapping), mapping[dep], dep))  # Replace department names with abbreviations if available, otherwise keep them unchanged
  return(paste(abbreviated_departments, collapse = ", "))  # Combine departments back into a single string
}

#remove leading spaces
groups <- groups %>%
  mutate(Department = str_trim(Department, side = "left"))

groups <- groups %>%
  mutate(Department = str_trim(Department, side = "right"))

# Apply the replace_departments function to each row of the 'groups' dataframe
groups$Department <- mapply(replace_departments, groups$Department, MoreArgs = list(mapping = department_mapping))

#-------------------------------------------------------------------------------
groups[groups == ""] <- NA #replace empty cells with NA

#finding missing departments and colleges
missing_department <- groups[groups$Department == "NA", ]
missing_college <- groups[is.na(groups$College.x), ]

#replacing unknown departments in grad groups with the name of the grad group
for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Economics")) {
    groups$Department[i] <- "ECN"
    groups$College.x[i] <- "LS"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Computer Science")) {
    groups$Department[i] <- "CSS"
    groups$College.x[i] <- "ENG"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Communication")) {
    groups$Department[i] <- "CMN"
    groups$College.x[i] <- "LS"
  }
}


for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("History")) {
    groups$Department[i] <- "HIS"
    groups$College.x[i] <- "LS"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Ecology")) {
    groups$Department[i] <- "ECL"
    groups$College.x[i] <- "AES"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Earth and Planetary Sciences")) {
    groups$Department[i] <- "EPS"
    groups$College.x[i] <- "LS"
  }
}


#grouping grad groups that don't have departments info or do not have anything to do with colleges
no_dep_groups <- groups[groups$Graduate.Group %in% c("Education (M.A.)", "Education (Ph.D.) ", "Educational Leadership (CANDEL)", "Epidemiology", "Graduate School of Management Programs", "Nursing (Doctor of Nursing Practice - Family Nurse Practitioner) ", "Nursing", "Law (LL.M.)", "Integrative Genetics and Genomics", "International Agricultural Development", "Maternal and Child Nutrition", "Mechanical and Aerospace Engineering", "Molecular, Cellular and Integrative Physiology", "Neuroscience", "Performance Studies", "Public Health (MPH)", "Soils and Biogeochemistry"), ]

#manually inputing departments into groups data set or deleting rows with no departments

groups$Department[573] <- "ECH"
groups$College.x[573] <- "ENG"

groups$Department[730] <- "ANS"
groups$College.x[730] <- "AES"

groups$Department[742] <- "SPH"
groups$College.x[742] <- "MED"

groups$Department[866] <- "COM"
groups$College.x[866] <- "LS"

groups$Department[1013] <- "ENL"
groups$College.x[1013] <- "LS"

groups$Department[1022] <- "ENL,STS"
groups$College.x[1022] <- "LS"

groups$Department[1031] <- "GSW"
groups$College.x[1031] <- "LS"

groups <- groups[-1504]

groups$Department[1745] <- "FRE"
groups$College.x[1745] <- "LS"

groups$Department[1838] <- "NRS"
groups$College.x[1838] <- "NRS"

groups$Department[1844] <- "NRS"
groups$College.x[1844] <- "NRS"

groups$Department[1993] <- "EDU"
groups$College.x[1993] <- "EDU"

groups$Department[2001] <- "EDU"
groups$College.x[2001] <- "EDU"

groups$Department[2007] <- "EDU"
groups$College.x[2007] <- "EDU"

groups$Department[3856] <- "STA"
groups$College.x[3856] <- "LS"

groups$Department[3864] <- "STA"
groups$College.x[3864] <- "LS"

groups$Department[3917] <- "ECI"
groups$College.x[3917] <- "ENG"

groups <- groups[-3919]

groups$Department[3924] <- "ESP"
groups$College.x[3924] <- "AES"

groups <- groups[-3927]

groups$Department[3940] <- "VEN"
groups$College.x[3940] <- "AES"

#more abbreviations cleaning
groups$Department <- str_replace_all(groups$Department, "Hydrological Sciences", "HYD")

groups$Department <- str_replace_all(groups$Department, "Clinical Population Health Reproduction", "PHR")

groups$Department <- str_replace_all(groups$Department, "Animal Science Helman Fellow", "ANS")

groups$Department <- str_replace_all(groups$Department, "Clinical Nutrition", "NUT")

groups$Department <- str_replace_all(groups$Department, "Cooperative Extension", "")
groups$Department <- str_replace_all(groups$Department, "Veterinary Genetics Laboratory", "VGL")
groups$Department <- str_replace_all(groups$Department, "PHR, Poultry Medicine", "PHR")
groups$Department <- str_replace_all(groups$Department, "School of Veterinary Medicine  Research Center", "PHR")
groups$Department[235] <- "VSR"
groups$Department <- str_replace_all(groups$Department, "Comparitive Literature", "COM")
groups$Department <- str_replace_all(groups$Department, "Comparitave Literature", "COM")
groups$Department <- str_replace_all(groups$Department, "Crocker Nuclear Laboratory", "")
groups$Department[372] <- "LAWR"
groups$Department <- str_replace_all(groups$Department, "Bodega Marine Laboratory", "")
groups$Department[378] <- "ESP"
groups$Department <- str_replace_all(groups$Department, "Neurology,", "NEU")
groups$Department <- str_replace_all(groups$Department, "Urologic Surgery", "USU")
groups$Department <- str_replace_all(groups$Department, "Ophthalmology Vision Science", "OVS")
groups$Department <- str_replace_all(groups$Department, "Radiology", "RDI")
groups$Department <- str_replace_all(groups$Department, "Biochemistry Molecular Medicine Internal Medicine", "BMM, IMD")
groups$Department <- str_replace_all(groups$Department, "Orthopedic Surgery Biomedical Engineering", "OSU, BIM")
groups$Department <- str_replace_all(groups$Department, "NEU, Center for Mind Brain", "NEU")
groups$Department <- str_replace_all(groups$Department, "Neurological Surgery", "NSU")
groups$Department <- str_replace_all(groups$Department, "NPB, Center for Mind Brain", "NPB")
groups$Department <- str_replace_all(groups$Department, "Biomedical Engineering Radiology", "BIM, RDI")
groups$Department <- str_replace_all(groups$Department, "General Pediatrics Cell Biology Human Anatomy", "PED, CHA")
groups$Department <- str_replace_all(groups$Department, "Physiology Membrane Biology Pharmacology", "PMB, PRC")
groups$Department <- str_replace_all(groups$Department, "Surgery Biomedical Engineering", "SUR, BIM")
groups$Department <- str_replace_all(groups$Department, "Oncology", "HON")
groups$Department <- str_replace_all(groups$Department, "Dept. of Evolution Ecology", "EVE")
groups$Department <- str_replace_all(groups$Department, "Vet Med", "PHR")
groups$Department <- str_replace_all(groups$Department, "Biomedical Engineering Genome Center", "BIM")
groups$Department <- str_replace_all(groups$Department, "Director, UC Davis West Coast Metabolomics Center", "MCB")
groups$Department <- str_replace_all(groups$Department, "Genome Center", "BIM")
groups$Department <- str_replace_all(groups$Department, "Atmospheric / Environmental Chemistry", "ETX")
groups <- groups[-823, ]
groups$Department <- str_replace_all(groups$Department, "Textiles CLothing", "DES, GSW")

#replacing empty departments with grad group name
for(i in 1:nrow(groups)){
  if(groups$Graduate.Group[i] == "Community Development" && groups$Department[i] == "NA")
    groups$Department[i] <- "CRD"
}

for(i in 1:nrow(groups)){
  if(groups$Graduate.Group[i] == "Entomology" && groups$Department[i] == "NA")
    groups$Department[i] <- "ENT/NEM"
}

#abbreviation cleaning
groups$Department <- str_replace_all(groups$Department, "Art", "ART")
groups$Department <- str_replace_all(groups$Department, "Comparative Literature,Middle East/South Asia Studies", "COM, MSA")
groups$Department <- str_replace_all(groups$Department, "Russian / Comparative Literature", "RUS, COM")
groups$Department <- str_replace_all(groups$Department, "TAD, Lighting, Sound Projections", "TAD")
groups$Department <- str_replace_all(groups$Department, "Dance", "TAD")
groups$Department <- str_replace_all(groups$Department, "Professor, EEC", "EEC")
groups$Department <- str_replace_all(groups$Department, "Technical Director, Center for Mind Brain", "NPB")
groups$Department <- str_replace_all(groups$Department, "Neurology Division of Epilepsy", "NEU")
groups$Department <- str_replace_all(groups$Department, "Institute of Transportation Studies", "ECL")
groups$Department <- str_replace_all(groups$Department, "Energy Efficiency Institute", "MAE")
groups$Department <- str_replace_all(groups$Department, "Western Cooling Efficiency Center", "MAE")
groups$Department <- str_replace_all(groups$Department, "California Lighting Technology Center", "DES")
groups$Department <- str_replace_all(groups$Department, "ENT, Director of The Bohart Museum of Entomology", "ENT")
groups$Department <- str_replace_all(groups$Department, "Urban Landscape Entomology", "ENT")
groups$Department <- str_replace_all(groups$Department, "VEN,  Chemical Engineering Materials Science", "VEN, ECH, EMS")
groups$Department <- str_replace_all(groups$Department, "FST, , Foods for Health Institute", "FST")
groups$Department <- str_replace_all(groups$Department, "VM: Population Health Reproduction", "PHR")
groups$Department <- str_replace_all(groups$Department, "FRE, ITA, LIN, the DE GSW", "FRE, ITA, LIN, GSW")
groups$Department <- str_replace_all(groups$Department, "BIM, Genomics", "BIM")
groups$Department <- str_replace_all(groups$Department, "NPB, Center for Neuroscience", "NPB")
groups$Department <- str_replace_all(groups$Department, "Neurology Center for Neuroscience", "NEU")
groups$Department <- str_replace_all(groups$Department, "Biomedical Engineering RDI", "BIM, RDI")
groups$Department[958] <- str_replace_all(groups$Department[958], "CSS", "NEU")
groups$College.x[958] <- str_replace_all(groups$College.x[958], "ENG", "MED")
groups$Department[965] <- str_replace_all(groups$Department[965], "CSS", "HYD")
groups$College.x[965] <- str_replace_all(groups$College.x[965], "ENG", "AES")
groups$Department <- str_replace_all(groups$Department, "EV Research Center", "ESP")
groups$Department <- str_replace_all(groups$Department, "NEU\\\\", "NEU")

#inputing matching colleges for departments

for(i in 1:nrow(groups)) {
  if(groups$Department[i] %in% c("LAWR", "FST, BAE", "FST, VEN", "FST, NUT","ENT/NEM","PLP","VEN","ENT, NEM","BAE, LAWR", "ESP", "BAE", "FST", "ETX", "LDA, EVD","HCL", "ANS","NUT", "Human Ecology", "CRD", "WFC","ARE", "PLS", "BAE, FST", "Viticulture Enology", "HYD", "CRD", "BAE, VEN", "ECL")) {
    groups$College.x[i] <- "AES"
  }
}


for(i in 1:nrow(groups)) {
  if(groups$Department[i] %in% c("CHE", "EPS", "AAS", "ENL, COM", "FRE, COM", "CLA, COM", "COM, MSA", "NAS, COM", "RUS, COM", "SPA, POR, COM, CRI", "CDM, STS", "DES, AMS", "TAD","SOC", "Chicano Studies","NAS","ASA" ,"Chemistry", "PSC", "CMN", "ANT", "STA", "MAT", "COM", "HIS", "DES", "ART", "PHY", "ECN", "ENL", "ART, AHI", "CDM", "CHI", "DES, GSW")) {
    groups$College.x[i] <- "LS"
  }
}


for(i in 1:nrow(groups)) {
  if(groups$Department[i] %in% c("ECI", "ECH, EMS","EMS", "Computer Science", "BIM", "EEC", "CSS", "MAE", "ECH","Chemical Engineering", "MAE, CSS", "EEC, CSS")) {
    groups$College.x[i] <- "ENG"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Department[i] %in% c("NPB", "MCB", "EVE", "Microbiology Molecular Genetics", "Plant Biology", "PLANT BIOLOGY", "PLB", " Molecular Cellular Biology", "	Plant Biology", " Evolution Ecology","Medical Microbiology", "MMG", "Dept of NPB")) {
    groups$College.x[i] <- "BIO"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Department[i] %in% c("SPH", "PMD, MMI","OVS","OSU", "USU", "PMB", "MMI", "PLM", "CHA", "IMD", "BMM", "CAR, Pharmacology", "Pharmacology", "PRC", "Radiology", "RDI", "NEU", "RON", "SUR", "CAR, PRC")) {
    groups$College.x[i] <- "MED"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Department[i] %in% c("APC", "VET","PHR", "VSR", "VGL", "VME", "VMB", "NUT, VMB", "VET, EPI")) {
    groups$College.x[i] <- "VETMED"
  }
}

#subsetting first 50 grad groups
first_50 <- groups[1:1748, ]
first_50 <- first_50[!first_50$Graduate.Group %in% c("Education (M.A.)", "Education (Ph.D.) ", "Educational Leadership (CANDEL)", "Epidemiology", "Graduate School of Management Programs", "Forensic Science"), ]

#removing cells with no department
first_50 <- first_50[first_50$Department != "NA", ]
first_50 <- first_50[first_50$Department != "", ]
first_50 <- first_50[first_50$Department != "no departments listed", ]

#remove leading spaces
first_50 <- first_50 %>%
  mutate(College.x = str_trim(College.x, side = "left"))

#abbreviate colleges
first_50$College.x <- str_replace_all(first_50$College.x, "Biological Sciences", "BIO")
first_50$College.x <- str_replace_all(first_50$College.x, "Letters and Sciences", "LS")
first_50$College.x <- str_replace_all(first_50$College.x, "Engineering", "ENG")
first_50$College.x <- str_replace_all(first_50$College.x, "Management", "MGT")
first_50$College.x <- str_replace_all(first_50$College.x, "Medicine", "MED")
first_50$College.x <- str_replace_all(first_50$College.x, "Agricultural and Environmental Sciences", "AES")
first_50$College.x <- str_replace_all(first_50$College.x, "School Of Law", "LAW")

#abbreviate departments
first_50$Department <- str_replace_all(first_50$Department, "ENT, NEM", "ENT/NEM")
first_50$Department <- str_replace_all(first_50$Department, "SPA, POR", "SPA/POR")
first_50$Department <- str_replace_all(first_50$Department, "GER, RUS", "GER/RUS")
first_50$Department <- str_replace_all(first_50$Department, "FRE, ITA", "FRE/ITA")
first_50$Department <- str_replace_all(first_50$Department, "NPB NEU", "NPB, NEU")

first_50 <- first_50[-1259, ]
first_50 <- first_50[-296, ]

#function that splits departments separated by commas into separate rows
#This is being done for visualization purposes so some faculty may be counted multiple times, but this will help show the department make up of the grad groups

write.csv(first_50, "first_50.csv", row.names = FALSE) #save cleaned data before manipulations

first_50 <- first_50 %>%
  separate_rows(Department, sep = ",") #separates departments into separtate rows by column

first_50 <- first_50 %>%
  mutate(Department = str_trim(Department, side = "left")) #remove leading spaces

#sorting departments into colleges
for(i in 1:nrow(first_50)) {
  if(first_50$Department[i] %in% c("LAWR", "ENT","NEM","FST, BAE", "FST, VEN", "FST, NUT","ENT/NEM","PLP","VEN","ENT, NEM","BAE, LAWR", "ESP", "BAE", "FST", "ETX", "LDA, EVD","HCL", "ANS","NUT", "Human Ecology", "CRD", "WFC","ARE", "PLS", "BAE, FST", "Viticulture Enology", "HYD", "CRD", "BAE, VEN", "ECL", "MSA")) {
  first_50$College.x[i] <- "AES"
  }
}


for(i in 1:nrow(first_50)) {
  if(first_50$Department[i] %in% c("CHE", "TCS","CRI","POL","ITA","MUS","EPS", "AAS", "ENL, COM", "FRE, COM", "CLA, COM", "COM, MSA", "NAS, COM", "RUS, COM", "SPA, POR, COM, CRI", "CDM, STS", "DES, AMS", "TAD","SOC", "Chicano Studies","NAS","ASA" ,"Chemistry", "PSC", "CMN", "GSW","GER","ANT", "STA", "MAT", "COM", "HIS", "DES", "ART", "PHY", "ECN", "ENL", "ART, AHI", "CDM", "CHI", "DES, GSW", "FRE", "FRE/ITA", "LIN")) {
    first_50$College.x[i] <- "LS"
  }
}


for(i in 1:nrow(first_50)) {
  if(first_50$Department[i] %in% c("ECI", "ECS","ECH, EMS","EMS", "Computer Science", "BIM", "EEC", "CSS", "MAE", "ECH","Chemical Engineering", "MAE, CSS", "EEC, CSS")) {
 first_50$College.x[i] <- "ENG"
  }
}

for(i in 1:nrow(first_50)) {
  if(first_50$Department[i] %in% c("NPB", "MCB", "EVE", "Microbiology Molecular Genetics", "Plant Biology", "PLANT BIOLOGY", "PLB", " Molecular Cellular Biology", "	Plant Biology", " Evolution Ecology","Medical Microbiology", "MMG", "Dept of NPB")) {
    first_50$College.x[i] <- "BIO"
  }
}

for(i in 1:nrow(first_50)) {
  if(first_50$Department[i] %in% c("SPH", "PMR","CAR","PMD, MMI","OVS","OSU", "USU", "PMB", "MMI", "PLM", "CHA", "IMD", "BMM", "CAR, Pharmacology", "Pharmacology", "PRC", "Radiology", "RDI", "NEU", "RON", "SUR", "CAR, PRC")) {
    first_50$College.x[i] <- "MED"
  }
}

for(i in 1:nrow(first_50)) {
  if(first_50$Department[i] %in% c("APC", "VET","PHR", "VSR", "VGL", "VME", "VMB", "NUT, VMB", "VET, EPI")) {
    first_50$College.x[i] <- "VETMED"
  }
}

#factoring the department and college columns
first_50$Department <- factor(first_50$Department)
first_50$College.x <- factor(first_50$College.x)

#creating proportions of each department out of total departments
proportions <- first_50 %>%
  group_by(Graduate.Group, Department) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

#proportions of each department in each grad group
normalized_proportions <- proportions %>%
  group_by(Graduate.Group) %>%
  mutate(Normalized.Proportion = Proportion / sum(Proportion))

#remove graduate group name and add the departments together
proportions_dep <- first_50 %>%
  group_by(Department) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

top_departments <- tail(proportions_dep[order(proportions_dep$Proportion), ], n=10)
top_departments <- top_departments[-c(9,10), ]
#excluding ECL and CSS since they are likely overrepresented. they were filled in based on grad group name and we didn't have info on official departments in the website 

#graph of most popular departments represented
my_palette <- viridis_pal(option = "C")(8)

ggplot(top_departments, aes(x = Proportion, y = Department, fill = Department)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_palette) + 
  labs(title = "Most Common Departments",
       x = "Proportion",
       y = "Department") +
  theme_minimal()

#each college representation in grad groups
departments <- data.frame(levels(first_50$Department))
colnames(departments) <- "department"


proportions_college <- first_50 %>%
  group_by(Graduate.Group, College.x) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

normalized_proportions_college <- proportions_college %>%
  group_by(Graduate.Group) %>%
  mutate(Normalized.Proportion = Proportion / sum(Proportion))

ggplot(normalized_proportions_college, aes(x = Normalized.Proportion, y = Graduate.Group, fill = College.x)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#022851", "#B3C1D1", "#CC79A7", "#6FCFEB", "#FFBF00", "#D55E00", "#0072B2", "#009E73", "#00441B")) + 
  labs(title = "Proportion of Each College in Each Graduate Group",
       x = "Proportion",
       y = "Graduate Group") +
  theme_minimal()

#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# last 50 groups
#grouping grad groups that don't have departments info or do not have anything to do with colleges
no_dep_groups <- groups[groups$Graduate.Group %in% c("Health Informatics",
                                                     "Integrative Genetics and Genomics", "Integrative Pathobiology",
                                                     "Law (LL.M.)","Microbiology","Molecular, Cellular and Integrative Physiology",
                                                     "Neuroscience","Nursing (Doctor of Nursing Practice - Family Nurse Practitioner)",
                                                     "Nursing","Preventive Veterinary Medicine","Public Health (MPH)","Hydrologic Sciences",
                                                     "International Agricultural Development","Maternal and Child Nutrition",
                                                     "Nutritional Biology","Pharmaceutical Chemistry","Professional Accountancy",
                                                     "Nursing (Doctor of Nursing Practice - Family Nurse Practitioner) ","Performance Studies","Immunology"), ]

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Philosophy")) {
    groups$Department[i] <- "PHI"
    groups$College.x[i] <- "LS"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Mechanical and Aerospace Engineering")) {
    groups$Department[i] <- "MAE"
    groups$College.x[i] <- "ENG"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Physics")) {
    if (groups$Department[i]=="NA"){
      groups$Department[i] <- "PHY"
    }
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Plant Pathology")) {
    if (groups$Department[i]=="NA"|groups$Department[i]==""|groups$Department[i]=="Cooperative Extension"){
      groups$Department[i] <- "PLP"
    }
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Political Science")) {
    if (groups$Department[i]=="NA"|groups$Department[i]==""){
      groups$Department[i] <- "POL"
    }
  }
}


for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Psychology")) {
    groups$Department[i] <- "PSC"
    groups$College.x[i] <- "LS"
  }
}

for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Sociology")) {
    groups$Department[i] <- "PSC"
    groups$College.x[i] <- "LS"
  }
}
for(i in 1:nrow(groups)) {
  if(groups$Graduate.Group[i] %in% c("Soils and Biogeochemistry")) {
    groups$Department[i] <- "SSC"
    groups$College.x[i] <- "AES"
  }
}

groups$Department <- replace(groups$Department, 2065, "PUL")
groups$Department <- replace(groups$Department, 2084, "IMD, CHA")
groups$Department <- replace(groups$Department, 3372, "CSS, MAE")
groups$Department <- replace(groups$Department, 3383, "PHY")
groups$Department <- replace(groups$Department, 3900, "CLA")
groups$Department <- replace(groups$Department, 3908, "TTP") 
groups$Department <- replace(groups$Department, 3913, "TTP")
groups$Department <- replace(groups$Department, 3914, "TTP")
groups$Department <- replace(groups$Department, 3916, "TTP")
groups$Department <- replace(groups$Department, 3919, "TTP")
groups$Department <- replace(groups$Department, 3922, "TTP")
groups$Department <- replace(groups$Department, 3925, "TTP")
groups$Department <- replace(groups$Department, 3926, "TTP")
groups$Department <- replace(groups$Department, 3927, "TTP")
groups$Department <- replace(groups$Department, 3928, "TTP")
groups$Department <- replace(groups$Department, 3929, "TTP")
groups$Department <- replace(groups$Department, 3840, "LAT")
groups$Department <- replace(groups$Department, 3842, "POR, SPA")
groups$Department <- replace(groups$Department, 2525, "OSU")
groups$Department <- replace(groups$Department, 3401, "PHY, AST")
groups$Department <- replace(groups$Department, 3508, "POL")
groups$Department <- replace(groups$Department, 1984, "PSY")
groups$Department <- replace(groups$Department, 3841, "SPA")
groups$Department <- replace(groups$Department, 3949, "VEN")
groups$Department <- replace(groups$Department, 3555, "WFC")

groups$College.x <- replace(groups$College.x, 2065, "MED")
groups$College.x <- replace(groups$College.x, 2084, "MED")
groups$College.x <- replace(groups$College.x, 3372, "ENG")
groups$College.x <- replace(groups$College.x, 3383, "LS")
groups$College.x <- replace(groups$College.x, 3900, "LS")
groups$College.x <- replace(groups$College.x, 3840, "LS")
groups$College.x <- replace(groups$College.x, 3842, "LS")
groups$College.x <- replace(groups$College.x, 2525, "MED")
groups$College.x <- replace(groups$College.x, 3401, "LS")
groups$College.x <- replace(groups$College.x, 3508, "LS")
groups$College.x <- replace(groups$College.x, 1984, "LS")


groups$Department <- str_replace_all(groups$Department, "Applied MAT", "MAT")
groups$Department <- str_replace_all(groups$Department, "GER, RUS, Language Center", "GER, RUS")

groups$Department <- gsub("Art", "ART", groups$Department, ignore.case = TRUE) 


#last 50 grad groups
last_50 <- groups[1749:3950, ]

#excluding groups that are not relevant
last_50 <- last_50[!last_50$Graduate.Group %in% no_dep_groups$Graduate.Group, ]

last_50 <- last_50[last_50$Department != "NA", ]
last_50 <- last_50[last_50$Department != "", ]
last_50 <- last_50[last_50$Department != "no departments listed", ]
last_50 <- last_50 %>%
  mutate(College.x = str_trim(College.x, side = "left"))


last_50$College.x <- str_replace_all(last_50$College.x, "Biological Sciences", "BIO")
last_50$College.x <- str_replace_all(last_50$College.x, "Letters and Sciences", "LS")
last_50$College.x <- str_replace_all(last_50$College.x, "Engineering", "ENG")
last_50$College.x <- str_replace_all(last_50$College.x, "Management", "MGT")
last_50$College.x <- str_replace_all(last_50$College.x, "Medicine", "MED")
last_50$College.x <- str_replace_all(last_50$College.x, "Agricultural and Environmental Sciences", "AES")
last_50$College.x <- str_replace_all(last_50$College.x, "School Of Law", "LAW")
last_50$College.x <- str_replace_all(last_50$College.x, "Education", "EDU")
last_50$College.x <- str_replace_all(last_50$College.x, "L&S", "LS")




last_50$Department <- str_replace_all(last_50$Department, "ENT, NEM", "ENT/NEM")
last_50$Department <- str_replace_all(last_50$Department, "SPA, POR", "SPA/POR")
last_50$Department <- str_replace_all(last_50$Department, "GER, RUS", "GER/RUS")
last_50$Department <- str_replace_all(last_50$Department, "FRE, ITA", "FRE/ITA")
last_50$Department <- str_replace_all(last_50$Department, "NPB NEU", "NPB, NEU")


last_50 <- last_50[!is.na(last_50$Name), ]


last_50 <- last_50 %>%
  separate_rows(Department, sep = ",")

last_50 <- last_50 %>%
  mutate(Department = str_trim(Department, side = "left"))

for(i in 1:nrow(last_50)) {
  if(last_50$Department[i] %in% c("ATM","LAWR", "ENT","NEM","FST, BAE", "FST, VEN", "FST, NUT","ENT/NEM","PLP","VEN","ENT, NEM","BAE, LAWR", "ESP", "BAE", "FST", "ETX", "LDA, EVD","HCL", "ANS","NUT", "Human Ecology", "CRD", "WFC","ARE", "PLS", "BAE, FST", "Viticulture Enology", "HYD", "CRD", "BAE, VEN", "ECL")) {
    last_50$College.x[i] <- "AES"
  }
}


for(i in 1:nrow(last_50)) {
  if(last_50$Department[i] %in% c("MSA","RST","MPS","UWP","SPA","PSY","AHI","CLA","AMS", "CHE", "TCS","CRI","POL","ITA","MUS","EPS", "AAS", "ENL, COM", "FRE, COM", "CLA, COM", "COM, MSA", "NAS, COM", "RUS, COM", "SPA, POR, COM, CRI", "CDM, STS", "DES, AMS", "TAD","SOC", "Chicano Studies","NAS","ASA" ,"Chemistry", "PSC", "CMN", "GSW","GER","ANT", "STA", "MAT", "COM", "HIS", "DES", "ART", "PHY", "ECN", "ENL", "ART, AHI", "CDM", "CHI", "DES, GSW", "FRE", "FRE/ITA", "LIN", "GER/RUS")) {
    last_50$College.x[i] <- "LS"
  }
}


for(i in 1:nrow(last_50)) {
  if(last_50$Department[i] %in% c("ECI", "ECS","ECH, EMS","EMS", "Computer Science", "BIM", "EEC", "CSS", "MAE", "ECH","Chemical Engineering", "MAE, CSS", "EEC, CSS")) {
    last_50$College.x[i] <- "ENG"
  }
}

for(i in 1:nrow(last_50)) {
  if(last_50$Department[i] %in% c("NPB", "MCB", "EVE", "Microbiology Molecular Genetics", "Plant Biology", "PLANT BIOLOGY", "PLB", " Molecular Cellular Biology", "	Plant Biology", " Evolution Ecology","Medical Microbiology", "MMG", "Dept of NPB")) {
    last_50$College.x[i] <- "BIO"
  }
}

for(i in 1:nrow(last_50)) {
  if(last_50$Department[i] %in% c("PUL","PED","SPH", "PMR","CAR","PMD, MMI","OVS","OSU", "USU", "PMB", "MMI", "PLM", "CHA", "IMD", "BMM", "CAR, Pharmacology", "Pharmacology", "PRC", "Radiology", "RDI", "NEU", "RON", "SUR", "CAR, PRC")) {
    last_50$College.x[i] <- "MED"
  }
}

for(i in 1:nrow(last_50)) {
  if(last_50$Department[i] %in% c("APC", "VET","PHR", "VSR", "VGL", "VME", "VMB", "NUT, VMB", "VET, EPI")) {
    last_50$College.x[i] <- "VETMED"
  }
}


#-------------------------------------------------------------------------------


last_50$Department <- factor(last_50$Department)
last_50$College.x <- factor(last_50$College.x)

#creating proportions of each department out of total departments
proportions <- last_50 %>%
  group_by(Graduate.Group, Department) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

#proportions of each department in each grad group
normalized_proportions <- proportions %>%
  group_by(Graduate.Group) %>%
  mutate(Normalized.Proportion = Proportion / sum(Proportion))

#remove graduate group name and add the departments together
proportions_dep <- last_50 %>%
  group_by(Department) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

top_departments <- tail(proportions_dep[order(proportions_dep$Proportion), ], n=10)
top_departments <- top_departments[-c(9,10), ]
#excluding ECL and CSS since they are likely overrepresented. they were filled in based on grad group name and we didn't have info on official departments in the website 

#graph of most popular departments represented
my_palette <- viridis_pal(option = "C")(8)

ggplot(top_departments, aes(x = Proportion, y = Department, fill = Department)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_palette) + 
  labs(title = "Most Common Departments",
       x = "Proportion",
       y = "Department") +
  theme_minimal()

#each college representation in grad groups
departments <- data.frame(levels(last_50$Department))
colnames(departments) <- "department"


proportions_college <- last_50 %>%
  group_by(Graduate.Group, College.x) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

normalized_proportions_college <- proportions_college %>%
  group_by(Graduate.Group) %>%
  mutate(Normalized.Proportion = Proportion / sum(Proportion))

ggplot(normalized_proportions_college, aes(x = Normalized.Proportion, y = Graduate.Group, fill = College.x)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#022851", "#B3C1D1", "#CC79A7", "#6FCFEB", "#FFBF00", "#0072B2", "#00441B")) + 
  labs(title = "Proportion of Each College in Each Graduate Group",
       x = "Proportion",
       y = "Graduate Group") +
  theme_minimal()

