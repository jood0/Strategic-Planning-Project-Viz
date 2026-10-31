#install.packages("humaniformat")
#install.packages("dplyr")

library(humaniformat)
library(dplyr)

# set working directory
setwd("2024_ls_strategic_plan")

# read in .csv file
gradCenterData <- read.csv("data/GradCentersScrapedRough.csv")
colnames(gradCenterData) = c("Center", "Name", "Title", "Department", "Email", "Website")

# get and parse names
gradCenterDataParsedNames = parse_names(gradCenterData$Name)

# get faculty data
facultyData <- read.csv("data/faculty_department.csv")
facultyData = data.frame(facultyData$Employee.Name, facultyData$Department)

colnames(facultyData) = c("Name", "facultyDepartment")

# Splitting the Name column
name_parts <- strsplit(facultyData$Name, ",")

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

facultyData$Name <- new_names

# get and parse names
facultyDataParsedNames = parse_names(facultyData$Name)

#merge parsed names back into their dataFrames
gradCenterData$Name = paste(gradCenterDataParsedNames$first_name, gradCenterDataParsedNames$last_name)
facultyData$Name = paste(facultyDataParsedNames$first_name, facultyDataParsedNames$last_name)

facultyData <- facultyData[facultyData$Name %in% gradCenterData$Name, ]

facultyData <- facultyData %>%
  group_by(Name) %>%
  summarize(facultyDepartment = paste(unique(facultyDepartment), collapse = ", "))

# Identify and handle duplicate names
duplicated_gradCenter <- duplicated(gradCenterData$Name)
duplicated_faculty <- duplicated(facultyData$Name)

# Merge data frames, keeping only unique matches
merged_data <- merge(gradCenterData[!duplicated_gradCenter, ], facultyData[!duplicated_faculty, ], by = "Name", all.x = TRUE)

# add a new column to see if the majors do match
# Function to determine the match type
match_type <- function(dept1, dept2) {
  if (is.na(dept1) | is.na(dept2) | dept1 == "" | dept2 == "") {
    return(0)  # Return 0 if either department name is NA or empty
  } else if (dept1 == dept2) {
    return(2)  # Return 2 if departments match exactly
  } else if (length(intersect(strsplit(dept1, " ")[[1]], strsplit(dept2, " ")[[1]])) > 0) {
    return(1)  # Return 1 if departments have a common word
  } else {
    return(0)  # Return 0 if no match
  }
}

merged_data$match <- mapply(match_type, merged_data$Department, merged_data$facultyDepartment)

# Assuming your data frame is named 'df'
merged_data <- merged_data %>%
  mutate(verifiedDepartment = 
           case_when(
             match %in% c(1, 2) ~ facultyDepartment,  # match is 1 or 2
             match == 0 & Department == "" ~ facultyDepartment,  # match is 0 and Department is empty
             match == 0 & is.na(Department) ~ facultyDepartment,
             match == 0 & facultyDepartment == "" ~ Department,  # match is 0 and facultyDepartment is empty
             match == 0 & is.na(facultyDepartment) ~ Department,
             
             TRUE ~ NA_character_  # any other cases (not expected based on description)
           )
  )

write.csv(merged_data, "gradCenterFacultyDepartments.csv", row.names = FALSE)
