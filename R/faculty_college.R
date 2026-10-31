
department_college <- read.csv("department_college.csv")
#read csv file

class(department_college$College) #character; we need to change it to a factor

department_college$College <- factor(department_college$College)
levels(department_college$College)

#load the faculty department data
library(readxl)
faculty_department <- read_excel(path = "Copy of Joint faculty appointments.xlsx", sheet = 2)

#rename department description column to just department
colnames(faculty_department) 
colnames(faculty_department)[colnames(faculty_department) == 'Department Description'] <- "Department"

#factor department column
faculty_department$Department <- factor(faculty_department$Department)
levels(faculty_department$Department)

#create a subset of department_college that's only L&S
LS <- department_college[department_college$College == 'Letters and Sciences', ]

#load stringr - now we can start normalizing the department names
library(stringr)

#according to this website? - https://lettersandscience.ucdavis.edu/departments-and-programs

#EXTRAS - all other departments had minor differences or were the same between both data sets

#Art and Art History - many groups
#East Asian Languages and Cultures - add east asian studies is a separate department
#History - projects
#Sociology - social sciences program
#Statistics - lab


#Programs

#East Asian Studies -> add to colleges
#Human Rights Studies -> add to colleges
#Humanities Program -> change in colleges data
#Medieval Studies -> add to colleges data

#History Projects -> unify into history
#Social Sciences Program -> unify with sociology
#statistics laboratory (LS) -> unify with statistics


#cleaning faculty_department data

faculty_department$Department <- str_to_title(str_to_lower(faculty_department$Department))

faculty_department$Department <- str_replace_all(faculty_department$Department, "And", "and")

faculty_department$Department <- str_replace_all(faculty_department$Department, "&", "and")

faculty_department$Department <- str_replace_all(faculty_department$Department, "Of", "of")

faculty_department$Department <- str_replace_all(faculty_department$Department, "&", "and")

faculty_department$Department <- str_replace_all(faculty_department$Department, "Department of Design", "Design")

faculty_department$Department <- str_replace_all(faculty_department$Department, "African American African Stds", "African American and African Studies")

faculty_department$Department <- str_replace_all(faculty_department$Department,"Gendersexuality Womensstudies", "Gender, Sexuality and Women's Studies")

faculty_department$Department<- str_replace_all(faculty_department$Department,"Chicano Studies", "Chicana/Chicano Studies")

faculty_department$Department <- str_replace_all(faculty_department$Department,"East Asian Lang. and Cultures", "East Asian Languages and Cultures")

faculty_department$Department <- str_replace_all(faculty_department$Department,"Middle East/South Asia Program", "Middle Eastern/South Asia Studies")

faculty_department$Department <- str_replace_all(faculty_department$Department, "Asian American", "Asian American Studies" )

faculty_department$Department <- str_replace_all(faculty_department$Department, "Medieval Studies", "Medieval and Early Modern Studies")

faculty_department$Department <- str_replace_all(faculty_department$Department, "LandS Dean - Admin", "Dean's Office, L&S")

faculty_department$Department <- str_replace_all(faculty_department$Department, "LandS Dean - Social Sciences", "Dean's Office, L&S")

faculty_department$Department <- str_replace_all(faculty_department$Department, "LandS Dean - Social Sciences", "Dean's Office, L&S")

faculty_department$Department <- str_replace_all(faculty_department$Department, "Arts Administrative Group", "Art and Art History")


faculty_department$Department <- str_replace_all(faculty_department$Department, "Cultural Studies Grad Group", "Cultural Studies")

#write.csv(faculty_department, file = "C:/Users/mbiye/2024_ls_strategic_plan-main/R/faculty_department.csv")


#cleaning department_college data

LS$Department <- str_replace_all(LS$Department, "Davis Humanities Institute", "Humanities")

which(LS$Department == 'Statistics Laboratory')
LS <- LS[-54, ] #delete duplicate

LS$Department <- str_replace_all(LS$Department, "Physical Education", "Physical Education Program") 

LS$Department <- str_replace_all(LS$Department, "Chemistry, M&PS", "Chemistry")

LS$Department <- str_replace_all(LS$Department, "Earth & Planetary Sciences", "Earth and Planetary Sciences")

LS$Department <- str_replace_all(LS$Department, "Hart Hall Interdisciplinary Programs Administration", "Hart Interdisciplinary Program")

LS$Department <- str_replace_all(LS$Department, "Art History", "Art and Art History")

LS$Department <- str_replace_all(LS$Department, "German & Russian", "German and Russian")

LS$Department <- str_replace_all(LS$Department, "Studio", "and Art History")

LS$Department <- str_replace_all(LS$Department, "The Arts Administrative Group", "Art and Art History")

which(LS$Department == "Art and Art History")
LS <- LS[-c(16,23), ] #delete duplicates

#write.csv(LS, file = "C:/Users/mbiye/2024_ls_strategic_plan-main/R/LS.csv")


#EXTRA CHANGES FOR PURPOSES OF VISUALIZATIONS

### Interdisciplinary Projects are being converted to home departments

faculty_department$Department <- str_replace_all(faculty_department$Department, "California History Ss Project", "History")

faculty_department$Department <- str_replace_all(faculty_department$Department, "History Project Ucd", "History")

faculty_department$Department <- str_replace_all(faculty_department$Department, "Center For Mind and Brain", "Psychology")

faculty_department$Department <- str_replace_all(faculty_department$Department, "Lighting Technology Center", "Design")

faculty_department$Department <- str_replace_all(faculty_department$Department, "Language Learning Center", "Linguistics")

faculty_department$Department <- factor(faculty_department$Department)

which(LS$Department %in% c( "Center for Mind and Brain", "Language Learning Center"))
LS <- LS[-c(28,54), ]

#deleting "clusters" since it's not commonly considered a department
which(LS$Department %in% c("Gorman Museum","General I&R, L&S", "Yellow Cluster, DSS", "Green Cluster, DSS", "Voorhies Administration", "Orange Cluster, DSS", "Blue Cluster, DSS", "Crocker Nuclear Laboratory", "Hemispheric Institute on the Americas", "Ann E. Pitzer Center - Non-Standard Programs"))
LS <- LS[-c(2, 11, 13, 23, 24, 28, 40, 45, 49, 52), ]

LS$Department <- factor(LS$Department)

levels(LS$Department)
levels(faculty_department$Department)


#use merge to combine the data frames with common department column

faculty_college = merge(LS, faculty_department, by = "Department", all = TRUE) #this includes all official departments and faculty mapped (no projects or centers -> separate data frame may need to be created for that info, but it is not necessary for this project, aka why I included the cleaned versions of the separate data sets)

which(is.na(faculty_college$College)) #replace any department not affiliated with a college
faculty_college$College[is.na(faculty_college$College)] <- "Letters and Sciences"

#fix name formatting
faculty_college$`Employee Name` <- str_to_title(str_to_lower(faculty_college$`Employee Name`))

faculty_college$`Employee Name` <- str_replace_all(faculty_college$`Employee Name`, ",", ", ")

#write.csv(faculty_college, file = "C:/Users/mbiye/2024_ls_strategic_plan-main/R/faculty_college_data.csv")

#VISUALIZATIONS

library(ggplot2)

#histogram of the number of faculty in each department

faculty_college$Department <- factor(faculty_college$Department, levels = names(sort(table(faculty_college$Department), decreasing = TRUE))) #want a descending bar graph

ggplot(faculty_college, aes(x = Department, fill = "pink")) + geom_bar(width = 0.9) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Faculty per Department", x = "Department", y = "Faculty Count") 


#faculty working in multiple departments
  #categorize (factor) by name - assign 1 to a name and 0 to the rest (binary), then sum up the rows and if it the result is greater than 1 then we can
  #group_by - merges the faculty names
  #new vector = factor levels
  #assign levels 

faculty <- data.frame(faculty_college$Department, faculty_college$`Employee Name`) #making a separate data frame with just the department and faculty names

library(reshape2)
table <- dcast(faculty_college, `Employee Name`~Department, length) #dcast makes the presence-absence table (1-present, 0-absent)

for(i in 1:nrow(table)) { #not considering multiple appointments within a department, so changing anything greater than 1 appointment to just 1
  for(j in 2:ncol(table)){
    if(table[i,j] > 1) {
      table[i,j] <- 1
    }
  }
}

table <- cbind(table, Total = rowSums(table[ ,2:48])) #summing up each row (faculty member) to see how many departments they are appointment at

#bar plot showing faculty department appointments
ggplot(table, aes(x = Total, fill = "pink")) + geom_bar(width = 0.8) + theme_minimal() + labs(title = "Faculty Department Appointments", x = "Number of Department Appointments", y = "Faculty Count")

multiple_appointments <- subset(table, Total != 1) #only want faculty with multiple appointments (>1)

#faculty with 2 or more appointments
ggplot(multiple_appointments, aes(x = Total, fill = "pink")) + geom_bar(width = 0.8) + theme_minimal() + labs(title = "Faculty Department Appointments", x = "Number of Department Appointments", y = "Faculty Count")

departments <- c()
  for(i in 1:nrow(multiple_appointments)) {
 # print(i)
  for(j in 2:48) {
    if(multiple_appointments[i,j] == 1) {
        departments[length(departments) +1] = colnames(multiple_appointments)[j]
      }
  }
  } #listing the departments within which the faculty are appointed at another department as well

unique(departments) #vector of all unique departments that have faculty that are appointed in at least one other department
departments #lists repetitions of departments where faculty have multiple appointments

#only unique(departments) to multiple_appointments (delete all others)
multiple_departments <- subset(multiple_appointments, select = unique(departments))

multiple_departments <- rbind(multiple_departments, Total = colSums(multiple_departments[1:63, ])) #totaling each column to see how many faculty appointments in each department are also appointed to another

transposed_multiple_departments <- data.frame(t(multiple_departments)) #transpose (switches rows to columns) lets me select the total column to use in ggplot

#want to make the row names a column so I can merge the groups
Department <- rownames(transposed_multiple_departments) #makes a vector with the rownames (department names with faculty appointed in multiple departments)

#making the rownames into a column
rownames(transposed_multiple_departments) <- NULL 
transposed_multiple_departments <- cbind(Department, transposed_multiple_departments)

transposed_multiple_departments <- subset(transposed_multiple_departments, select = c(Department, Total)) #only need the department name and total number of faculty appointments in multiple departments

## which departments tend to have faculty working in other departments as well
barplot(height = transposed_multiple_departments$Total, names = Department, las =2)  #las for label orientation


#sorting by area of study (multiple appointments only)

department_groups <- read.csv("major_categories_visualized - departments_groups.csv")
study_areas <- merge(transposed_multiple_departments, department_groups, by = 'Department')

study_areas$Group <- factor(study_areas$Group)
levels(study_areas$Group)

sum1 = 0
sum2 = 0
sum3 = 0
sum4 = 0
sum5 = 0
sum6 = 0
sum7 = 0
sum8 = 0

for(i in seq_along(study_areas$Group)) { #generates a sequence of indices
  if(study_areas$Group[i] == 'Arts') { #creating groups with the total sum of dual faculty appointments among all departments in that group
    sum1 = sum1 + study_areas$Total[i]
  }
  if(study_areas$Group[i] == 'Humanities') {
    sum2 = sum2 + study_areas$Total[i]
  }
  if(study_areas$Group[i] == 'Physical / Life Science') {
    sum3 = sum3 + study_areas$Total[i]
  }
  if(study_areas$Group[i] == 'Social Sciences') {
    sum4 = sum4 + study_areas$Total[i]
  }
  if(study_areas$Group[i] == 'International Languages') {
    sum5 = sum5 + study_areas$Total[i]
  }
  if(study_areas$Group[i] == 'Mathematical Sciences') {
    sum6 = sum6 + study_areas$Total[i]
  }
  if(study_areas$Group[i] == 'Administrative') {
    sum7 = sum7 + study_areas$Total[i]
  }
  if(study_areas$Group[i] == 'Interdisciplinary') {
    sum8 = sum8 + study_areas$Total[i]
  }
}

Sum <- data.frame(Group = c("Arts", "Humanities", "Physical / Life Science", "Social Sciences", "International Languages", "Mathematical Sciences", "Administrative", "Interdisciplinary"), Total = c(13, 69, 7, 18, 10, 2, 6, 2))
study_areas <- merge(study_areas, Sum, by = 'Group')

#sum of all dual faculty appointments per study area
ggplot(study_areas, aes(x = Group, y = Total.y, fill = Group)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(title = "Multi-Department Appointments per Area of Study", x = "Area of Study", y = "Appointments") + coord_flip()

#look at each of the faculty in a department and find which other departments they are part of

#iterate through each row and each column
#if it equals 1 within the row, print the column names side by side

multiple_departments[multiple_departments == 1]

matches = list()
for(i in 1:63) {
  matches[[i]] <- vector("list", ncol(multiple_departments))
  for(j in 1:ncol(multiple_departments)) {
    if(multiple_departments[i,j] == 1) {
      matches[[i]][[j]] <- colnames(multiple_departments)[j]
    }
  }
  matches[[i]] <- matches[[i]][sapply(matches[[i]], function(x) !is.null(x))]
}

first <- sapply(matches[1:63], `[`, 1) #makes it a vector instead of list
first <- unlist(first, recursive = TRUE, use.names = TRUE) #
first <- factor(first)

second <- sapply(matches[1:63], `[`, 2)
second <- unlist(second, recursive = TRUE, use.names = TRUE)
second <- factor(second)

department_match <- data.frame(first, second)
#number of dual faculty appointments per department
ggplot(department_match, aes(x = first)) + geom_bar(fill = "#CC79A7") + theme_minimal() + labs(title = "Number of Dual Department Appointments per Department", x = "Department Name", y = "Appointments") + coord_flip()

department_group_first <- merge(department_match, department_groups, by.x = 'first', by.y = 'Department', match = 'second')
palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#301934")
ggplot(department_group_first, aes(x = first, fill = Group)) + geom_bar() + scale_fill_manual(values = palette) + theme_minimal() + labs(title = "Number of Dual Department Appointments per Department", x = "Department Name", y = "Appointments") + coord_flip()

department_group_match <- merge(department_group_first, department_groups, by.x = 'second', by.y = 'Department', match = 'first')

colnames(department_group_match) <- c("Second_Department", "First_Department", "First_Group", "Second_Group")
department_group_match <- department_group_match[ ,c('First_Department', 'First_Group', 'Second_Department', 'Second_Group')]

library(dplyr)
proportions_study <- department_group_match %>%
  group_by(First_Group, Second_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

normalized_proportions_study <- proportions_study %>%
  group_by(First_Group) %>%
  mutate(Normalized.Proportion = Proportion / sum(Proportion))

#proportion-based overlap of second study area over first
palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#301934")
ggplot(normalized_proportions_study, aes(x = Normalized.Proportion, y = First_Group, fill = Second_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette, name = "Second Area") + 
  labs(title = "Proportions of each Second Department Group according to First Department Group of Faculty",
       x = "Proportion",
       y = "First Area of Study") +
  theme_minimal()
