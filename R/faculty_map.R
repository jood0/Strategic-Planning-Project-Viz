#GOAL: clean and join data from the College of L&S about their faculty appointments to make data for a map of their reach on campus.


# add columns to the campus map data:
#   - reached within College of L&S
#   - reached from connections with other colleges --> maybe just a column for which college... unless we want to show if a space has multiple college affiliations... is that possible on campus? Probably.

# SETUP -------------------------------------------------------------------

# Libraries
library("readxl")
library("sf")



# Set Working Directory
setwd("2024_l&s_strategic_plan\\data")

# Load Data
all_appointments<-read_excel(
  path = "Copy of Joint faculty appointments.xlsx",
  sheet = 2)

space <- read.csv("space.csv", header = TRUE, stringsAsFactors = FALSE)
# NOTE: the Division indicates the college or other high level unit on campus


buildings <- st_read("./UCDavis_Facilities_Buildings/Footprint.shp")

department_homes <-read.csv("department_homes.csv", header = TRUE, stringsAsFactors = FALSE)




# DATA CLEANING -----------------------------------------------------------

# remove spaces from appointment column names
names(all_appointments) <- c("Department", "Employee", "Job_Code", "Job_Description", "FTE")

# filter appointments to just faculty
faculty_appointments <- all_appointments[grep(
  pattern = "PROF",
  x = all_appointments$Job_Description
),]

faculty_departments <- as.data.frame(unique(faculty_appointments$Department))
names(faculty_departments)<-c("Department")
faculty_departments$Lower_Department <- tolower(faculty_departments$Department)



# get a list of the departments by college
dep_college <- space[,26:25]
dep_college <- dep_college[!duplicated(dep_college[c(1:2)]),]
dep_college <- dep_college[grep("College",dep_college$Division),]
names(dep_college)[2]<- "College"

dep_college$College[which(dep_college$College == "AAES - Agricultural and Environmental Sciences, College of")]<- "Agriculture and Environmental Sciences"
dep_college$College[which(dep_college$College == "CLAS - Letters and Science, College of")]<- "Letters and Sciences"
dep_college$College[which(dep_college$College == "2400 - Engineering, College of")]<- "Engineering"
dep_college$College[which(dep_college$College == "BIOS - Biological Sciences, College of")]<- "Biological Sciences"

dep_college$Lower_Department <- tolower(dep_college$Department)


# ??? How do we match on Department name? 
# Standardizing the text

# !!!!!!!! Some of the department names in the space list/dep_college have \t\n in front of them, making them not match.

faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "african american african stds")] <- "african american and african studies"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "art & art history")] <- "art history"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "arts administrative group")] <- "art history"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "asian american")] <- "asian american studies"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "center for mind & brain")] <- "center for mind and brain"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "chemistry")] <- "chemistry, m&ps"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "chicano studies")] <- "chicana/chicano studies"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "cinema & digital media")] <- "cinema and digital media"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "department of design")] <- "design"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "earth and planetary sciences")] <- "earth & planetary sciences"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "east asian lang. & cultures")] <- "east asian languages and cultures"


faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "french & italian")] <- "french and italian"


faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "l&s dean - social sciences")] <- "dean's office, l&s"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "middle east/south asia program")] <- "middle eastern/south asia studies"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "science & technology studies")] <- "science and technology studies"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "social sciences program")] <- "other"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "spanish & portuguese")] <- "spanish and portuguese"
faculty_departments$Lower_Department[which(faculty_departments$Lower_Department == "gendersexuality womensstudies")] <- "gender, sexuality and women's studies"



# JOIN DATA ---------------------------------------------------------------


# add college (from dep_college table) to the faculty_appointments table

faculty_department_college_all <- merge(
  x = faculty_departments, 
  y = dep_college,
  by = "Lower_Department",
  all.x = TRUE)

faculty_department_college <- faculty_department_college_all[, c(3, 4)]

names(faculty_department_college)[1] <- "Department"


# !!!!! Faculty appointments are all in the College of L&S - Is that because we filtered that? No.
# !!!!! This map will just be a map of space allocation.
# !!!!! Would this be more interesting for their graduate students? The data we have is just for L&S appointments.


# use faculty_appointments table to match which buildings are reached within and outside of College of L&S

faculty_building_all <- merge(
  x = faculty_department_college,
  y = space,
  by.x = "Department",
  by.y = "Department"
)

faculty_building <- faculty_building_all[, c(1:2, 8)]

faculty_building$LS_Faculty <- 1



space_faculty <- merge(
  x = faculty_building,
  y = buildings,
  by.x = "Building.Name",
  #by.y = "FullBldgNa",
  by.y = "ShortBldgN",   #the campus space data seems to use the building short names
  all.y = TRUE
)

space_faculty <- space_faculty[which(!duplicated(space_faculty)),]

# Generate Lines for Multiple Appointment Faculty -----------------------------

# which faculty have multiple appointments?
appointment_counts <- as.data.frame(table(faculty_appointments$Employee), stringsAsFactors = FALSE)

appointment_counts <- appointment_counts[which(appointment_counts$Freq > 1),]

names(appointment_counts) <- c("name", "count")

multi_appointments <- as.data.frame(matrix(ncol=3))

for (i in 1:length(appointment_counts$name)){
  depts <- faculty_appointments$Department[which(faculty_appointments$Employee == appointment_counts$name[i])]
  
  if (length(depts)==2){
    new.row <- c(appointment_counts[i, 1], depts)
    multi_appointments <- rbind(multi_appointments, new.row)
    #print(new.row)
  
    }else{ #one faculty member has 3 departments
      #print(depts)
      
      new.row1 <- c(appointment_counts[i, 1], depts[1], depts[2])
      new.row2 <- c(appointment_counts[i, 1], depts[2], depts[3])
      new.row3 <- c(appointment_counts[i, 1], depts[3], depts[1])
      
      multi_appointments <- rbind(multi_appointments, new.row1, new.row2, new.row3)
    
    
    
  } #end of if/else
} #end of for-loop

multi_appointments <- multi_appointments[which(multi_appointments$V2 != multi_appointments$V3),]
names(multi_appointments) <- c("name", "from_original", "to_original")

# join the from and to columns with faculty_department_college_all - Department.x and Department.y - table to get the corrected department names

multi_appointments <- merge(
  x = multi_appointments,
  y = faculty_department_college_all,
  by.x = "from_original",
  by.y = "Department.x" #the original all caps department name
)

multi_appointments <- multi_appointments[, c(2, 1, 3, 5)]

names(multi_appointments)[4] <- "from_department" 

multi_appointments <- merge(
  x = multi_appointments,
  y = faculty_department_college_all,
  by.x = "to_original",
  by.y = "Department.x"
)

multi_appointments <- multi_appointments[, c(2, 3, 1, 4, 6)]

names(multi_appointments)[5] <- "to_department" 

# add the building names base on the department

dep_building <- unique(faculty_building[, c(1,3)])


# join the building centroids to the corrected department names for the from and to columns

#   make centroids for the buildings

building_centroids <- st_centroid(buildings)

building_centroids <- building_centroids[,c(6,42)] #5 is FullBldgNa; 6 is ShortBldgN


# add the buildings to the dataset

points_appointments <- merge(
  x = multi_appointments,
  y = department_homes,
  by.x = "from_department",
  by.y = "department"
)

names(points_appointments)[6] <- "from_building"

points_appointments <- merge(
  x = points_appointments,
  y = department_homes,
  by.x = "to_department",
  by.y = "department"
)

names(points_appointments)[7] <- "to_building"

# add the centroid points to the dataframe

points_appointments <- merge(
  x = points_appointments,
  y = building_centroids,
  by.x = "from_building",
  by.y = "ShortBldgN"
)

names(points_appointments)[8] <- "from_point"  

points_appointments <- merge(
  x = points_appointments,
  y = building_centroids,
  by.x = "to_building",
  by.y = "ShortBldgN"
)

names(points_appointments)[9] <- "to_point"

# turn the from and to points into lines
# https://www.adventuremeng.com/post/create-sf-lines-from-start-and-end-coordinates/

# st_linestring requires a matrix
# for each pair of departments, we need from on line 1 and to on line 2
# Loop through df --> linestring(rbind(coords(from), coords(to))) ???

#line_geom <-c()

line_appointments <- data.frame()

for(i in 1:dim(points_appointments)[1]){
  print(i)
  from_to <- as.data.frame(rbind(st_coordinates(points_appointments$from_point[i]), st_coordinates(points_appointments$to_point[i])))
  # make a spatial data frame
  sf_object <- st_as_sf(from_to, coords = c("X", "Y"), crs = 3857)
  # create linestring
  linestring <- st_cast(st_combine(sf_object), "LINESTRING")
  # construct a one line data frame to contain the new row of data to add
  row.to.add <- st_as_sf(data.frame(points_appointments[i, 1:5], linestring))
  # append the new row to the existing dataframe
  line_appointments <- rbind(line_appointments, row.to.add)
}

line_appointments<-line_appointments[which(st_is_valid(line_appointments)),]

# points with counts for node size map

from_depts <- points_appointments[, c(2, 4, 8)]
names(from_depts)<-c("building", "department", "geometry")
to_depts <- points_appointments[, c(1, 3, 9)]
names(to_depts)<-c("building", "department", "geometry")

dept_points <- as.data.frame(rbind(from_depts, to_depts))

building_counts <- as.data.frame(table(dept_points$building))
names(building_counts)<-c("building", "count")

building_counts <- merge(
  x = building_counts,
  y = dept_points[, c(1, 3)],
  by= "building",
  all.x = TRUE
)

building_counts <- st_as_sf(building_counts[!duplicated(building_counts),], crs = 3857)


# Write Data to Disk ------------------------------------------------------



st_write(space_faculty, layer="buildings", "buildings.gpkg", delete_layer = TRUE)
st_write(obj=line_appointments, layer="line_appointments", "buildings.gpkg", append=TRUE)
st_write(obj=building_counts, layer="building_counts", "buildings.gpkg", append=TRUE)



