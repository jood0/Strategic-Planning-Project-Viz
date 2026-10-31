library(dplyr) #mutate
library(ggplot2) #plot
library(tidyr) #pivot_longer

# cleaning and selecting data
minor18 = read.csv("ampus Minors.xlsx - 18-19 Data.csv")
minor22 = read.csv("ampus Minors.xlsx - 22-23 Data.csv")

minor18 <- minor18[ -c(4:8, 10:12) ]
minor22 <- minor22[ -c(4:8) ]

#removing extra rows
minor18 <- minor18[-nrow(minor18), ]
minor18 <- minor18[-172, ]
minor22 <- minor22[1:(nrow(minor22) - 2), ]

bothY <- merge(minor18, minor22, by = c("Minor", "Code", "College"), all = TRUE) 
bothY <- subset(bothY, !(X18.19 == 0 & X22.23.Total == 0)) #remove minors with 0 count both years

# correct the college of Accounting (CPAC) and Technology Management (STMG) to be the same college (Graduate School of Management)
# and make Education (CEDU) (School of Education) and Public Health Sciences (CSPH)  (School of Medicine) be "others" (remaining c)

bothY <- bothY %>% 
  mutate(College = if_else(Code == "CPAC", "M", College),
         College = if_else(Code == "STMG", "M", College))






# Is a given minor increasing over time?

# enrolment in minor relative to students 
bothY <- bothY %>% 
  mutate(enrolment_18=(X18.19/29431)*100,
         enrolment_22=(X22.23.Total/30360)*100) 

# is enrolment increasing in a minor, and how much increasing/decreasing 
bothY <- bothY %>% 
  mutate(increasing = ifelse(enrolment_22 > enrolment_18, TRUE, FALSE),
         change=enrolment_22-enrolment_18)


#plot
ggplot(bothY) + aes(x = reorder(Code, change), y = change, fill = increasing) +
  geom_col() + scale_fill_manual(values = c("#022851", "#FFBF00")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  labs(x = "Minor", y = "Change in Enrollment", fill = "Increasing Enrollment") +
  ggtitle("Change in Enrollment for Minors between 18-19 and 22-23")





#By year, is enrollment in minors increasing over time?

#enrolment each year by college

by_college <- bothY %>%
  group_by(College) %>%
  summarise(`Sum18` = sum(X18.19),
            `Sum22` = sum(X22.23.Total))


by_college <- by_college %>%
  mutate(College = c("AES", "BIO", "Others", "ENG", "LS", "MGT"))


# enrolment in minor relative to students 
by_college <- by_college %>% 
  mutate(col_by_totStudents18=(Sum18/29431)*100,
         col_by_totStudents22=(Sum22/30360)*100)

#reshape data
data_plot <- by_college %>%
  pivot_longer(cols = starts_with("col_by_totStudents"),
               names_to = "Year",
               values_to = "Proportion") %>%
  mutate(Year = gsub("col_by_totStudents", "", Year))

# Plotting
ggplot(data = data_plot, aes(x = Year, y = Proportion, fill = College)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("#022851", "#B3C1D1","#6FCFEB", "#FFBF00", "#009E73", "#0072B2")) +
  labs(title = "Students Enrolled in Minors by College in Year 2018-2019 and 2022-2023",
       x = "Year",
       y = "Students Enrolled to Total Students %",
       fill = "College") +
  theme_minimal() 
write.csv(bothY, "minor_growth.csv")






# top 3 most Increasing/Descreasing majors by College

major_change <- bothY %>%
  group_by(College, Minor) %>%
  summarise(total_change = sum(change))

top_growing <- major_change %>%
  arrange(College, desc(total_change)) %>%
  group_by(College) %>%
  top_n(3)

top_shrinking <- major_change %>%
  arrange(College, total_change) %>%
  group_by(College) %>%
  slice_head(n = 3)

top_shrink_grow <- bind_rows(top_shrinking, top_growing)
top_shrink_grow_unique <- distinct(top_shrink_grow)


ggplot(top_shrink_grow_unique, aes(x = reorder(Minor, total_change), y = total_change, fill = College)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("#022851", "#B3C1D1","#0072B2", "#6FCFEB", "#FFBF00", "#009E73"),labels = c("AES", "BIO", "Others", "ENG", "LS", "MGT"))+
  labs(y = "Total Change in Enrollment", x = "Minor", fill = "College") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top 3 Increasing and Decreasing Minors by College")
