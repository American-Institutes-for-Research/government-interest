library(trend)
library (ggplot2)
library(plyr)
library(scales)
library(MASS)
library(reshape)
library(tools)
library(car)
library(dplyr)

setwd("F:\\Govt_Int\\Final_CSVS") 
script_v <- "0.6"

# load patent level data
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)



# load assignee level data
in.assignee_level <- read.csv("assignee_type.csv", header = TRUE, stringsAsFactors = FALSE)
in.assignee_level$type_clean <- recode(in.assignee_level$assignee_type, "c(12, 2)='US Company';
 c(13, 3)='Foreign Company';c(16, 6)='US Government';else='Other'")

#load assignee level data with the dictionary looked up categories
in.assignee_level_types <- read.csv("assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = TRUE)
head(in.assignee_level_types)


in.assignee_level %>% 
  group_by(type_clean) %>%
  summarise(no_rows = length(type_clean))


#get only the patents with the correct year count
assignees_years <- left_join(in.assignee_level, in.patent_level,
                             by = 'patent_id')
assignees_years <- subset(assignees_years, year >= 1980 & year <= 2015)

grouped_assignees <- assignees_years %>% group_by(year, type_clean) %>% summarise(type_count = length(type_clean))
total_count_assignees <- assignees_years %>% group_by(year) %>% summarise(total = length(year))
for_graph <- left_join(grouped_assignees, total_count_assignees, by = 'year')
for_graph <- mutate(for_graph, PercentageofAssignees = type_count/total)
head(for_graph)

#repeat the same for the looked up types
assignees_years_types <- left_join(in.assignee_level_types, in.patent_level,
                             by = 'patent_id')
assignees_years_types <- subset(assignees_years_types, year >= 1980 & year <= 2015)

head(assignees_years_types)

grouped_assignees_types <- assignees_years_types %>% group_by(year, thes_types) %>% summarise(type_count = length(thes_types))
total_count_assignees_types <- assignees_years_types %>% group_by(year) %>% summarise(total = length(year))
for_graph_types <- left_join(grouped_assignees_types, total_count_assignees_types, by = 'year')
for_graph_types <- mutate(for_graph_types, PercentageofAssignees = type_count/total)
head(for_graph_types)



#make graph
graph1 <- ggplot(for_graph, aes(x=year,y=PercentageofAssignees,group=type_clean,fill=type_clean)) +
  geom_area(position="fill") + labs(y= "Percentage of Assignees", x="Year", fill = "Type of Assignee") +
  ggtitle("Trends over Time in Percentage of Government Interest \nPatents Assigned to Different Categories")
graph1
ggsave (paste0("out\\assignee_types\\gi_patent_assignees_over_time.png"), device = "png")

#make graph with looked up types
graph2 <- ggplot(for_graph_types, aes(x=year,y=PercentageofAssignees,group=thes_types,fill=thes_types)) +
  geom_area(position="fill") + labs(y= "Percentage of Assignees", x="Year", fill = "Type of Assignee") +
  ggtitle("Trends over Time in Percentage of Government Interest \nPatents Assigned to Different Categories")
graph2
ggsave (paste0("out\\assignee_types\\looked_up_gi_patent_assignees_over_time.png"), device = "png")



#Government to Private connections

in.connection_level <- read.csv("GI_connection_to_private_industry.csv", header = TRUE, stringsAsFactors = FALSE)
head(in.connection_level)
connection_counts <- in.connection_level %>% group_by(degrees) %>% summarise(Number_of_Patents = length(degrees))
connection_counts
graph3 <- (ggplot(connection_counts, aes(degrees, Number_of_Patents)) +
                 +geom_bar(stat = "identity", fill = 'blue') + ggtitle("Degrees Separating GOvernment Interest Patents From Private Sector Patents"))
graph3
