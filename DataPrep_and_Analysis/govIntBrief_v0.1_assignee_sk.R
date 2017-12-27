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

in.assignee_level %>% 
  group_by(type_clean) %>%
  summarise(no_rows = length(type_clean))

assignees_years <- left_join(in.assignee_level, in.patent_level,
                             by = 'patent_id')
assignees_years <- subset(assignees_years, year >= 1980 & year <= 2015)

grouped_assignees <- assignees_years %>% group_by(year, type_clean) %>% summarise(type_count = length(type_clean))
total_count_assignees <- assignees_years %>% group_by(year) %>% summarise(total = length(year))
for_graph <- left_join(grouped_assignees, total_count_assignees, by = 'year')
for_graph <- mutate(for_graph, PercentageofAssignees = type_count/total)
head(for_graph)



graph1 <- ggplot(for_graph, aes(x=year,y=PercentageofAssignees,group=type_clean,fill=type_clean)) +
  geom_area(position="fill") + labs(y= "Percentage of Assignees", x="Year", fill = "Type of Assignee") +
  ggtitle("Trends over Time in Percentage of Government Interest \nPatents Assigned to Different Categories")
graph1
ggsave (paste0("out\\assignee_types\\gi_patent_assignees_over_time.png"), device = "png")

head(total_count_assignees)

head(grouped_assignees)
