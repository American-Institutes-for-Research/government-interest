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
script_v <- "1.5"

cyan <- rgb (0, 123, 188,	maxColorValue = 255)
darkRed <- rgb (208, 32, 47, maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (91, 140, 41, maxColorValue=255)
darkPurple <- rgb (135, 30, 110, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (206, 206, 199, maxColorValue=255)

# load patent level data
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)

# need to re-org this into main script 
in.patent_level.gi <- in.patent_level.merged[in.patent_level.merged$avg_gi > 0, ]

# load assignee level data
in.assignee_level <- read.csv("assignee_type.csv", header = TRUE, stringsAsFactors = FALSE)
in.assignee_level$type_clean <- recode(in.assignee_level$assignee_type, "c(12, 2)='US Company';
 c(13, 3)='Foreign Company';c(16, 6)='US Government';else='Other'")

#load assignee level data with the dictionary looked up categories
in.assignee_level_types <- read.csv("assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = FALSE)
head(in.assignee_level_types)


in.assignee_level %>% 
  group_by(type_clean) %>%
  summarise(no_rows = length(type_clean))


#get only the patents with the correct year count
assignees_years <- left_join(in.assignee_level, in.patent_level,
                             by = 'patent_id')
assignees_years <- subset(assignees_years, year >= 1980 & year <= 2016)

grouped_assignees <- assignees_years %>% group_by(year, type_clean) %>% summarise(type_count = length(type_clean))
total_count_assignees <- assignees_years %>% group_by(year) %>% summarise(total = length(year))
for_graph <- left_join(grouped_assignees, total_count_assignees, by = 'year')
for_graph <- mutate(for_graph, PercentageofAssignees = type_count/total)
head(for_graph)

#repeat the same for the looked up types
assignees_years_types <- left_join(in.assignee_level_types, in.patent_level.gi,
                             by = 'patent_id')
assignees_years_types <- subset(assignees_years_types, year >= 1980 & year <= 2016)

assignees_years_types[which(assignees_years_types$thes_types == 'Hospital'), 4] <- "Academic"
assignees_years_types[which(assignees_years_types$thes_types == 'Ambiguous' | assignees_years_types$thes_types == 'Person'), 4] <- "Other"


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
graph2 <- ggplot(for_graph_types, aes(x=year,y=PercentageofAssignees,group=thes_types,linetype=thes_types, colour=thes_types)) +
  geom_line(size=1.5) + labs(y= "Percentage of Assignees", x="Year") +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_colour_manual(values=c(darkPurple, cyan, darkGreen, darkGrey), labels=c("Academic", "Corporate", "Government", "Other")) +
  scale_linetype_manual(values=c("dashed", "twodash", "solid", "dotted"), labels=c("Academic", "Corporate", "Government", "Other")) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  ) 

graph2
ggsave (paste0("out\\looked_up_gi_patent_assignees_over_time.png"), device = "png")


g.12 <- ggplot(melt.ind) + 
  geom_line(aes(x = year, y = value, colour=variable, linetype=variable), size=1.5) + 
  ylab(label="Indexed value") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkPurple, darkGreen), labels=c( "Government interest patents", "All patents", "Federal R&D funding")) +
  scale_linetype_manual(values=c("solid", "dashed", "twodash"), labels=c( "Government interest patents", "All patents", "Federal R&D funding")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottoms',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  ) 

g.19a <- ggplot(count.new_entrants.small, aes(x = year, y = freq, colour=type, group = type, linetype=type)) + 
  geom_line(size=1.5) + 
  ylab(label="Number of Patent Entrants Receiving Government Funding") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_colour_manual(values=c(darkPurple, darkBlue, cyan), labels=c("Academic", "Large firm", "Small firm")) +
  scale_linetype_manual(values=c("dotted", "twodash", "solid"), labels=c("Academic", "Large firm", "Small firm")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.19a


#Government to Private connections

in.connection_level <- read.csv("GI_connection_to_private_industry.csv", header = TRUE, stringsAsFactors = FALSE)
head(in.connection_level)
connection_counts <- in.connection_level %>% group_by(degrees) %>% summarise(Number_of_Patents = length(degrees))
connection_counts
graph3 <- (ggplot(connection_counts, aes(degrees, Number_of_Patents)) 
                 +geom_bar(stat = "identity", fill = 'blue') + ggtitle("Degrees Separating Government Interest Patents From Private Sector Patents"))
graph3

ggsave (paste0("out\\degrees_of_separation.png"), device = "png")

#government to firm connections

distance <- read.csv("assignee_distance_by_type.csv", header = TRUE, stringsAsFactors = FALSE)
head(distance)

