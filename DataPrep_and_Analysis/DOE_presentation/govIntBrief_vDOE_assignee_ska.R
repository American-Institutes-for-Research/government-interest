library(trend)
library (ggplot2)
library(plyr)
library(scales)
library(MASS)
library(reshape)
library(tools)
library(dplyr)

setwd("F:\\Govt_Int\\Final_CSVS") 
script_v <- "2.0"

# set color scheme
cyan <- rgb (0, 123, 188,	maxColorValue = 255)
darkRed <- rgb (208, 32, 47, maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (91, 140, 41, maxColorValue=255)
darkPurple <- rgb (135, 30, 110, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (152, 152, 152, maxColorValue=255)

# load patent data
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.patent_level <- in.patent_level[in.patent_level$year != "NULL",]
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)

#load gov level funders
in.gov_level <- read.csv("government_interest_level.csv", header = TRUE, stringsAsFactors = FALSE)
# add patents without organizations as US government to in.gov_level (fixing a data problem here)
set.diff <- setdiff(sort(in.patent_level$patent_id), unique(sort(in.gov_level$patent_id)))
length(set.diff) # 2,511
new.rows <- cbind (set.diff, rep("United States Government"), rep("United States Government"), rep("NULL"), rep("NULL"))
# if you look like at these government interest patents, there are some full text fields which have organizations in there; not fixing here

# load government interest data 
View(in.gov_level[1:100,])
colnames(in.gov_level)
in.gov_level.short <- in.gov_level
colnames(new.rows) <- colnames(in.gov_level)
in.gov_level <- rbind(in.gov_level, new.rows)
in.gov_level$patent_id <- as.character(in.gov_level$patent_id)
nrow(in.gov_level)
length(unique(in.gov_level$patent_id)) 

# remove non-DEO patents
doe.patent_ids <- unique(in.gov_level[in.gov_level$level_one == "Department of Energy", 1])
length(doe.patent_ids) # 22,925 

in.patent_level.b4doe <- in.patent_level
in.gov_level.b4doe <- in.gov_level

in.patent_level <- in.patent_level[which(in.patent_level$patent_id %in% doe.patent_ids), ]
in.gov_level <- in.gov_level[which(in.gov_level$patent_id %in% in.patent_level$patent_id), ]

length(in.patent_level$patent_id) # 22,925
length(unique(in.gov_level$patent_id))

# load assignee level data
in.assignee_level <- read.csv("assignee_type.csv", header = TRUE, stringsAsFactors = FALSE)

#load assignee level data with the dictionary looked up categories
in.assignee_level_types <- read.csv("assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = FALSE)
head(in.assignee_level_types)


#repeat the same for the looked up types
assignees_years_types <- left_join(in.assignee_level_types, in.patent_level,
                             by = 'patent_id')
assignees_years_types <- subset(assignees_years_types, year >= 1980 & year <= 2017)


assignees_years_types[which(assignees_years_types$thes_types == 'Academic'), 5] <- "Academic or Hospital"
assignees_years_types[which(assignees_years_types$thes_types == 'Hospital'), 5] <- "Academic or Hospital"
assignees_years_types[which(assignees_years_types$thes_types == 'Ambiguous'),  5] <- "Corporate"
assignees_years_types[which(assignees_years_types$thes_types == 'Person'), 5] <- "Other"

colnames (assignees_years_types)
head (assignees_years_types)

assignees_years_types.short <- assignees_years_types[,c(2,5,10)]
head (assignees_years_types.short)
assignees_years_types.ratio <- ddply (assignees_years_types.short, .(patent_id), transform, weight= (1/length(patent_id))) # needed because patents have multiple assignees which belong to select sectors we care about

grouped_assignees_types <- assignees_years_types.ratio %>% group_by(year, thes_types) %>% summarise(type_count = sum(weight))
total_count_assignees_types <- assignees_years_types.ratio %>% group_by(year) %>% summarise(total = sum(weight))
for_graph_types <- left_join(grouped_assignees_types, total_count_assignees_types, by = 'year')
for_graph_types <- mutate(for_graph_types, PercentageofAssignees = type_count/total)

head(for_graph_types)

unique(for_graph_types$thes_types)

for_graph_types <- for_graph_types[which(for_graph_types$thes_types != "Other"), ]


#make graph with looked up types
graph2 <- ggplot(for_graph_types, aes(x=year,y=PercentageofAssignees,group=thes_types,linetype=thes_types, colour=thes_types)) +
  geom_line(size=1.5) + labs(y= "Percentage of Patents", x="Year") +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_colour_manual(values=c(darkPurple, cyan, darkGreen, darkGrey)) +
  scale_linetype_manual(values=c("dashed", "twodash", "solid")) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::percent) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  ) 

graph2
ggsave (paste0("out\\doe\\looked_up_gi_patent_assignees_over_time", script_v, ".png"), device = "png")
write.csv(for_graph_types, "out\\doe\\sectors_over_time.csv")

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

