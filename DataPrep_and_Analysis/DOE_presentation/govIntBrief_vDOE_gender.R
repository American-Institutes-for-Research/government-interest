#imports
library(trend)
library (ggplot2)
library(plyr)
library(scales)
library(MASS)
library(reshape)
library(tools)
library(car)
library(plotly)
library(rjson) 
library(data.table)
library(gridExtra)
library(psych)
library(dplyr)
library(extrafont)

setwd("F:\\Govt_Int\\Final_CSVS") 
loadfonts(device = "win") #load the extra fonts for graphing
script_v <- "2.0"

# set color scheme
cyan <- rgb (0, 123, 188,	maxColorValue = 255)
darkRed <- rgb (208, 32, 47, maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (91, 140, 41, maxColorValue=255)
darkPurple <- rgb (135, 30, 110, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (152, 152, 152, maxColorValue=255)


############################
#load helper data
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.patent_level <- in.patent_level[in.patent_level$year != "NULL",]
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)
in.patent_level <- subset(in.patent_level, year >= 1980 & year <= 2016)
nrow(in.patent_level) # 117,970

#load inventor gender
in.inventor_has_fem <- read.csv('wipo_gender/gi_has_female_inventor_v2-ska.csv', header = TRUE, stringsAsFactors = FALSE)

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

length(in.patent_level$patent_id) #21,479 in period
length(unique(in.gov_level$patent_id))

#join with helper data
gi_inventor_has_fem <- merge(in.inventor_has_fem, in.patent_level,
                                 by = 'patent_id')
head(in.inventor_has_fem)
nrow(gi_inventor_has_fem) #21,226
View(gi_inventor_has_fem)


#drop unknown gender and years outside range
gi_inventor_has_fem.sub <- subset(gi_inventor_has_fem, year >= 1980 & year <= 2016)
gi_inventor_has_fem.sub <- subset(gi_inventor_has_fem.sub, has_fem_inv==0|has_fem_inv==1)
gi_inventor_has_fem.sub$has_fem_inv <- as.numeric(gi_inventor_has_fem.sub$has_fem_inv)
nrow(gi_inventor_has_fem.sub)
head(gi_inventor_has_fem.sub)


#################################################
#make the counts of teams with at least one woman

#organize the data to get the percentage of patents in each year with atleast one woman
grouped_gi_inventor_has_fem <- gi_inventor_has_fem.sub %>% group_by(year, has_fem_inv) %>% summarise(count_inv = length(has_fem_inv))
head(grouped_gi_inventor_has_fem)
total_count_gi_inventor_has_fem <- gi_inventor_has_fem %>% group_by(year) %>% summarise(total = length(year))
head(total_count_gi_inventor_has_fem)
head(grouped_gi_inventor_has_fem)
for_graph <- merge(grouped_gi_inventor_has_fem, total_count_gi_inventor_has_fem, by = 'year')
for_graph <- mutate(for_graph, PercentageofPatents = count_inv/total)
for_graph$has_fem_inv <- as.character(for_graph$has_fem_inv)

head(for_graph)

graph1 <- ggplot(for_graph) +
  geom_line(aes(x=year,y=PercentageofPatents,color=has_fem_inv,linetype=has_fem_inv), size = 1.5) +
  ylab(label="Percentage of DoE GI Patents") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkPurple, darkGreen), labels=c("All Male Inventors", "At Least One Woman",  "All Unknown")) +
  scale_linetype_manual(values=c("solid", "dashed", "twodash"), labels=c("All Male Inventors",  "At Least One Woman", "All Unknown")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y = 0) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  )
graph1
ggsave (paste0("out\\doe\\gi_teams_with_female_inventors_over_time.png"), device = "png")
write.csv (for_graph, file="out\\doe\\teams_with_at_least_one_woman.csv")



#################################################
# show growth of women by team size

#organize the data to get the percentage of patents in each year with atleast one woman
gi_inventor_has_fem$num_inventors_cat <-cut(gi_inventor_has_fem$num_inventors, c(1:15), labels = c(as.character(1:14)))

grouped_gi_inventor_has_fem <- gi_inventor_has_fem %>% group_by(year, num_inventors_cat) %>% summarise(count_inv = n(), count_fem = sum(has_fem_inv), pct_fem = sum(has_fem_inv)/n())

graph2 <- ggplot(grouped_gi_inventor_has_fem) +
  geom_line(aes(x=year,y=pct_fem,color=num_inventors_cat,linetype=num_inventors_cat), size = 1.5) +
  ylab(label="Percent of Teams with At Least One Female Inventor") +  xlab("Year") +
  # scale_colour_manual("Inventor Team Size", values=c(cyan, darkPurple, darkGreen, darkGrey))+
  # scale_linetype_manual("Inventor Team Size", values=c("solid", "dashed", "twodash", "dotted")) +
  # scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_y_continuous(labels = scales::percent) + 
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.title.align = 0.5,
        legend.key.width = unit(3, 'lines'),
        text=element_text(size=16,  family="Cambria"),
        legend.box.background = element_rect(colour = lightGrey), 
        legend.background = element_blank()
  ) +
  guides(colour = guide_legend(title.position = "bottom")) 
graph2
ggsave (paste0("out\\line_teams_with_fem_inv_over_time_by_teamsize.png"), device = "png")


##################################################
# percent of teams with at least one woman by team size
# AMyers: - I want to reformat this one as a scatter plot team size (number of inventors) on the x-axis, 
# average women's share of the inventor team (mean proportion of the team that is female for each team size) 
# and size the markers based on the number of patents. Make sense?

in.gender_wipo <- read.csv("wipo_gender\\gi_wipo_gender_v2-ska.csv", header = TRUE, stringsAsFactors = FALSE)
head(in.gender_wipo)
nrow(in.gender_wipo)
in.gender_wipo[which(in.gender_wipo$patent_id == '8518650'), ] # has one woman, and three men
in.gender_wipo[which(in.gender_wipo$patent_id == '4181900'), ] # has three men

in.gender_wipo.sub <- subset(in.gender_wipo, year >= 1980 & year <= 2016)
nrow(in.gender_wipo.sub)

in.gender_wipo.sub.b4doe <- in.gender_wipo.sub
in.gender_wipo.sub <- in.gender_wipo.sub[in.gender_wipo.sub$patent_id %in% doe.patent_ids,]
length(unique(in.gender_wipo.sub$patent_id)) # 21475

in.gender_wipo.sub <- in.gender_wipo.sub [,c(1,2,7)] # patent_id, team_size, dumale
head (in.gender_wipo.sub)
nrow(in.gender_wipo.sub) # 61,602
length(unique(in.gender_wipo.sub$patent_id)) 

in.gender_wipo.sub <- subset(in.gender_wipo.sub, dumale==0|dumale==1)
in.gender_wipo.sub$dumale <- as.numeric(in.gender_wipo.sub$dumale)
nrow(in.gender_wipo.sub) # 57,339
length(unique(in.gender_wipo.sub$patent_id)) #21,202

women_in_teams <- in.gender_wipo.sub %>% group_by(patent_id, num_inventors) %>% summarise(num_men = sum(dumale))
women_in_teams$percentWomen <- (women_in_teams$num_inventors - women_in_teams$num_men) / women_in_teams$num_inventors 
women_in_teams[which(women_in_teams$patent_id == '8518650'), ]
women_in_teams[which(women_in_teams$patent_id == '4181900'), ]
View(women_in_teams)

women_in_teams.sub <- subset(women_in_teams, percentWomen > 0)
women_in_teams.sub[which(women_in_teams.sub$patent_id == '8518650'), ] 
women_in_teams.sub[which(women_in_teams.sub$patent_id == '4181900'), ] 
grouped_by.num_inventors <- women_in_teams.sub %>% group_by(num_inventors) %>% summarise(meanPercWomenOnTeam = mean(percentWomen), numPatents = length(patent_id))
View(grouped_by.num_inventors)

for_graph.num_inventors <- grouped_by.num_inventors [grouped_by.num_inventors$num_inventors <= 15,]
colnames(for_graph.num_inventors)

graph3 <- ggplot(data=for_graph.num_inventors, aes(x=num_inventors, y=meanPercWomenOnTeam)) +
  geom_point(aes(colour = numPatents, size=numPatents)) + 
  labs(y= "Mean Percentage of Women\non Teams with At Least One Woman", x="Inventor Team Size", colour="Number of\nPatents", size="Number of\nPatents") +
  theme_set(theme_gray(base_size = 16)) + 
  scale_size_continuous(range = c(2, 16), labels = comma) +
  scale_colour_gradient(low = "blue", labels = comma) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::percent) + 
  guides(color=guide_legend(), size = guide_legend()) + 
  scale_x_continuous(breaks = rep(1:15), minor_breaks = rep(1:15)) +
  theme(
    legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.title.align = 0.5,
        legend.key.width = unit(3, 'lines'),
        text=element_text(size=16,  family="Cambria"),
        legend.box.background = element_rect(colour = lightGrey), 
        legend.background = element_blank()
  ) 
graph3
ggsave (paste0("out\\doe\\percent_with_female_inventor_by_team_size.png"), device = "png")
write.csv(for_graph.num_inventors, "out\\doe\\mean_percent_of_teams_with_a_female_inventor.csv")


##################################################
# AMyers: Can you look into which agencies supported and in which technologies the most patents with at 
# least one women inventor fall? I would like to add a little more context to the discussion of these figures. 

# wipo fields
women.patents_ids <- women_in_teams.sub$patent_id
head(in.gender_wipo)
patent_wipos.women <- unique(in.gender_wipo[in.gender_wipo$patent_id %in% women.patents_ids, c(1,5)])
nrow(patent_wipos.women)
head(patent_wipos.women)
top.wipos <- patent_wipos.women %>% group_by(wipo_field) %>% summarise(freq = n())
write.csv(top.wipos, "out\\doe\\top.wipos.women_teams.csv")

# funders
# load government interest data 
View(in.gov_level[1:100,])
colnames(in.gov_level)
in.gov_level.short <- in.gov_level
colnames(new.rows) <- colnames(in.gov_level)
in.gov_level <- rbind(in.gov_level, new.rows)
in.gov_level$patent_id <- as.character(in.gov_level$patent_id)
nrow(in.gov_level) # 155,885
length(unique(in.gov_level$patent_id)) 


# filter on gov_level ids to get the right years 
in.gov_level.bkp <- in.gov_level
in.gov_level <- in.gov_level[which(in.gov_level$patent_id %in% women.patents_ids), ]
length(unique(in.gov_level$patent_id)) # 41,081

# merge data two main data files
in.patent_level.women.merged <- merge(in.patent_level, in.gov_level, by="patent_id")
nrow(in.patent_level.women.merged) # 50,226

# weight and count
in.patent_level.women.ratio <- ddply (in.patent_level.women.merged, .(patent_id), transform, weight= (1/length(patent_id))) # weight by patent_id
women.ratio.funders <- in.patent_level.women.ratio %>% group_by(level_one) %>% summarise(weightedFreq = sum(weight) )
write.csv(women.ratio.funders, "women.ratio.funders.csv")

# test
nrow(in.patent_level)
in.patent_level.merged <- merge(in.patent_level, in.gov_level.bkp, by="patent_id")
in.patent_level.ratio <- ddply (in.patent_level.merged, .(patent_id), transform, weight= (1/length(patent_id))) # weight by patent_id
patent_ratio.funders <- in.patent_level.ratio %>% group_by(level_one) %>% summarise(weightedFreq = sum(weight) )
sum(patent_ratio.funders$weightedFreq)

patent_classes <- in.patent_level %>% group_by(wipo_field) %>% summarise(freq = n() )
sum(patent_classes$freq)
