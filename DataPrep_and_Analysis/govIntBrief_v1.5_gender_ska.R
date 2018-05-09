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

# set color scheme
cyan <- rgb (0, 123, 188,	maxColorValue = 255)
darkRed <- rgb (208, 32, 47, maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (91, 140, 41, maxColorValue=255)
darkPurple <- rgb (135, 30, 110, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (206, 206, 199, maxColorValue=255)

############################
#load helper data
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)

#load inventor gender
in.inventor_has_fem <- read.csv('wipo_gender/gi_has_female_inventor.csv', header = TRUE, stringsAsFactors = FALSE)

#join with helper data
gi_inventor_has_fem <- left_join(in.inventor_has_fem, in.patent_level,
                                 by = 'patent_id')

#drop unknown gender and years outside range
gi_inventor_has_fem <- subset(gi_inventor_has_fem, year >= 1980 & year <= 2016)
gi_inventor_has_fem <- subset(gi_inventor_has_fem, has_fem_inv==0|has_fem_inv==1)
gi_inventor_has_fem$has_fem_inv <- as.numeric(gi_inventor_has_fem$has_fem_inv)


#################################################
#make the counts of teams with at least one woman

#organize the data to get the percentage of patents in each year with atleast one woman
grouped_gi_inventor_has_fem <- gi_inventor_has_fem %>% group_by(year, has_fem_inv) %>% summarise(count_inv = length(has_fem_inv))
total_count_gi_inventor_has_fem <- gi_inventor_has_fem %>% group_by(year) %>% summarise(total = length(year))
for_graph <- left_join(grouped_gi_inventor_has_fem, total_count_gi_inventor_has_fem, by = 'year')
for_graph <- mutate(for_graph, PercentageofPatents = count_inv/total)


graph1 <- ggplot(for_graph) +
  geom_line(aes(x=year,y=PercentageofPatents,color=has_fem_inv,linetype=has_fem_inv), size = 1.5) +
  ylab(label="Percentage of Patents") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkPurple, darkGreen), labels=c("All Male Inventors", "Atleast one woman",  "All Unknown")) +
  scale_linetype_manual(values=c("solid", "dashed", "twodash"), labels=c("All Male Inventors",  "Atleast one woman", "All Unknown")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  )
graph1
ggsave (paste0("out\\gi_teams_with_female_inventors_over_time.png"), device = "png")

gi_inventor_has_fem$num_inventors_cat <-cut(gi_inventor_has_fem$num_inventors, c(0,1,2,5,100), labels = c('1', '2', '3-5', '6+'))

#################################################
# show growth of women by team size

#organize the data to get the percentage of patents in each year with atleast one woman
gi_inventor_has_fem$num_inventors_cat <-cut(gi_inventor_has_fem$num_inventors, c(0,1,2,5,100), labels = c('1', '2', '3-5', '6+'))

grouped_gi_inventor_has_fem <- gi_inventor_has_fem %>% group_by(year, num_inventors_cat) %>% summarise(count_inv = n(), count_fem = sum(has_fem_inv), pct_fem = sum(has_fem_inv)/n())

graph2 <- ggplot(grouped_gi_inventor_has_fem) +
  geom_line(aes(x=year,y=pct_fem,color=num_inventors_cat,linetype=num_inventors_cat), size = 1.5) +
  ylab(label="Percent of Teams with At Least One Female Inventor") +  xlab("Year") +
  scale_colour_manual("Inventor Team Size", values=c(cyan, darkPurple, darkGreen, darkGrey))+
  scale_linetype_manual("Inventor Team Size", values=c("solid", "dashed", "twodash", "dotted")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
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
#percent of teams with at least one woman by team size
gi_inventor_has_fem$num_inventors_clean <-gi_inventor_has_fem$num_inventors
gi_inventor_has_fem$num_inventors_clean[gi_inventor_has_fem$num_inventors_clean>15] <-15
grouped_inventors_team <- gi_inventor_has_fem %>% group_by(num_inventors_clean, has_fem_inv) %>% summarise(has_fem_inv_count = length(has_fem_inv))
total_count_inventors_team <- gi_inventor_has_fem %>% group_by(num_inventors_clean) %>% summarise(total = length(num_inventors_clean))
for_graph_inventors_team <- left_join(grouped_inventors_team, total_count_inventors_team, by = 'num_inventors_clean')
for_graph_inventors_team <- mutate(for_graph_inventors_team, PercentageofInventors = has_fem_inv_count/total)


graph3 <- ggplot(data=for_graph_inventors_team, aes(x=num_inventors_clean, y=PercentageofInventors, fill=has_fem_inv)) +
  geom_bar(stat="identity") + labs(y= "Percentage of Patents", x="Number of Inventors Associated with Patent", fill = "With at least one female inventor") +
  scale_fill_manual(values=c(cyan, darkPurple, darkGreen),labels=c( "All Male Inventors", "At Least One Woman", "Unknown")) + 
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  )
graph3

ggsave (paste0("out\\percent_with_female_inventor_by_team_size.png"), device = "png")
