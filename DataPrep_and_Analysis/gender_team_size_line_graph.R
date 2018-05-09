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
darkRed <- rgb (134, 14, 34,	maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (0, 111, 39, maxColorValue=255)
darkPurple <- rgb (35, 9, 57, maxColorValue=255)
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

gi_inventor_has_fem$num_inventors_cat <-cut(gi_inventor_has_fem$num_inventors, c(0,1,2,5,100), labels = c('1', '2', '3-5', '6+'))

#organize the data to get the percentage of patents in each year with atleast one woman
grouped_gi_inventor_has_fem <- gi_inventor_has_fem %>% group_by(year, num_inventors_cat) %>% summarise(count_inv = n(), count_fem = sum(has_fem_inv), pct_fem = sum(has_fem_inv)/n())



graph1 <- ggplot(grouped_gi_inventor_has_fem) +
  geom_line(aes(x=year,y=pct_fem,color=num_inventors_cat,linetype=num_inventors_cat), size = 1.5) +
  ylab(label="% patents with atleast one female inventor") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkPurple, darkGreen, darkRed))+
  scale_linetype_manual(values=c("solid", "dashed", "twodash", "dotted")) +
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