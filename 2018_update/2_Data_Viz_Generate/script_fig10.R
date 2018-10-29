# create visualizations for government interest issue brief
# sarora@air.org
# march 2017
# check for supporting docs and code in github or on the patentsview shared drive 

library(trend) 
library(plyr)
library(scales)
library(MASS)
library(reshape)
library(tools)
library(plotly)
library(rjson) 
library(data.table)
library(gridExtra)
library(psych)
library(trend)
library(dbplyr)
library(dplyr)
library(extrafont)
loadfonts(device = "win")
windowsFonts(Arial=windowsFont("TT Arial"))
library (ggplot2)
# set color scheme
cyan <- rgb (0, 123, 188,	maxColorValue = 255)
darkRed <- rgb (208, 32, 47, maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (91, 140, 41, maxColorValue=255)
darkPurple <- rgb (135, 30, 110, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (152, 152, 152, maxColorValue=255)

setwd("F:/Govt_Int/2018_Update/2018_Update_gov_int") 
script_v <- "3.0"
#########################################################################################################

# connect to the aws mySQL server
source("G:\\config.r")
my_db=src_mysql(dbname=dbname_new,host=host,port=port,user=user,password=password)
##################################################f#######################################################

### read in tables from mySQL DB

#read from db
in.patent_level <- as.data.frame(tbl(my_db, "temp_patent_level_gi_v3"))
in.gov_level <- as.data.frame(tbl(my_db, "temp_gi_level_gi_v3"))
in.all <- as.data.frame(tbl(my_db, "temp_patent_level_all"))
in.assignees.all <- as.data.frame(tbl(my_db, "all_assignees"))
in.cite_1 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr1"))
in.cite_2 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr2"))
in.cite_3 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr3"))
in.cite_4 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr4"))
in.cite_5 <- as.data.frame(tbl(my_db, "temp_5yr_citations_yr5")) 

#read from db
in.patent_level <- as.data.frame(tbl(my_db, "temp_patent_level_gi_v3"))
in.gov_level <- as.data.frame(tbl(my_db, "temp_gi_level_gi_v3"))
in.all <- as.data.frame(tbl(my_db, "temp_patent_level_all_v3"))
in.assignees.all <- as.data.frame(tbl(my_db, "all_assignees"))
in.cite_1 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr1_v3"))
in.cite_2 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr2_v3"))
in.cite_3 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr3_v3"))
in.cite_4 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr4_v3"))
in.cite_5 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr5_v3")) 

# if you used R to generate table, change the read method of following tables to read from local
#in.patent_level <- read.csv("data_to_read\\temp_patent_level_gi.csv", header = TRUE, stringsAsFactors = FALSE)
#in.gov_level <- read.csv("data_to_read\\temp_gi_level_gi.csv", header = TRUE, stringsAsFactors = FALSE)
#in.all <- read.csv("data_to_read\\temp_patent_level_all", header = TRUE, stringsAsFactors = FALSE)
#in.assignees.all <- read.csv("data_to_read\\all_assignees", header = TRUE, stringsAsFactors = FALSE)
#in.cite_1 <- read.csv("data_to_read\\temp_5yr_citations_by_cite_yr1", header = TRUE, stringsAsFactors = FALSE)
#in.cite_2 <- read.csv("data_to_read\\temp_5yr_citations_by_cite_yr2", header = TRUE, stringsAsFactors = FALSE)
#in.cite_3 <- read.csv("data_to_read\\temp_5yr_citations_by_cite_yr3", header = TRUE, stringsAsFactors = FALSE)
#in.cite_4 <- read.csv("data_to_read\\temp_5yr_citations_by_cite_yr4", header = TRUE, stringsAsFactors = FALSE)
#in.cite_5 <- read.csv("data_to_read\\temp_5yr_citations_by_cite_yr5", header = TRUE, stringsAsFactors = FALSE)


### read in tables from local file(not exist in db)
in.sector <- read.csv("data_to_read\\assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = FALSE)
in.fund <- read.csv("data_to_read\\Agencies.csv", header = TRUE, stringsAsFactors = FALSE)
in.size <- read.csv("data_to_read\\government_interest_patents_1980-2018_returned.csv", header = TRUE, stringsAsFactors = FALSE)
#########################################################################################################


# if you look like at these government interest patents, there are some full text fields which have organizations in there; not fixing here
# process patent level data
in.patent_level <- in.patent_level %>% filter(year != "NULL" & year >= 1980 & year <= 2017) # filter on years 
patents.keep_ids <- in.patent_level$patent_id                    

# filter on gov_level ids to get the right data 
in.gov_level <- in.gov_level %>% filter(patent_id %in% patents.keep_ids)
# merge data two main data files
in.patent_level.merged <- merge(in.patent_level, in.gov_level, by="patent_id")
test <- left_join(in.patent_level, in.gov_level, by = "patent_id")

 #########################################################################################################

# 18. merge sector type for assignees and create dodged bar chart
in.sector$organization <- trimws(in.sector$organization)
sector_org.merged <- in.patent_level.merged %>% 
  select(patent_id, level_one) %>% 
  distinct() %>% # just patent_id, level_one org
  inner_join(in.sector, by="patent_id") %>% 
  select(patent_id, level_one, type, organization, thes_types)

# add a column weight = 1 / number of patent_id
sector_org.merged.ratio <- sector_org.merged %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n())

# change the thes_types
sector_org.merged.ratio.clnd <- sector_org.merged.ratio %>% 
  mutate(thes_types = recode(thes_types, 'Ambiguous' = 'Corporate',
                             'Hospital' = "Academic or Hospital",
                             'Academic' = "Academic or Hospital",
                             'Person' = "Other"))
# distinct get top funders
level_one.clnd <- in.gov_level %>% 
  select(patent_id, level_one) %>% 
  distinct() %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n())

# add a column weight = 1 / number of patent_id
sector_org.merged.ratio.clnd <- sector_org.merged.ratio.clnd %>% 
  group_by(patent_id) %>% 
  mutate(weight = 1/n())


#########################################################################################################
#########################################################################################################

# 27. Five year citation analysis 
# merge in.patent_level gi only with each of the five year counts; left join on all.  Must remove 2014 and later patents
in.cite_1_sum <- in.cite_1 %>%  
  group_by(cited_patent_id) %>% 
  summarise(num_citation = sum(num_times_cited_by_us_patents)/n()) %>% 
  rename(patent_id = cited_patent_id, num_citation = num_citation)

in.cite_2_sum <- in.cite_2 %>%  
  group_by(cited_patent_id) %>% 
  summarise(num_citation = sum(num_times_cited_by_us_patents)/n()) %>% 
  rename(patent_id = cited_patent_id, num_citation = num_citation)

in.cite_3_sum <- in.cite_3 %>%  
  group_by(cited_patent_id) %>% 
  summarise(num_citation = sum(num_times_cited_by_us_patents)/n()) %>% 
  rename(patent_id = cited_patent_id, num_citation = num_citation)

in.cite_4_sum <- in.cite_4 %>%  
  group_by(cited_patent_id) %>% 
  summarise(num_citation = sum(num_times_cited_by_us_patents)/n()) %>% 
  rename(patent_id = cited_patent_id, num_citation = num_citation)

in.cite_5_sum <- in.cite_5 %>%  
  group_by(cited_patent_id) %>% 
  summarise(num_citation = sum(num_times_cited_by_us_patents)/n()) %>% 
  rename(patent_id = cited_patent_id, num_citation = num_citation)

# merge all five year analysis data together
cit.merge_1 <- in.patent_level %>%
  filter(year < 2013) %>% 
  left_join(in.cite_1_sum, by = c("patent_id" = "patent_id"))
cit.merge_2 <- left_join(cit.merge_1, in.cite_2_sum, by = c("patent_id" = "patent_id"), suffix = c("_yr1", "_yr2"))
cit.merge_3 <- cit.merge_2 %>% left_join(in.cite_3_sum, by = c("patent_id" = "patent_id"),suffix = c("_yr2", "_yr3"))
cit.merge_4 <- cit.merge_3 %>% left_join(in.cite_4_sum, by = c("patent_id" = "patent_id"),suffix = c("_yr3", "_yr4")) 
cit.merge_5 <- cit.merge_4 %>% left_join(in.cite_5_sum, by = c("patent_id" = "patent_id"),suffix = c("_yr4", "_yr5"))
sector_merge <- cit.merge_5 %>% left_join(sector_org.merged.ratio.clnd, by="patent_id")

#calculate the weight for each year
sector_merge$First <- sector_merge$num_citation_yr1 * sector_merge$weight
sector_merge$Second <- sector_merge$num_citation_yr2 * sector_merge$weight
sector_merge$Third <- sector_merge$num_citation_yr3 * sector_merge$weight
sector_merge$Fourth <- sector_merge$num_citation_yr4 * sector_merge$weight
sector_merge$Fifth <- sector_merge$num_citation * sector_merge$weight
sector_merge.sub <- sector_merge[,c(23:30)]

sector_merge.sub[is.na(sector_merge.sub)] <- 0
cit_sector.count <- aggregate(cbind (weight, First, Second, Third, Fourth, Fifth)
                              ~
                                thes_types, sum, na.rm= TRUE, data = sector_merge.sub)
cit_sector.count.clnd <- cit_sector.count %>% filter(thes_types != 0) 


# average by year
cit_sector.count.clnd$First <- cit_sector.count.clnd$First / cit_sector.count.clnd$weight
cit_sector.count.clnd$Second <- cit_sector.count.clnd$Second / cit_sector.count.clnd$weight
cit_sector.count.clnd$Third <- cit_sector.count.clnd$Third / cit_sector.count.clnd$weight
cit_sector.count.clnd$Fourth <- cit_sector.count.clnd$Fourth / cit_sector.count.clnd$weight
cit_sector.count.clnd$Fifth <- cit_sector.count.clnd$Fifth / cit_sector.count.clnd$weight
cit_sector.count.melt <- melt(cit_sector.count.clnd, id="thes_types", measure.vars = c(3:7))
cit_sector.count.melt.clnd <- cit_sector.count.melt %>% filter(thes_types != "Other")

g.27 <- ggplot(cit_sector.count.melt.clnd, aes(x=variable,y=value,fill=thes_types)) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(y= "Average Accrued Weighted Citations", x="Year After Publication") +
  scale_y_continuous(label=comma) +
  scale_fill_manual(values=c(darkPurple, cyan, darkGreen), labels=c("Academic", "Corporate", "Government")) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        text=element_text(size=16,  family="Cambria"),
        legend.title=element_blank()
  )

g.27
ggsave (filename = paste0("data_viz\\fiveYearCitationImpact_", script_v, ".png"), plot = g.27, device = "png")

#########################################################################################################
