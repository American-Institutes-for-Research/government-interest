# create visualizations for government interest issue brief
# sarora@air.org
# march 2017
# check for supporting docs and code in github or on the patentsview shared drive 

source("helpers.R")

loadfonts(device = "win")
pkg_list <- c("trend", "plyr", "scales", "MASS", "reshape", "tools", "plotly", "rjson",
              "data.table", "gridExtra", "psych", "dplyr", "dbplyr", "extrafont", "ggplot2")
lapply(pkg_list, install_pkgs)

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
source("G:/PatentsView/config.r")
my_db=src_mysql(dbname=dbname,host=host,port=port,user=user,password=password)
##################################################f#######################################################

### read in tables from mySQL DB

#read from db
in.patent_level <- as.data.frame(tbl(my_db, "temp_patent_level_gi"))
in.gov_level <- as.data.frame(tbl(my_db, "temp_gi_level_gi"))
in.all <- as.data.frame(tbl(my_db, "temp_patent_level_all"))
in.assignees.all <- as.data.frame(tbl(my_db, "all_assignees"))
in.cite_1 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr1"))
in.cite_2 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr2"))
in.cite_3 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr3"))
in.cite_4 <- as.data.frame(tbl(my_db, "temp_5yr_citations_by_cite_yr4"))
in.cite_5 <- as.data.frame(tbl(my_db, "temp_5yr_citations_yr5")) 

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


#read from db
in.patent_level <- as.data.frame(tbl(my_db, "temp_patent_level_gi_v3"))
in.gov_level <- as.data.frame(tbl(my_db, "temp_gi_level_gi_v3"))

# these government interest patents have some full text fields that contain organizations; not fixing here
# process patent level data
in.patent_level <- in.patent_level %>% filter(year != "NULL" & year >= 1980 & year <= 2017) # filter on years 
patents.keep_ids <- in.patent_level$patent_id                    
write.csv (patents.keep_ids, file="out\\patents.keep_ids.csv")

# filter on gov_level ids to get the right data 
in.gov_level <- in.gov_level %>% filter(patent_id %in% patents.keep_ids)

# merge data two main data files
in.patent_level.merged <- merge(in.patent_level, in.gov_level, by="patent_id")
write.csv (in.patent_level.merged, file="out\\out.patent_level.merged.csv")


#########################################################################################################
# Plots

p.top6_techFields <- gi_patents_in_top6_techFields(in.patent_level) 
ggsave (filename = paste0("data_viz\\longWipoFields_v", script_v, ".png"), plot = top6_plot, device = "png")



p.share_gi_total_patents <- share_of_gi_patents_from_total(in.all, in.patent_level)
ggsave (filename = paste0("data_viz\\longWipoFieldsPercent_v", script_v, ".png"), plot = 
          p.share_gi_total_patents, device = "png")


p.growth_gi_all_rd <- growth_gi_allPatents_rdFund(in.fund, in.all)
ggsave (filename = paste0("data_viz\\indexed_", script_v, ".png"), plot = g.12, device = "png")


p.mean_num_patents <- mean_num_inv_gi_patents(in.patent_level, in.all)
ggsave (filename = paste0("data_viz\\longInventor_v", script_v, ".png"), plot = mean_num_inv_plot, device = "png")


p.patent_flow <- top_gi_patent_flow(in.assignees, in.patent_level.merged)
orca(patent_flow_plot, file = paste0("data_viz\\Sankey_", script_v, ".png"))
htmlwidgets::saveWidget(p.sk, file = paste0("F:\\Govt_Int\\Final_CSVS\\out\\assignees\\Sankey org name_", script_v, "_", ".html"))



# merge sector type for assignees and create dodged bar chart
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


p.gi_patents_fund <- gi_patents_funding_agencies()
ggsave (filename = paste0("data_viz\\funders-assignees.dodged_", script_v, ".png"), plot = funding_agencies_plot, device = "png")


p.firm_size <- firmsize() 
ggsave (filename = paste0("data_viz\\firmSize_", script_v, ".png"), plot = g.19n, device = "png")

p.citation <- five_year_citation_analysis()
ggsave (filename = paste0("data_viz\\fiveYearCitationImpact_", script_v, ".png"), plot = citation_plot, device = "png")

# check sectors before and after my improvements
in.sector.archive %>% group_by(thes_types) %>% summarise(count = length(patent_id))
in.sector %>% group_by(thes_types) %>% summarise(count = length(patent_id))
gov.archive <- in.sector.archive[which(in.sector.archive$thes_types == "Government"), ]
View(gov.archive %>% group_by(organization) %>% summarise(count = length(patent_id)))
