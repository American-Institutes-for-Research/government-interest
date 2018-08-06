library(dplyr)
library(tidyr)
# read in relevant data
uspatentcitation <- read.csv(file = "uspatentcitation.csv", header=TRUE, sep=",")
patent <- read.csv(file = "patent.csv", header=TRUE, sep=",")
patent_govintorg <- read.csv(file = "patent_govintorg.csv", header=TRUE, sep=",")

#For each patent, create the 5 year citation counts and weighted citation counts
#Uses only the government relationships in the government interest table (not government assignees)
#Does not require any other new tables to be pre-generated

############################################################################
##  All Patents
############################################################################
  
## table with all patents and any citations within 5 years
## table has the id and date of both cited and citing patent ids
temp_5yr_citations_by_cite_all <- uspatentcitation %>% select(citing_patent_id, cited_patent_id) %>% 
      left_join(patent, by=(c("citing_patent_id" = "patent_id"))) %>% 
      select(citing_patent_id, cited_patent_id, date, num_times_cited_by_us_patents) %>% 
      rename(citing_patent_date = date) %>%  
      left_join(patent, by=(c("cited_patent_id" = "patent_id"))) %>% 
      select(citing_patent_id, cited_patent_id, citing_patent_date, num_times_cited_by_us_patents.x, date) %>% 
      rename(cited_patent_date = date, num_times_cited_by_us_patents = num_times_cited_by_us_patents.x) %>% 
      mutate(date_diff = difftime(citing_patent_date, cited_patent_date)) %>% 
      filter(date_diff <= 365*5) %>% 
      select(citing_patent_id, cited_patent_id, citing_patent_date, cited_patent_date, num_times_cited_by_us_patents)

write.csv(temp_5yr_citations_by_cite_all, file = "temp_5yr_citations_by_cite_all.csv")



## derivative table with 5 year citation counts and weighted citation count
temp_5yr_citations_all_1 <- temp_5yr_citations_by_cite_all %>% 
                              group_by(cited_patent_id) %>% 
                              summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents))

temp_5yr_citations_all_2 <- temp_5yr_citations_by_cite_all %>% 
                              group_by(cited_patent_id) %>%
                              count(citing_patent_id) %>% 
                              rename(num_citations_in_5yrs = n) %>%
                              select(cited_patent_id, num_citations_in_5yrs)

temp_5yr_citations_all <- temp_5yr_citations_all_1 %>% 
                            inner_join(temp_5yr_citations_all_2, by = "cited_patent_id")

write.csv(temp_5yr_citations_all, file = "temp_5yr_citations_all.csv")
############################################################################
##  Government Interest Patents
############################################################################

  
## create citation tables for only government interest patents
## table has the id and date of both cited and citing patent ids
                            
distinct_patent_id <- patent_govintorg %>% distinct(patent_id)
temp_5yr_citations_by_cite <- temp_5yr_citations_by_cite_all %>% 
                              filter(cited_patent_id %in% distinct_patent_id$patent_id)

write.csv(temp_5yr_citations_by_cite, file = "temp_5yr_citations_by_cite.csv")

## derivative table with 5 year citation counts and weighted citation count
temp_5yr_citations_1 <- temp_5yr_citations_by_cite %>% 
                              group_by(cited_patent_id) %>% 
                              summarize(weighted_cites_5yrs = sum(num_times_cited_by_us_patents))

temp_5yr_citations_2 <- temp_5yr_citations_by_cite %>% 
                              group_by(cited_patent_id) %>%
                              count(citing_patent_id) %>% 
                              rename(num_citations_in_5yrs = n) %>%
                              select(cited_patent_id, num_citations_in_5yrs)

temp_5yr_citations <- temp_5yr_citations_1 %>% 
                              inner_join(temp_5yr_citations_2, by = "cited_patent_id")

write.csv(temp_5yr_citations_all, file = "temp_5yr_citations.csv")