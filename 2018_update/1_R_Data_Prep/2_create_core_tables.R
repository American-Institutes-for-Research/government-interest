library(dplyr)
library(tidyr)

#read in table
patent_inventor <- read.csv(file = "patent_inventor.csv", header=TRUE, sep=",")
patent_assignee <- read.csv(file = "patent_assignee.csv", header=TRUE, sep=",")
nber <- read.csv(file = "nber.csv", header=TRUE, sep=",")
wipo <- read.csv(file = "wipo.csv", header=TRUE, sep=",")
wipo_field <- read.csv(file = "wipo_field.csv", header=TRUE, sep=",")
patent <- read.csv(file = "patent.csv", header=TRUE, sep=",")
temp_5yr_citations <- read.csv(file = "temp_5yr_citations.csv", header=TRUE, sep=",")
patent_govintorg <- read.csv(file = "patent_govintorg.csv", header=TRUE, sep=",")
government_organization <- read.csv(file = "government_organization.csv", header=TRUE, sep=",")

## Create the main Patent Level and Government Interest Level tables
## These tables have a lot of details around the patents including information from the database
## and  also from the citition count tables generated in the previous steps

############################################################################
##  All Patents
############################################################################
c <- patent_inventor %>% 
        group_by(patent_id) %>% 
        count(inventor_id) %>% 
        rename(num_inventors = n) %>% 
        select(patent_id, num_inventors)

d <- patent_assignee %>% 
        group_by(patent_id) %>% 
        count(assignee_id) %>% 
        rename(num_assignees = n) %>% 
        select(patent_id, num_assignees)

e <- c %>% left_join(d, by = "patent_id")

w <- wipo %>% 
      filter(sequence = 0) %>% 
      right_join(e, by = "patent_id") %>% 
      rename(id = field_id)

wf <- w %>% 
          left_join(wipo_field, by = "id") %>% 
          rename(wipo_sector = sector_title, wipo_field = field_title)
p <- wf %>% 
        left_join(table, by = "patent_id") %>% 
        select(patent_id, year, num_us_patents_cited, num_us_applications_cited, num_foreign_documents_cited, kind, type)
n <- nber %>% 
      select(patent_id, category_title, subcategory_title) %>% 
      rename(nber_category = category_title, nber_subcategory = subcategory_title) %>%  
      right_join(p, by = "patent_id")


temp_patent_level_all <- n %>% 
                          left_join(temp_5yr_citations, by = "patent_id") %>% 
                          mutate(num_inventors = if_else(is.na(num_inventors), 0, num_inventors),
                                 num_assignees = if_else(is.na(num_assignees), 0, num_assignees),
                                 weighted_cites_5yrs = ifelse(is.na(weighted_cites_5yrs), 0, weighted_cites_5yrs),
                                 num_citations_in_5yrs = if_else(is.na(num_citations_in_5yrs), 0, num_citations_in_5yrs))
write.csv(temp_patent_level_all, file = "temp_patent_level_all.csv")

############################################################################
##  Government Interest Patents
############################################################################

  
## table of just GI patents
## each row is a patent and each patent appears only once

govint_distinct_id <- patent_govintorg %>% distinct(patent_id)

temp_patent_level_gi <- temp_patent_level_all %>% 
                          filter(patent_id %in% govint_distinct_id$patent_id)
write.csv(temp_patent_level_gi, file = "temp_patent_level_gi.csv")

## government-interest level table of just GI Patents
temp_gi_level_gi <- patent_govintorg %>%
                      left_join(government_organization, by ="organization_id")

############################################################################
##  Non Government Interest Patents
############################################################################

## patent-level data for just non-GI patents
temp_patent_level_nongi <- temp_patent_level_all %>% 
  filter(!(patent_id %in% govint_distinct_id$patent_id))

write.csv(temp_patent_level_nongi, file = "temp_patent_level_nongi.csv")
