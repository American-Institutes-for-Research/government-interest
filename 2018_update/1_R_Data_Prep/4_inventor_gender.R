# read in table
temp_inventor_gender <- read.csv(file = "temp_inventor_gender.csv", header=TRUE, sep=",")
temp_patent_level_gi <- read.csv(file = "temp_patent_level_gi.csv", header=TRUE, sep=",")
patent_inventor <- read.csv(file = "patent_inventor.csv", header=TRUE, sep=",")
temp_govt_associated_inventors_clean <- read.csv(file = "temp_govt_associated_inventors_clean.csv", header=TRUE, sep=",")
temp_inventor_gender <- read.csv(file = "temp_inventor_gender.csv", header=TRUE, sep=",")

## Inventor gender data 
## the tbales you wante are temp_gi_inventor_gender (which has gender and wipo sector etc)
##and temp_gi_has_female_inv which has an indicator for whether the patent has any female inventors

## the existing temp_inventor_gender is the results the italians produced, uploaded ot mysql 

a <- temp_patent_level_gi %>% select(patent_id)
g <- temp_inventor_gender %>% 
      rename(inventor_id = id) %>% 
      filter(!is.na(inventor_id))

patent_id_filter_lst <- a %>% 
                          left_join(g, by = "inventor_id") %>% 
                          select(patent_id)

temp_govt_associated_inventors_clean <- patent_inventor %>% 
                                            filter(patent_id %in% patent_id_filter_lst$patent_id) %>% 
                                            select(patent_id, inventor_id, dumale)

write.csv(temp_govt_associated_inventors_clean, file = "temp_govt_associated_inventors_clean.csv")



## create table temp_gi_inventor_gender
tg <- temp_govt_associated_inventors_clean %>% filter(!is.na(dumale)) %>%  select(patent_id, inventor_id, dumale)
temp_gi_inventor_gender <- temp_patent_level_gi %>% 
                             select(patent_id, num_inventors, year, wipo_sector, wipo_field) %>% 
                             left_join(tg, by ="patent_id")
write.csv(temp_gi_inventor_gender, file = "temp_gi_inventor_gender.csv")

## create table 
temp_gi_has_female_inv <- temp_gi_inventor_gender %>% 
                            select(patent_id, dumale) %>% 
                            group_by(patent_id) %>% 
                            summarise(gender_min = min(dumale)) %>% 
                            mutate(has_fem_inv = ifelse(gender_min == 0, 1, 0)) %>% 
                            select(patent_id, has_fem_inv)

write.csv(temp_gi_has_female_inv, file = "temp_gi_has_female_inv.csv")