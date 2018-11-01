# Avg. Number of Citations from Subsequent Patents Received by Gov Interest Patents by Assignee Sector - Last Five Years
# Figure 10 in gov interest data brief
  
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


  
  # Five year citation analysis 
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


  
  
  citation_plot <- ggplot(cit_sector.count.melt.clnd, aes(x=variable,y=value,fill=thes_types)) +
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
  
  citation_plot  
 
  # export select datasets
  write.csv (cit_sector.count.melt.clnd, file = "out\\citations_accrued_years_1_thru_5.csv")
  write.csv (cit_sector.count.melt.clnd, file = "out\\cit_sector.count.melt.clnd.csv")
  


