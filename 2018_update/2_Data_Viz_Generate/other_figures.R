
# Growth in Government Interest, all Patents, and real Federal R&D Funding
# Figure 1 in gov interest data brief

  
  # graph R&D expenditures over time (line) 
  in.fund$FederalIndex <- in.fund$Total.R.D / in.fund[in.fund$Fiscal.Year==1980, which(colnames(in.fund)=="Total.R.D")] * 100
  rd_expenditures_plot <- ggplot(data=in.fund, aes(x=Fiscal.Year, y=Total.R.D, group=1)) + geom_line(size=1.5, color="#0066CC") + 
    xlab ("Year") + ylab("Federal Expenditures (in billions of dollars)") + scale_y_continuous(labels = scales::dollar)
  rd_expenditures_plot
  
  # look at the increase in all patents over the years
  freq.all.by_year <- as.data.frame(table(in.all$year))
  freq.all.by_year <- freq.all.by_year %>% 
    rename(year = Var1, freq = Freq)
  
  # create two columns freqIndex and year
  freq.all.by_year$freqIndex <- freq.all.by_year$freq / freq.all.by_year[freq.all.by_year$year=="1980", which(colnames(freq.all.by_year)=="freq")] * 100
  freq.all.by_year$year <- as.numeric(levels(freq.all.by_year$year))
  freq.all.by_year <- freq.all.by_year %>%  
    filter(year <= 2017 & year >= 1980)
  
  # graph count of patents over time
  all_patents_plot <- ggplot(data=freq.all.by_year, aes(x=year, y=freq,group=1)) + 
    geom_line(size=1.5, color="#EC7C27") + xlab ("Number of patents") + ylab("Count") + 
    scale_y_continuous(labels = comma)
  all_patents_plot
  
  # increase in GI patents over the years
  freq.gi.by_year <- as.data.frame(table(in.patent_level$year))
  freq.gi.by_year <- freq.gi.by_year %>% 
    rename(year = Var1, freq = Freq)
  freq.gi.by_year$year=as.numeric(levels(freq.gi.by_year$year))
  freq.gi.by_year.clnd <- freq.gi.by_year %>% 
    filter(!is.na(year) & year <= 2017 & year >= 1980)
  
  freq.gi.by_year.clnd$freqIndex <- freq.gi.by_year.clnd$freq / freq.gi.by_year.clnd[freq.gi.by_year.clnd$year=="1980", which(colnames(freq.gi.by_year.clnd)=="freq")] * 100
  
  unique(freq.gi.by_year.clnd$year)
  gi_patents_plot <- ggplot(data=freq.gi.by_year.clnd, aes(x=year, y=freq,group=1)) + 
    geom_line(size=1.5, color="#EC272a") + xlab ("Number of patents") + ylab("Count") + 
    scale_y_continuous(labels = comma)
  gi_patents_plot
  
  # create index graphs across R&D expenditures, all patents, and gi patents
  merged.ind1 <- freq.gi.by_year.clnd %>% inner_join(freq.all.by_year , by="year", suffixes=c("_gi", "_all"))
  merged.ind2 <- merged.ind1 %>% inner_join(in.fund, by=c("year"= "Fiscal.Year"))
  melt.ind <- melt(merged.ind2, id="year", measure.vars = c(3,5,21))
  
  index_plot <- ggplot(melt.ind) + 
    geom_line(aes(x = year, y = value, colour=variable, linetype=variable), size=1.5) + 
    ylab(label="Indexed Value") +  xlab("Year") +
    scale_colour_manual(values=c(cyan, darkGrey, darkGreen), labels=c( "Government interest patents", "All patents", "Federal R&D funding")) +
    scale_linetype_manual(values=c("solid", "twodash", "dashed"), labels=c( "Government interest patents", "All patents", "Federal R&D funding")) +
    scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
    theme_set(theme_gray(base_size = 16)) + 
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_blank(),
          legend.key.width = unit(3, 'lines'),
          legend.title=element_blank(),
          text=element_text(size=16,  family="Cambria")
    ) 
  index_plot
  
   write.csv (freq.all.by_year, file="out\\sector_org_field.by_year.count")
   write.csv (freq.all.by_year, file="out\\freq.all.by_year.csv")
   write.csv (freq.gi.by_year, file="out\\freq.gi.by_year.csv")
   



# Mean Number of Inventors in Gov Interest Patents and Across All Patents
# Figure 7 in gov interest data brief

  
  # 13. inventors by year
  patent_level.bkp <- in.patent_level
  colnames(patent_level.bkp)
  nrow(patent_level.bkp)
  patent_level.bkp$year <- as.numeric(as.character(patent_level.bkp$year))
  gi.agg <- aggregate(patent_level.bkp[,c(2,3)], list(Year = patent_level.bkp$year), na.rm=TRUE, mean)
  
  all.bkp <- in.all 
  all.bkp$year <- as.numeric(as.character(all.bkp$year))
  all.agg <- aggregate(all.bkp[,c(2,3)], list(Year = all.bkp$year), na.rm=TRUE, mean)
  
  merged.agg <- gi.agg %>% inner_join(all.agg, by="Year", suffixes=c("_gi", "_all"))
  melt.agg <- melt(merged.agg, id="Year", measure.vars = c(2,4)) # was c(2:5)
  
  mean_num_inv_plot <- ggplot(melt.agg) + 
    geom_line(aes(x = Year, y = value, colour=variable, linetype=variable), size=1.5) + 
    scale_x_continuous(breaks=c(1980, 1985, 1990,1995,2000,2005,2010,2015)) +
    ylab(label="Mean Number of Inventors") +  xlab("Year") +
    scale_colour_manual(values=c(cyan, darkGrey), labels=c("Government interest patents", "All patents")) +
    scale_linetype_manual(values=c("solid", "twodash"), labels=c("Government interest patents", "All patents")) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_blank(),
          legend.key.width = unit(5, 'lines'),
          legend.title=element_blank(),
          text=element_text(size=16,  family="Cambria")
    )
  mean_num_inv_plot
  


# Gov Interest Patents with Top High Level Federal Funding Agencies & Assignee Sectors
# Figure 4 in gov interest data brief

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
  
  
  
  
  # get a table of unique level_one org with weighted count
  freq.level_one.srtd <- level_one.clnd %>% 
    group_by(level_one) %>% 
    summarise(count = sum(weight)) %>% 
    arrange(desc(count))
  top6 <- as.character(as.data.frame(freq.level_one.srtd)[1:6,1])
  
  count.sector_org <- sector_org.merged.ratio.clnd %>% 
    group_by(level_one, thes_types) %>% 
    summarise(count = sum(weight)) 
  
  count.sector_org.short <- count.sector_org %>% 
    filter(level_one %in% top6) %>% 
    filter(level_one != "United States Government") %>% 
    filter(thes_types != "Other")
  count.sector_org.short$level_one <- gsub ("([^ ]+) ", "\\1\n", count.sector_org.short$level_one)
  
  funding_agencies_plot <- ggplot(aes(y = count, x = reorder(level_one, -count), fill = thes_types), data = count.sector_org.short) + 
    geom_bar( stat="identity", position = "dodge") +
    ylab(label="Number of Patents") +  xlab("US Federal Department or Agency") +
    scale_y_continuous(label=comma) + 
    scale_fill_manual(values = c(darkPurple, cyan, darkGreen, darkGrey, darkRed, darkBlue)) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          text=element_text(size=16,  family="Cambria"),
          legend.title=element_blank()
    )
  funding_agencies_plot
  


# Create firm size

  
  # Assignees by firm size
  patent_size.merged.clnd <- sector_org.merged.ratio.clnd %>%
    inner_join(in.size, by="patent_id") %>% 
    inner_join(in.patent_level, by="patent_id") %>%
    filter(size_issue != "")
  
  # rename some fields according to the size of the organizations
  patent_size.merged.clnd$type <- ""
  colnames(patent_size.merged.clnd)
  patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Academic or Hospital"), 7] <- "Academic or Hospital" 
  patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "micro"), 7] <- "Small firm" 
  patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "small"), 7] <- "Small firm" 
  patent_size.short <- patent_size.merged.clnd %>% select(patent_id, thes_types, year, size_issue) # patent_id, thes_types (sector), and type
  patent_size.unique <- distinct(patent_size.short) # unique because sector_org contains gi orgs and we don't need that info here
  
  patent_size.ddply.short <- patent_size.unique %>% 
    group_by(patent_id) %>% 
    mutate(weight = 1/n()) 
  
  patent_size.cnt <-  patent_size.ddply.short %>% 
    group_by(year, thes_types, size_issue) %>% 
    summarise(freq = sum(weight)) %>% 
    filter(size_issue %in% list("Academic or Hospital", "Large firm", "Small firm"))
  
  patent_size.cnt <- patent_size.cnt[patent_size.cnt$year >= 1990,]
  
  firm_size_plot <- ggplot(patent_size.cnt, aes(x = year, y = freq, colour=size_issue, group = size_issue, linetype=size_issue)) + 
    geom_line(size=1.5) + 
    ylab(label="Weighted Number of Patents") +  xlab("Year") + 
    scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015)) +
    scale_y_continuous(label=comma, breaks=c(0,500,1000,1500,2000,2500, 3000, 3500)) +
    scale_colour_manual(values=c(darkGreen, darkBlue, cyan), labels=c("Academic or hospital", "Large firm", "Small firm")) +
    scale_linetype_manual(values=c("dashed", "twodash", "solid"), labels=c("Academic or hospital", "Large firm", "Small firm")) +
    theme_set(theme_gray(base_size = 16)) + 
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_blank(),
          legend.key.width = unit(3, 'lines'),
          legend.title=element_blank(),
          text=element_text(size=16,  family="Cambria")
    ) 
  
  firm_size_plot
  
  write.csv (patent_size.cnt, file = "out\\patent_size.cnt.csv")


