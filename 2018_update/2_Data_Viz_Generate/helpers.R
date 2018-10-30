# Requires: R package
# Modifies: Nothing
# Effects: Installs package 
install_pkgs <- function(pkg) { 
  
  # Install package if it isn't already
  if (!(pkg %in% installed.packages()[, "Package"])){ 
    
    install.packages(pkg, repos='http://cran.us.r-project.org')
  }
  
  library(pkg, character.only = TRUE)
  
  
} # end install_pkgs()




# Gov Int Patents, Top 6 Technology Fields
# Government interest patents granted 1980-2017, top six technology fields
# Figure 2 in gov interest data brief
gi_patents_in_top6_techFields <- function(in.patent_level){
  
  # Patent classifications -- WIPO fields (below sectors)
  freq.wipo_field <- as.data.frame(table(in.patent_level$wipo_field))
  freq.wipo_field <- freq.wipo_field %>% 
    select(wipo_field = Var1, freq = Freq)
  
  # Cleaning step for NULL data
  freq.wipo_field.nn <- freq.wipo_field %>% 
    filter(wipo_field != "NULL") %>%
    arrange(desc(freq))
  
  # Extract top 6 fields
  top6 <- as.character(freq.wipo_field.nn[1:6,1])
  
   
  sum.weights <- sum(freq.wipo_field.nn$freq)
  freq.wipo_field.nn <- freq.wipo_field.nn %>%
    mutate(percentage = freq/sum.weights)
  
  # Get longitudinal by adding year column (gi_patents)
  long.6a <- as.data.frame(table(in.patent_level$wipo_field, in.patent_level$year))
  
  keep.6a <- long.6a %>% 
    select(wipo_field = Var1, year = Var2, freq = Freq) %>% 
    filter(wipo_field %in% top5)
  long.6a <- long.6a %>% select(wipo_field = Var1, year = Var2, freq = Freq)
 
   # Change year var from discrete values to continuous values - allow plotting
  keep.6a$year <- as.numeric(levels(keep.6a$year))[keep.6a$year]
  
  top6_plot <- ggplot(keep.6a, aes(x = year, y = freq, colour=wipo_field, group = wipo_field, linetype=wipo_field)) + 
    geom_line(size=1.5) + 
    ylab(label="Number of Patents") +  xlab("Year") +
    scale_colour_manual(values=c(cyan, darkGrey, darkGreen, darkRed, darkPurple, lightGrey)) +
    scale_linetype_manual(values=c("solid", "twodash", "dashed", "dotted", "longdash", "dotdash")) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    #scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
    theme_set(theme_gray(base_size = 16)) + 
    guides(colour=guide_legend(nrow=3,byrow=TRUE)) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_blank(),
          legend.key.width = unit(3, 'lines'),
          legend.title=element_blank(),
          text=element_text(size=16,  family="Cambria")
    )
  top6_plot
   
  write.csv (long.5a, file = "out\\gi.wipo_fields.keep6.csv") 
  
}


# Share of total patents with a government interest 2005-2017, top six technology fields 
# Figure 3 in gov interest data brief
share_of_gi_patents_from_total <- function(in.all, in.patent_level){
  
  # Patent classifications -- WIPO fields (below sectors)
  freq.wipo_field <- as.data.frame(table(in.patent_level$wipo_field))
  freq.wipo_field <- freq.wipo_field %>% 
    select(wipo_field = Var1, freq = Freq)
  
  # Cleaning step for NULL data
  freq.wipo_field.nn <- freq.wipo_field %>% 
    filter(wipo_field != "NULL") %>%
    arrange(desc(freq))
  
  # Extract top 6 fields
  top6 <- as.character(freq.wipo_field.nn[1:6,1])
  
  # get wipo_field frequencies across all patents
  in.all.since_1980 <- in.all %>% filter(year > 1980 & year <= 2017)
  in.all.since_2005 <- in.all %>% filter(year >= 2005 & year <= 2017)
  
  freq.wipo_field.all <- as.data.frame(table(in.all.since_2005$wipo_field))
  freq.wipo_field.all <- freq.wipo_field.all %>% 
    select(wipo_field = Var1, freq = Freq)
  
  
  # determine overall GI patent ratio vis-a-vis all patents, by wipo field
  freq.wipo_field.merge <- merge (freq.wipo_field, freq.wipo_field.all, by="wipo_field")
  freq.wipo_field.merge <- freq.wipo_field.merge %>% mutate(ratio = freq.x/freq.y)
  
  
  long.6b <- as.data.frame(table(in.all.since_2005$wipo_field, in.all.since_2005$year))
  long.6b <- long.6b %>% select(wipo_field = Var1, year = Var2, freq = Freq)
  merged.long5 <- long.6a %>% 
    inner_join(long.6b, by=c("wipo_field","year"), suffixes=c("_5a", "_5b")) %>% 
    mutate(ratio = freq.x/freq.y)
  
  # keep only the top 5 wipo_fields
  keep.6b <- long.6b %>% filter(wipo_field %in% top6)
  keep.6b$year<-as.numeric(levels(keep.6b$year))[keep.6b$year]
  merged.keep6 <- keep.6a %>% 
    inner_join(keep.6b , by=c("wipo_field","year"), suffixes=c("_6a", "_6b")) %>% 
    mutate(ratio = freq.x/freq.y)
  
  #change the year from discrete value to continuous value so that it could be plot by ggplot2
  merged.keep6$year <- as.numeric(merged.keep6$year)
  
  share_gi_total_plot <- ggplot(merged.keep6) + 
    geom_line(aes(x = year, y = ratio, colour=wipo_field, group = wipo_field, linetype = wipo_field), size=1.5) + 
    ylab(label="Percent of Government Interest Patents to All Patents") +  xlab("Year") + 
    scale_colour_manual(values=c(cyan, darkGrey, darkGreen, darkRed, darkPurple, lightGrey)) +
    scale_x_continuous(breaks=c(2005,2010,2015), minor_breaks = seq(2005, 2016, 1)) +
    expand_limits(y = 0) +
    scale_linetype_manual(values=c("solid", "twodash", "dashed", "dotted", "longdash", "dotdash")) +
    scale_y_continuous(labels = scales::percent) +
    theme_set(theme_gray(base_size = 16)) + 
    guides(colour=guide_legend(nrow=3,byrow=TRUE)) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_blank(),
          legend.key.width = unit(3, 'lines'),
          legend.title=element_blank(),
          text=element_text(size=16,  family="Cambria")
    ) 
  share_gi_total_plot
  write.csv (merged.keep5, file = "out\\merged_wipo_fields.keep6.csv")

}

# Growth in Government Interest, all Patents, and real Federal R&D Funding
# Figure 1 in gov interest data brief
growth_gi_allPatents_rdFund <- function(in.fund, in.all){
  
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
  
}


# Mean Number of Inventors in Gov Interest Patents and Across All Patents
# Figure 7 in gov interest data brief
mean_num_inv_gi_patents <- function(in.patent_level, in.all){
  
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
  
}


# Top Gov Interest to assignee Patenting Flows
# Figure A in gov interest data brief
top_gi_patent_flow <- function(in.assignees, in.patent_level.merged){
  
  # process assignees table
  in.assignees <- in.assignees.all %>% 
    filter(patent_id %in% patents.keep_ids)
  
  in.assignees$name <- paste (in.assignees$name_first, in.assignees$name_last, sep = " ")
  in.assignees$entity <- in.assignees$organization
  in.assignees$entity[which(in.assignees$entity == "")] <- in.assignees$name[which(in.assignees$entity == "")]
  
  assignees.clnd <- in.assignees[,c(1,2,8)]
  #calculate the weight for each patent_id
  assignees.clnd <- assignees.clnd %>%  
    group_by(patent_id) %>% 
    mutate(weight = 1/n())
  
  assignees.merged <- assignees.clnd %>%  inner_join(in.patent_level.merged, by="patent_id")
  assignees.merged$entity <- toTitleCase(tolower(assignees.merged$entity))
  
  
  # sankey viz for funders --> assignees to do the network viz
  
  assignees.merged.sub <- assignees.merged[,c(1, 3, 20)] # patent_id, entity, level_one
  ass_org.merged.ratio <- assignees.merged.sub %>%  
    distinct() %>% 
    group_by(patent_id) %>% 
    mutate(weight = 1/n())
  
  count.ass_org.srtd <- ass_org.merged.ratio %>% 
    group_by(entity, level_one) %>% 
    summarise(freq = sum(weight)) %>% 
    arrange(desc(freq))
  
  top.rows <- 30
  
  count.ass_org.srtd <- count.ass_org.srtd[1:top.rows,]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the United States Department of Energy", count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Administrator of the National Aeronautics and Space Administration", count.ass_org.srtd$entity), ]
  count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Secretary of the Navy", count.ass_org.srtd$entity), ]
  
  # create nodes
  top.rows <- nrow(count.ass_org.srtd)
  top.funders <- unique(count.ass_org.srtd[1:top.rows,2])
  top.assignees <- unique(count.ass_org.srtd[1:top.rows,1])
  nodes = unlist(c(top.funders, top.assignees))
  
  # create source 
  source <- list(count.ass_org.srtd$level_one)
  source_lst <- lapply(source, function(x) match(x,nodes))
  count.ass_org.srtd$source <- unlist(source_lst)
 
   # create target 
  target <- list(count.ass_org.srtd$entity)
  target_lst <- lapply(target, function(x) match(x,nodes))
  count.ass_org.srtd$target <- unlist(target_lst)
  
  # impute funder node ids onto original counts 
  funders.links <- sapply(1:length(count.ass_org.srtd), function(x) grep(nodes[x], count.ass_org.srtd[,2], ignore.case = TRUE))
  fl.df <- data.frame(ID = rep(seq(funders.links), sapply(funders.links, length)), Obs = unlist(funders.links))
  fl.df[, 1] <- fl.df[,1] - 1
  count.ass_org.srtd$source <- fl.df[order(fl.df$Obs), 1]
  
  # impute assignee node ids onto original counts
  assignees.links <- sapply(1:nrow(count.ass_org.srtd), function(x) grep(nodes[x], count.ass_org.srtd[,1], ignore.case = TRUE))
  al.df <- data.frame(ID = rep(seq(assignees.links), sapply(assignees.links, length)), Obs = unlist(assignees.links))
  al.df[,1] <- al.df[,1] + length(top.funders) - 1 # offset
  count.ass_org.srtd$target <- al.df[order(al.df$Obs), 1]
  
  View(count.ass_org.srtd)
  
  patent_flow_plot <- plot_ly(
    type = "sankey",
    orientation = "h",
    
    node = list(
      label = nodes,
      # color = json_data$data[[1]]$node$color,
      pad = 15,
      thickness = 15,
      line = list(
        color = "black",
        width = 0.5
      )
    ), 
    
    link = list(
      source = count.ass_org.srtd$source,
      target = count.ass_org.srtd$target,
      value =  count.ass_org.srtd$freq
      # color =  json_data$data[[1]]$link$color,
      # label =  json_data$data[[1]]$link$label
    )
  ) %>% 
    layout(
      title = "Top Government Interest Organizations to Assignees, 1980 - 2017",
      font = list(
        size = 10
      ),
      xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
      yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
    )
  
  patent_flow_plot
  
    
}

# Gov Interest Patents with Top High Level Federal Funding Agencies & Assignee Sectors
# Figure 4 in gov interest data brief
gi_patents_funding_agencies <- function(){
  
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
    
}

# Create firm size
firmsize <- function(){
 
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
    write.csv (patent_size.cnt, file = "out\\patent_size.cnt.csv")
    firm_size_plot
  
}

# Avg. Number of Citations from Subsequent Patents Received by Gov Interest Patents by Assignee Sector - Last Five Years
# Figure 10 in gov interest data brief
five_year_citation_analysis <- function(){
  
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
  
    # export select datasets
  write.csv (cit_sector.count.melt.clnd, file = "out\\citations_accrued_years_1_thru_5.csv")
  write.csv (cit_sector.count.melt.clnd, file = "out\\cit_sector.count.melt.clnd.csv")
  
  citation_plot
  
}


sector_agency_field_datatable <- function(){
  # data table alinging sector, agency, and field, by year
  sector_agency_field_dt() 
  sector_org_field.by_year.merged <- merge(sector_org.merged.ratio.clnd, in.patent_level, by="patent_id")
  colnames (sector_org_field.by_year.merged)
  head(sector_org_field.by_year.merged)
  sector_org_field.by_year.small <- sector_org_field.by_year.merged[c(1,2,5,10,11)]
  sector_org_field.by_year.ratio <- sector_org_field.by_year.small %>% 
    group_by(patent_id) %>% 
    mutate(weight = 1/n())
  
  sector_org_field.by_year.clnd <- sector_org_field.by_year.ratio
  sector_org_field.by_year.clnd[which (! sector_org_field.by_year.clnd$level_one %in% top6 ), 2] <- "Other"
  sector_org_field.by_year.clnd[which (sector_org_field.by_year.clnd$level_one == "United States Government"), 2] <- "Other"
  
  sector_org_field.by_year.count <- sector_org_field.by_year.clnd %>% 
    group_by(level_one, thes_types, wipo_field, year) %>% 
    summarise(freq = sum(weight))
  
  # create patent_level-gov_level merged dataset and weight by level_one org 
  # 5/2/2018 for Andy's DOE presentation 
  in.patent_level.ratio <-  in.patent_level.merged %>% 
    group_by(patent_id) %>% 
    mutate(weight = 1/n())
  
  patents.by_agency <- in.patent_level.ratio %>% group_by (level_one) %>% summarize (count = sum(weight))
  
  write.csv (sector_org_field.by_year.count, file="out\\sector_org_field.by_year.count.csv")

  write.csv (in.patent_level.ratio, file = "out\\weighted_gi_patents.by_funding_agency.csv")
  
} 
