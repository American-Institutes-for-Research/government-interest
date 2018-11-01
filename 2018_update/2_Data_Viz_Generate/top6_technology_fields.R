# Gov Int Patents, Top 6 Technology Fields
# Government interest patents granted 1980-2017, top six technology fields
# Figure 2 in gov interest data brief

  
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
    filter(wipo_field %in% top6)
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
  
  write.csv (long.6a, file = "out\\gi.wipo_fields.keep6.csv") 
  


# Share of total patents with a government interest 2005-2017, top six technology fields 
# Figure 3 in gov interest data brief

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
  

