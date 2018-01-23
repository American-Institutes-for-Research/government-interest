# create visualizations for government interest issue brief
# sarora@air.org
# november 2017
# check for supporting docs and code in github or on the patentsview shared drive 

library(trend)
library (ggplot2)
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

setwd("F:\\Govt_Int\\Final_CSVS") 
script_v <- "0.7"

# load patent level data
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)
View(in.patent_level[1:100,])
colnames(in.patent_level)
nrow(in.patent_level)

# filter on years 
in.patent_level.bkp <- in.patent_level
in.patent_level <- subset(in.patent_level, year >= 1980 & year <= 2015)
patents.keep_ids <- in.patent_level$patent_id
length(patents.keep_ids) # 123,275

# load government interest data 
in.gov_level <- read.csv("government_interest_level.csv", header = TRUE, stringsAsFactors = FALSE)
View(in.gov_level[1:100,])
colnames(in.gov_level)
in.gov_level$patent_id <- as.character(in.gov_level$patent_id)
nrow(in.gov_level)

# filter on gov_level ids to get the right years 
in.gov_level.bkp <- in.gov_level
in.gov_level <- in.gov_level[which(in.gov_level$patent_id %in% patents.keep_ids), ]
length(unique(in.gov_level$patent_id))

# create separate datasets for government interest (1) and government assignee data (0)
in.gi_level <- subset(in.gov_level, in_gi == 1)
in.ga_level <- subset(in.gov_level, in_gi == 0)
nrow(in.gov_level)
nrow(in.gi_level)
nrow(in.ga_level)
colnames(in.gov_level)

# merge data two main data files, but first determine whether patent is gi, ga, or somewhere in between 
in.gov_level.ratio <- ddply (in.gov_level, .(patent_id), transform, avg_gi= (mean(in_gi)))
colnames(in.gov_level.ratio)
in.gov_level.ratio.clnd <- in.gov_level.ratio[,c(1,7)]
in.gov_level.ratio.clnd <- in.gov_level.ratio.clnd[!duplicated(in.gov_level.ratio.clnd),]
in.patent_level.merged <- merge(in.patent_level, in.gov_level.ratio.clnd, by="patent_id")

# test this merge looks legit 
View(in.patent_level.merged)
in.gov_level[which(in.gov_level$patent_id=='4180935'),] # 1.0 weight 
in.gov_level[which(in.gov_level$patent_id=='4181062'),] # 0.0 weight 
in.gov_level[which(in.gov_level$patent_id=='4181139'),] # 0.5 weight


# 1. plot the most frequently observed values @ level_one. need for loop to iterate over patents with gi orgs, assignee orgs, and combined orgs
for (i in 0:2) {
  if (i < 2) {
    level_one <- in.gov_level[which(in.gov_level$in_gi==i),c(1,3)] # need to collapse on level one orgs to remove within group dups at the parent agency level
  }
  else {
    level_one <- in.gov_level[,c(1,3)] # need to collapse on level one orgs to remove within group dups at the parent agency level
  }
  level_one.clnd <- level_one[!duplicated(level_one),]
  View(level_one.clnd)
  level_one.clnd <- ddply (level_one.clnd, .(patent_id), transform, weight= (1/length(patent_id)))
  
  freq.level_one <- count(level_one.clnd, 'level_one', wt_var = "weight")
  View(freq.level_one)
  freq.level_one.srtd <- freq.level_one[order(-freq.level_one$freq), ]
  head(freq.level_one.srtd, 10)
  
  sum.weights <- sum(freq.level_one.srtd$freq)
  freq.level_one.srtd$percentage <- freq.level_one.srtd$freq / sum.weights
  
  top10 <- as.character(freq.level_one.srtd[1:6,1])
  levels(freq.level_one.srtd$level_one) <- gsub (" ", "\n", levels(freq.level_one.srtd$level_one))
  
  g.1 <- ggplot(data=freq.level_one.srtd[1:10,] , aes(x=reorder(level_one,-percentage), y=percentage, fill=percentage)) + 
    geom_bar(stat="identity") + xlab ("US Federal Department or Agency") + ylab("Weighted Percentage") + 
    scale_fill_gradient(low="#00CCFF", high="#0099FF", guide=FALSE) + scale_y_continuous(labels = scales::percent)
  g.1
  ggsave (paste0("out\\top10LevelOneOrgs_v", script_v, "_", i, ".png"), device = "png")
  
  # longitudinal 
  merged.1a <- merge (level_one.clnd, in.patent_level, by="patent_id")
  colnames(merged.1a)
  hold.1a <- merged.1a[which( !is.na(merged.1a$year) & merged.1a$year <= 2015),c(2:3, 8)]
  head(hold.1a)
  count.1a <- count(hold.1a, vars = c("level_one", "year"), wt_var = "weight")
  head(count.1a)
  keep.1a <- count.1a[which(count.1a$level_one %in% top10), ]
  head(keep.1a)
  
  g.1l <- ggplot(keep.1a, aes(x = year, y = freq, colour=level_one, group = level_one), size=1) + 
    geom_line() + geom_point() +
    ylab(label="Weighted count") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_rect(size = 5),
          legend.key.size = unit(1.5, 'lines'),
          legend.title=element_blank()) 
  g.1l
  ggsave (paste0("out\\longLevelOneOrgs_v", script_v, "_", i, ".png"), device = "png")
  
}

# 2. plot the most frequently observed values @ name
for (i in 0:2) {
  if (i < 2) {
    org_name <- in.gov_level[which(in.gov_level$in_gi==i),c(1,2)] # need to collapse on org name to remove within group dups 
  }
  else {
    org_name <- in.gov_level[,c(1,2)] # need to collapse on org name to remove within group dups 
  }
  
  org_name.clnd <- org_name[!duplicated(org_name),]
  View(org_name.clnd)
  org_name.clnd <- ddply (org_name.clnd, .(patent_id), transform, weight= (1/length(patent_id)))
  colnames(org_name.clnd)
  
  freq.name <- count(org_name.clnd, 'name', wt_var = "weight") # name is already unique per government organization identification process, so don't collapse
  View(freq.name)
  freq.name.srtd <- freq.name[order(-freq.name$freq), ]
  View(freq.name.srtd)
  
  sum.weights <- sum(freq.name.srtd$freq)
  freq.name.srtd$percentage <- freq.name.srtd$freq / sum.weights
  
  top10 <- as.character(freq.name.srtd[1:6,1])
  levels(freq.name.srtd$name) <- gsub (" ", "\n", levels(freq.name.srtd$name))
  
  g.2 <- ggplot(data=freq.name.srtd[1:10,] , aes(x=reorder(name,-percentage), y=percentage, fill=percentage)) + 
    geom_bar(stat="identity") + xlab ("US Federal Department or Agency") + ylab("Weighted Percentage") + 
    scale_fill_gradient(low="#00CC99", high="#009999", guide=FALSE) + scale_y_continuous(labels = scales::percent)
  g.2
  ggsave (paste0("out\\top10OrgNames_v", script_v, "_", i, ".png"), device = "png")
  
  
  # longitudinal 
  merged.2a <- merge (org_name.clnd, in.patent_level, by="patent_id")
  colnames(merged.2a)
  # merged.2a$year <- as.numeric(as.character(merged.2a$year))
  hold.2a <- merged.2a[which( !is.na(merged.2a$year) & merged.2a$year <= 2015),c(2:3, 8)]
  head(hold.2a)
  count.2a <- count(hold.2a, vars = c("name", "year"), wt_var = "weight")
  head(count.2a)
  keep.2a <- count.2a[which(count.2a$name %in% top10), ]
  head(keep.2a)
  
  g.2l <- ggplot(keep.2a, aes(x = year, y = freq, colour=name, group = name), size=1) + 
    geom_line() + geom_point() +
    ylab(label="Weighted count") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_rect(size = 5),
          legend.key.size = unit(1.5, 'lines'),
          legend.title=element_blank()
    )
  g.2l
  ggsave (paste0("out\\longOrgNames_v", script_v, "_", i, ".png"), device = "png")
}

# 5. patent classifications -- WIPO fields (below sectors).  TODO: Are there multiple fields?  Which one did we
# check a patent with orgs identified by gi and assignee
in.gov_level[which(in.gov_level$patent_id=='3930449'), ]

for (i in 0:2) {
  if (i < 2) {
    in.patent_level.sub <- in.patent_level.merged[which(in.patent_level.merged$avg_gi==i),c(1,5,6)] # subset on whether a patent only has gi or assignee orgs
  }
  else {
    in.patent_level.sub <- in.patent_level.merged[which(in.patent_level.merged$avg_gi>0 & in.patent_level.merged$avg_gi<1),c(1,5,6)] 
  }
  
  in.patent_level.sub <- in.patent_level.sub[which(in.patent_level.sub$year >= 1980 & in.patent_level.sub$year <= 2015), ]
  
  colnames(in.patent_level.sub)
  freq.wipo_field <- count(in.patent_level.sub, 'wipo_field')
  freq.wipo_field.nn <- subset (freq.wipo_field, wipo_field != "NULL")
  head(freq.wipo_field.nn)
  freq.wipo_field.srtd <- freq.wipo_field.nn[order(-freq.wipo_field.nn$freq), ]
  head(freq.wipo_field.srtd)
  top10 <- as.character(freq.wipo_field.srtd[1:6,1])
  top10 
  
  sum.weights <- sum(freq.wipo_field.srtd$freq)
  freq.wipo_field.srtd$percentage <- freq.wipo_field.srtd$freq / sum.weights
  levels(freq.wipo_field.srtd$wipo_field) <- gsub (" ", "\n", levels(freq.wipo_field.srtd$wipo_field))
  
  
  g.5 <- ggplot(data=freq.wipo_field.srtd[1:10,] , aes(x=reorder(wipo_field,percentage), y=percentage, fill=percentage)) + 
    geom_bar(stat="identity") + xlab ("WIPO Field") + ylab("Weighted Percentage") + 
    scale_fill_gradient(low="#918DBC", high="#5344D7", guide=FALSE) + scale_y_continuous(labels = scales::percent) + coord_flip()
  g.5
  ggsave (paste0("out\\topWIPOFields_v", script_v, "_", i, ".png"), device = "png")
  
  # longitudinal 
  long.5a <- count(in.patent_level.sub, vars = c("wipo_field", "year"))
  keep.5a <- long.5a[which(long.5a$wipo_field %in% top10), ]
  head(keep.5a)
  
  g.5l <- ggplot(keep.5a, aes(x = year, y = freq, colour=wipo_field, group = wipo_field), size=1) + 
    geom_line() + geom_point() + 
    ylab(label="Frequency") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_rect(size = 5),
          legend.key.size = unit(1.5, 'lines'),
          legend.title=element_blank()
    )
  g.5l
  ggsave (paste0("out\\longWipoFields_v", script_v, "_", i, ".png"), device = "png")
}

# 8. number of government orgs per patent by year analysis #longitudinal 
# a. first count patents by level_one occurrences
# b. then merge in with patent level data to get the year
# c. finally viz the data in line graph mode
# d. a quick modeling analysis 

for (i in 0:1) {
  # a.
  level_one <- in.gov_level[which(in.gov_level$in_gi==i),c(1,3)] # need to collapse on level one orgs to remove within group dups at the parent agency level
  level_one.clnd <- level_one[!duplicated(level_one),]
  View(level_one.clnd)
  freq.patent_id.orgs <- count(level_one.clnd, 'patent_id')
  View(freq.patent_id.orgs)
  freq.patent_id.orgs.srtd <- freq.patent_id.orgs[order(-freq.patent_id.orgs$freq), ]
  View(freq.patent_id.orgs.srtd)
  
  # b. 
  merged_on.pid <- merge (in.patent_level, freq.patent_id.orgs.srtd, by="patent_id")
  colnames(merged_on.pid)[ncol(merged_on.pid)] <- "num_govint"
  # merged_on.pid$year <- as.numeric(as.character(merged_on.pid$year))
  unique(merged_on.pid$year)
  merged_on.pid.clnd <- subset(merged_on.pid, !is.na(year) & year <= 2015)
  unique(merged_on.pid.clnd$year)
  tmp <- count(merged_on.pid.clnd[,c(6,16)])
  
  # c. 
  g.8 <- ggplot(tmp, aes(x = year, y = freq, colour=factor(num_govint), group = num_govint), size=1) + 
    geom_point() + geom_line() +
    ylab(label="Frequency") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) 
  g.8
  ggsave (paste0("out\\coFundingSeries_v", script_v, "_", i, ".png"), device = "png")
  
  # d. 
  m <- glm(num_govint ~ year, family="poisson", data = merged_on.pid.clnd)
  summary(m)
  freq.num_govint <- count(merged_on.pid.clnd, 'num_govint')
  freq.num_govint
  
}


# 9. graph R&D expenditures over time (line) 
in.fund <- read.csv("USFund1_0.csv", header = TRUE)
in.fund[1:10,]
colnames(in.fund)
in.fund$FederalIndex <- in.fund$Federal / in.fund[in.fund$Year==1980, which(colnames(in.fund)=="Federal")] * 100
g.9 <- ggplot(data=in.fund, aes(x=Year, y=Federal, group=1)) + geom_line(size=1.5, color="#0066CC") + 
  xlab ("Year") + ylab("Federal Expenditures (in billions of dollars)") + scale_y_continuous(labels = scales::dollar)
g.9

# 10. let's look at the increase in all patents over the years
in.all <- read.csv("patent_level_all.csv", header = TRUE)
in.all[1:10,]
nrow(subset(in.all, year <= 2015 & year >= 1980))
colnames(in.all)
freq.all.by_year <- count(in.all, 'year')
freq.all.by_year
freq.all.by_year$freqIndex <- freq.all.by_year$freq / freq.all.by_year[freq.all.by_year$year=="1980", which(colnames(freq.all.by_year)=="freq")] * 100
freq.all.by_year <- subset(freq.all.by_year, year <= 2015 & year >= 1980)

g.10 <- ggplot(data=freq.all.by_year, aes(x=year, y=freq,group=1)) + 
  geom_line(size=1.5, color="#EC7C27") + xlab ("Number of patents") + ylab("Count") + 
  scale_y_continuous(labels = comma)
g.10

# 11a. increase in GI patents over the years
freq.gi.by_year <- count(in.patent_level.merged[which (in.patent_level.merged$avg_gi > 0),], 'year')
unique(freq.gi.by_year$year)
freq.gi.by_year.clnd <- subset(freq.gi.by_year, !is.na(year) & year <= 2015 & year >= 1980)
head(freq.gi.by_year.clnd)
freq.gi.by_year.clnd$freqIndex <- freq.gi.by_year.clnd$freq / freq.gi.by_year.clnd[freq.gi.by_year.clnd$year=="1980", which(colnames(freq.gi.by_year.clnd)=="freq")] * 100

unique(freq.gi.by_year.clnd$year)
g.11a <- ggplot(data=freq.gi.by_year.clnd, aes(x=year, y=freq,group=1)) + 
  geom_line(size=1.5, color="#EC272a") + xlab ("Number of patents") + ylab("Count") + 
  scale_y_continuous(labels = comma)
g.11a

# 11b. gov assignees over the years
freq.ga.by_year <- count(in.patent_level.merged[which (in.patent_level.merged$avg_gi < 1),], 'year')
unique(freq.ga.by_year$year)
freq.ga.by_year.clnd <- subset(freq.ga.by_year, !is.na(year) & year <= 2015 & year >= 1980)
head(freq.ga.by_year.clnd)
freq.ga.by_year.clnd$freqIndex <- freq.ga.by_year.clnd$freq / freq.ga.by_year.clnd[freq.ga.by_year.clnd$year=="1980", which(colnames(freq.ga.by_year.clnd)=="freq")] * 100
head(freq.ga.by_year.clnd)

unique(freq.gi.by_year.clnd$year)
g.11b <- ggplot(data=freq.ga.by_year.clnd, aes(x=year, y=freq,group=1)) + 
  geom_line(size=1.5, color="#EC272a") + xlab ("Number of patents") + ylab("Count") + 
  scale_y_continuous(labels = comma)
g.11b

# export select datasets
write.csv (freq.all.by_year, file="out\\freq.all.by_year.csv")
write.csv (freq.gi.by_year, file="out\\freq.gi.by_year.csv")
write.csv (freq.ga.by_year, file="out\\freq.ga.by_year.csv")

# 12. create index graphs across R&D expenditures, all patents, and gi patents
merged.ind1 <- merge (freq.gi.by_year.clnd, freq.all.by_year , by="year", suffixes=c("_gi", "_all"))
merged.ind2 <- merge (merged.ind1, in.fund, by.x="year", by.y = "Year")
merged.ind3 <- merge (freq.ga.by_year.clnd, merged.ind2 , by="year", suffixes=c("_ga", "_m2"))
colnames(merged.ind3)
melt.ind <- melt(merged.ind3, id="year", measure.vars = c(3,5,7,14))
colnames(melt.ind)
View(melt.ind )

g.12 <- ggplot(melt.ind) + 
  geom_line(aes(x = year, y = value, colour=variable), size=1.1) + 
  ylab(label="Indexed value") +  xlab("Year") +
  scale_colour_manual(values=c("blue","dark blue", "purple", "dark green"), labels=c("Government assigned patents", "Government interest patents", "All patents", "R&D funding")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.12


# how many gi patents are there? TODO: fix this code.  Need to make merged.new look like merged
# See  in.patent_level.merged[which(in.patent_level.merged$patent_id == '8435507'), ]
in.patent_level.merged.new <- merge(in.patent_level, in.gov_level, by="patent_id") # check 4180935
dd123 <- ddply (in.patent_level.merged.new, .(patent_id), transform, avg_gi = mean(in_gi))
length(unique(in.patent_level.merged[which(in.patent_level.merged$year >= 1980 & in.patent_level.merged$year <= 2015 & in.patent_level.merged$avg_gi > 0),1]))


# 15. Determine the number of co-funders in cross-tab format (for Amanda) 
level_one <- in.gov_level[which(in.gov_level$in_gi==1),c(1,3)] # need to collapse on level one orgs to remove within group dups at the parent agency level
level_one.clnd <- level_one[!duplicated(level_one),]
View(level_one.clnd)
level_one.table <- table (level_one.clnd)
level_one.cp <- crossprod(level_one.table)

write.csv (level_one.cp, file="out\\level_once.cp.csv")

# 16. load assignees, run descriptives on the top 10, create long view
in.assignees.all <- read.csv("assignees.csv", header = TRUE, stringsAsFactors = FALSE) # note this is all assignees! 
in.assignees <- in.assignees.all[which(in.assignees.all$patent_id %in% patents.keep_ids), ]
View (in.assignees)
colnames(in.assignees)
in.assignees$name <- paste (in.assignees$name_first, in.assignees$name_last, sep = " ")
in.assignees$entity <- in.assignees$organization
in.assignees$entity[which(in.assignees$entity == "")] <- in.assignees$name[which(in.assignees$entity == "")]

assignees.clnd <- in.assignees[,c(1,2,8)]
assignees.clnd <- ddply (assignees.clnd, .(patent_id), transform, weight= (1/length(patent_id)))
head(assignees.clnd)

assignees.merged <- merge (assignees.clnd, in.patent_level.merged, by="patent_id")
assignees.merged$entity <- toTitleCase(tolower(assignees.merged$entity))
head(assignees.merged[order(assignees.merged$weight),])

for (i in 0:2) {
  if (i < 2) {
    assignees.merged.sub <- assignees.merged[which(assignees.merged$avg_gi==i),c(1:4,9)] # subset on whether a patent only has gi or assignee orgs
  }
  else {
    assignees.merged.sub <- assignees.merged[which(assignees.merged$avg_gi>0 & assignees.merged$avg_gi<1),c(1:4,9)] 
  }

  
  colnames(assignees.merged.sub)
  freq.assignees <- count(assignees.merged.sub, 'entity', wt_var = "weight")
  head(freq.assignees)
  freq.assignees.srtd <- freq.assignees[order(-freq.assignees$freq), ]
  head(freq.assignees.srtd)
  top10 <- freq.assignees.srtd[1:6,1]
  top10 # toTitleCase(tolower(top10))
  
  sum.weights <- sum(freq.assignees.srtd$freq)
  freq.assignees.srtd$percentage <- freq.assignees.srtd$freq / sum.weights
  
  if ( i != 1) {
    freq.assignees.srtd$entity <- gsub ("([^ ]+ [^ ]+ [^ ]+ [^ ]+ [^ ]+) ", "\\1\n", freq.assignees.srtd$entity)
  }
  else {
    freq.assignees.srtd$entity <- gsub ("([^ ]+ [^ ]+ [^ ]+) ", "\\1\n", freq.assignees.srtd$entity)
  }
  
  g.16 <- ggplot(data=freq.assignees.srtd[1:10,] , aes(x=reorder(entity,percentage), y=percentage, fill=percentage)) + 
    geom_bar(stat="identity") + xlab ("Assignee") + ylab("Weighted Percentage") + 
    scale_fill_gradient(low="#003333", high="#00CCCC", guide=FALSE) + scale_y_continuous(labels = scales::percent) + coord_flip()
  g.16
  ggsave (paste0("out\\assignee_v", script_v, "_", i, ".png"), device = "png")
  
  # longitudinal 
  long.16a <- count(assignees.merged.sub, vars = c("entity", "year"))
  keep.16a <- long.16a[which(long.16a$entity %in% top10), ]
  head(keep.16a)
  
  keep.16a$entity <- gsub ("([^ ]+ [^ ]+ [^ ]+ [^ ]+ [^ ]+) ", "\\1\n", keep.16a$entity)

  g.16l <- ggplot(keep.16a, aes(x = year, y = freq, colour=entity, group = entity), size=1) + 
    geom_line() + geom_point() + 
    ylab(label="Frequency") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) +
    theme(legend.direction = 'horizontal', 
          legend.position = 'bottom',
          legend.key = element_rect(size = 5),
          legend.key.size = unit(1.5, 'lines'),
          legend.title=element_blank()
    )
  g.16l
  ggsave (paste0("out\\longAssignees_v", script_v, "_", i, ".png"), device = "png")
}

# 17. run sankey viz for funders --> assignees. Note: can run for org_name or level_one orgs
assignees.merged.sub <- assignees.merged[which(assignees.merged$avg_gi==1),c(1:4,9)] # subset on whether a patent only has gi or assignee org
org_name <- in.gov_level[which(in.gov_level$in_gi==1),c(1,2)] # need to collapse on org name to remove within group dups 
org_name.clnd <- org_name[!duplicated(org_name),]

level_one <- in.gov_level[which(in.gov_level$in_gi==1),c(1,3)] # need to collapse on level one orgs to remove within group dups at the parent agency level
level_one.clnd <- level_one[!duplicated(level_one),]

head(assignees.merged.sub)
head(org_name)

# ass_org.merged <- merge (assignees.merged.sub, org_name.clnd, by="patent_id")
ass_org.merged <- merge (assignees.merged.sub, level_one.clnd, by="patent_id")
head(ass_org.merged[order(ass_org.merged$patent_id),])

ass_org.merged.ratio <- ddply (ass_org.merged, .(patent_id), transform, weight= (1/length(patent_id)))
head(ass_org.merged.ratio[order(ass_org.merged.ratio$patent_id),])

# count.ass_org <- count(ass_org.merged.ratio, vars = c("entity", "name"), wt_var = "weight")
count.ass_org <- count(ass_org.merged.ratio, vars = c("entity", "level_one"), wt_var = "weight")
count.ass_org.srtd <- count.ass_org[order(-count.ass_org$freq),]
colnames(count.ass_org.srtd)

top.rows <- 30
count.ass_org.srtd <- count.ass_org.srtd[1:top.rows,]
count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the United States Department of Energy", count.ass_org.srtd$entity), ]
count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Administrator of the National Aeronautics and Space Administration", count.ass_org.srtd$entity), ]
count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Secretary of the Navy", count.ass_org.srtd$entity), ]
View(count.ass_org.srtd)

top.rows <- nrow(count.ass_org.srtd)
top.funders <- unique(count.ass_org.srtd[1:top.rows,2])
top.assignees <- unique(count.ass_org.srtd[1:top.rows,1])
nodes = c (top.funders, top.assignees)

funders.links <- sapply(1:length(top.funders), function(x) grep(nodes[x], count.ass_org.srtd[,2]))
fl.df <- data.frame(ID = rep(seq(funders.links), sapply(funders.links, length)), Obs = unlist(funders.links))
fl.df[, 1] <- fl.df[,1] - 1
count.ass_org.srtd$source <- fl.df[order(fl.df$Obs), 1]

assignees.links <- sapply((length(top.funders)+1):length(nodes), function(x) grep(nodes[x], count.ass_org.srtd[,1]))
al.df <- data.frame(ID = rep(seq(assignees.links), sapply(assignees.links, length)), Obs = unlist(assignees.links))
al.df[,1] <- al.df[,1] + length(top.funders) - 1 # offset
count.ass_org.srtd$target <- al.df[order(al.df$Obs), 1]

View(count.ass_org.srtd)

p.sk <- plot_ly(
  type = "sankey",
  domain = c(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  
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
    title = "Top Government Interest Organizations to Assignees, 1980 - 2015",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
  )

p.sk
export(p.sk, file = paste0("out\\assignees\\Sankey_", script_v, "_", ".png"))
htmlwidgets::saveWidget(p.sk, file = paste0("F:\\Govt_Int\\Final_CSVS\\out\\assignees\\Sankey org name_", script_v, "_", ".html"))

# 18. merge sector type for assignees and create stacked bar chart
in.sector <- read.csv("assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = FALSE)
head(in.sector)
in.sector$organization <- trimws(in.sector$organization)

ass_sector_org.merged <-  merge (ass_org.merged, in.sector, by="patent_id")
head(ass_sector_org.merged)
ass_sector_org.merged <- ass_sector_org.merged[,c(1,2,5, 6, 8, 9, 10)]
head(ass_sector_org.merged)

ass_sector_org.merged.ratio <- ddply (ass_sector_org.merged, .(patent_id), transform, weight= (1/length(patent_id)))
head(ass_sector_org.merged.ratio)

count.ass_sector_org <- count(ass_sector_org.merged.ratio, vars = c("level_one", "thes_types"), wt_var = "weight")
count.ass_sector_org.short <- count.ass_sector_org[which(count.ass_sector_org$level_one %in% top10), ]
count.ass_sector_org.short <- count.ass_sector_org.short[which(count.ass_sector_org.short$level_one != "United States Government" ) ,]
count.ass_sector_org.short$level_one <- gsub ("([^ ]+ [^ ]+) ", "\\1\n", count.ass_sector_org.short$level_one)
head(count.ass_sector_org.short)

g.18 <- ggplot() + 
  geom_bar(aes(y = freq, x = reorder(level_one, -freq), fill = thes_types), data = count.ass_sector_org.short, stat="identity") + 
  ylab(label="Number of Patents") +  xlab("US Federal Department or Agency") +
  scale_y_continuous(label=comma) + 
  scale_fill_discrete(name="Sector")
g.18
ggsave (paste0("out\\funders\\funders-assignees.stacked_", script_v, ".png"), device = "png")

# 19a. Number of new entrants over time by sector
in.firstdate <- read.csv("assignee_first_patent_date.csv", header = TRUE, stringsAsFactors = FALSE)
in.patentdate <- read.csv("patent_date.csv", header = TRUE, stringsAsFactors = FALSE)
in.patentdate.keep <- in.patentdate[which(in.patentdate$id %in% patents.keep_ids), ]

head(in.firstdate)
head(assignees.merged)

firstdate.merged <- merge (in.firstdate, assignees.merged, by.x="assignee_id", by.y = "id")
colnames (firstdate.merged)
colnames (in.patentdate.keep)
firstdate.patentdate.merged <- merge (in.patentdate.keep, firstdate.merged, by.x = "id", by.y="patent_id")
  
# 5834840, 3930295 is a patent_id with two assignees

firstdate.patentdate.merged$entry <- 0
firstdate.patentdate.merged$entry[which (firstdate.patentdate.merged$date == firstdate.patentdate.merged$to_date)] <- 1
View(firstdate.patentdate.merged)

new_entrants.merged <-  merge (firstdate.patentdate.merged, in.sector, by.x = "id", by.y="patent_id")
colnames(new_entrants.merged)
new_entrants.merged.small <- new_entrants.merged[which(new_entrants.merged$entry == 1),c(1:7, 12, 23, 27)]
head(new_entrants.merged.small)

count.new_entrants <- count(new_entrants.merged.small, vars = c("thes_types", "year"), wt_var = "entry")
colnames(count.new_entrants)



g.19a <- ggplot(count.new_entrants, aes(x = year, y = freq, colour=thes_types, group = thes_types)) + 
  geom_line(size=1.2) + 
  ylab(label="Number of New Patentee Entrants Receiving Government Funding") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.19a
ggsave (paste0("out\\entrants\\newEntrants_", script_v, ".png"), device = "png")

# 19b. Also check to see the number of new entrants over time across all patents
in.all.assignees_to_patents <- read.csv("assignee_id.to.patent_date.csv", header = TRUE, stringsAsFactors = FALSE)
all.firstdate.merged <- merge (in.firstdate, in.all.assignees_to_patents, by="assignee_id") # assignee info
firstdate.patentdate_all.merged <- merge (in.patentdate, all.firstdate.merged, by = "id")

firstdate.patentdate_all.merged$year <- substr(firstdate.patentdate_all.merged$date.x, 1, 4)
firstdate.patentdate_all.merged$year <- as.numeric(firstdate.patentdate_all.merged$year)
firstdate.patentdate_all.small <- subset(firstdate.patentdate_all.merged, year >= 1980 & year <= 2015)

firstdate.patentdate_all.small$entry <- 0
firstdate.patentdate_all.small$entry[which (firstdate.patentdate_all.small$date.x == firstdate.patentdate_all.small$to_date)] <- 1
head(firstdate.patentdate_all.small)

count.new_entrants.all <- count (firstdate.patentdate_all.small, vars = "year", wt_var = "entry")
freq.all.by_year

merged.entrants1 <- merge(count.new_entrants.all, freq.all.by_year, by="year")
merged.entrants1$percentAll <- merged.entrants1$freq.x / merged.entrants1$freq.y

count.new_entrants
freq.gi.by_year.clnd

merged.entrants2 <- merge(count.new_entrants, freq.gi.by_year.clnd, by="year")
merged.entrants2$percentGI <- merged.entrants2$freq.x / merged.entrants2$freq.y

g.19b <- ggplot() + 
  geom_bar(aes(y = percentGI, x = year, fill = thes_types), data = merged.entrants2, stat="identity") + 
  ylab(label="Percentage of Patents") +  xlab("Year") +
  scale_y_continuous(label=comma) + 
  scale_fill_discrete(name="Sector of\nAssignee") +
  geom_line(data=merged.entrants1, aes(x=year, y=percentAll), size=1.2) 
g.19b
ggsave (paste0("out\\entrants\\entrantsBySector.compare_", script_v, ".png"), device = "png")

count.new_entrants.year <- count(new_entrants.merged.small, vars = c("year"), wt_var = "entry")
merged.tmp <- merge(count.new_entrants.year, freq.gi.by_year.clnd, by="year")
merged.tmp$percentGI <- merged.tmp$freq.x / merged.tmp$freq.y
merged.tmp

# 20. Number of new entrants over time by funder
colnames(new_entrants.merged.small)[1] <- "patent_id" 
new_entrants.funded.merged <- merge (new_entrants.merged.small, ass_org.merged, by = "patent_id")
colnames(new_entrants.funded.merged)                                               
new_entrants.funded.small <- new_entrants.funded.merged[,c(15, 8, 9)]
head(new_entrants.funded.small)

count.new_entrants.funded <- count(new_entrants.funded.small, vars = c("level_one", "year.x"), wt_var = "entry")
colnames(count.new_entrants.funded)

count.new_entrants.funded.short <- count.new_entrants.funded[which(count.new_entrants.funded$level_one %in% top10), ]
count.new_entrants.funded.short <- count.new_entrants.funded.short[which(count.new_entrants.funded.short$level_one != "United States Government" ) ,]
count.new_entrants.funded.short$level_one <- gsub ("([^ ]+ [^ ]+) ", "\\1\n", count.new_entrants.funded.short$level_one)
head(count.new_entrants.funded.short)


g.20 <- ggplot(count.new_entrants.funded.short, aes(x = year.x, y = freq, colour=level_one, group = level_one)) + 
  geom_line(size=1.2) + 
  ylab(label="Number of New Patentee Entrants Receiving Government Funding") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.20
ggsave (paste0("out\\entrants\\newEntrantsByFunder_", script_v, ".png"), device = "png")

# 21. Citation analysis.  Plot distributions of citations: all patents vs. three sectors from GI
# SQL is available at create_distance_metrics.sql in code dir
# Python code for sector assignements for assignees is in code dir: Regex_dev.ipynb

head(ass_sector_org.merged)
university.patents <- unique(ass_sector_org.merged[which(ass_sector_org.merged$thes_types == "Academic"),1])
corporate.patents <- unique(ass_sector_org.merged[which(ass_sector_org.merged$thes_types == "Corporate"),1])
government.patents <- unique(ass_sector_org.merged[which(ass_sector_org.merged$thes_types == "Government"),1])
colnames(in.patent_level)

university.num_citations_in_5yrs <- data.frame(in.patent_level[in.patent_level$patent_id %in% university.patents,15], "university")
colnames (university.num_citations_in_5yrs) <- c("num_citations_in_5yrs", "entity")
corporate.num_citations_in_5yrs <- data.frame(in.patent_level[in.patent_level$patent_id %in% corporate.patents,15], "corporate")
colnames (corporate.num_citations_in_5yrs) <- c("num_citations_in_5yrs", "entity")
government.num_citations_in_5yrs <- data.frame(in.patent_level[in.patent_level$patent_id %in% government.patents,15], "government")
colnames (government.num_citations_in_5yrs) <- c("num_citations_in_5yrs", "entity")

num_citations_in_5yrs <- rbind (university.num_citations_in_5yrs, corporate.num_citations_in_5yrs, government.num_citations_in_5yrs)

colnames(in.all)
nrow(in.all)
in.non_GI.patent_cites <- read.csv("non-GI_patent_citations.csv", header = TRUE, stringsAsFactors = FALSE)
nrow(in.non_GI.patent_cites)
colnames(in.non_GI.patent_cites)


non_GI.cites.merged <- merge (in.non_GI.patent_cites,in.all[,c(1,7)], by="patent_id", all.y = TRUE)
non_GI.cites.merged[which(is.na(non_GI.cites.merged$num_citations_in_5yrs)),2] <- 0
nrow(non_GI.cites.merged)
head(non_GI.cites.merged)

g21a <- ggplot(non_GI.cites.merged) + 
  geom_histogram(aes(x=num_citations_in_5yrs, ..density..),  binwidth=1, alpha=.5) + 
  scale_x_reverse() + coord_cartesian(xlim = c(-1, 20)) + ylim(0,.42) +
  ylab(label="Density") +  xlab("Citations - All Patents") +
  geom_vline(aes(xintercept=mean(num_citations_in_5yrs, na.rm=T)), color="black", linetype="dashed", size=1) 
g21a

g21b <- ggplot(num_citations_in_5yrs) + 
  geom_histogram(data=subset(num_citations_in_5yrs,entity == 'university'), aes(x=num_citations_in_5yrs, ..density..), alpha=.5, fill = "red", binwidth = 1) +
  xlim(-1,20) + ylim(0,.42) + 
  ylab(label="") +  xlab("Citations - University Assigned with Government Interest") + 
  geom_vline(aes(xintercept=mean(subset(num_citations_in_5yrs,entity == 'university'), na.rm=T)), color="black", linetype="dashed", size=1) 
g21b

g21c <- ggplot(num_citations_in_5yrs) + 
  geom_histogram(data=subset(num_citations_in_5yrs,entity == 'corporate'), aes(x=num_citations_in_5yrs, ..density..), alpha=.5, fill = "blue", binwidth = 1) +
  scale_x_reverse() + coord_cartesian(xlim = c(-1, 20))  + ylim(0,.42) +
  ylab(label="Density") +  xlab("Citations - Corporate Assigned with Government Interest") + 
  geom_vline(aes(xintercept=mean(subset(num_citations_in_5yrs,entity == 'corporate'), na.rm=T)), color="black", linetype="dashed", size=1) 
g21c

g21d <- ggplot(num_citations_in_5yrs) + 
  geom_histogram(data=subset(num_citations_in_5yrs,entity == 'government'), aes(x=num_citations_in_5yrs, ..density..), alpha=.5, fill = "green", binwidth = 1) +
  xlim(-1,20) + ylim(0,.42) + 
  ylab(label="") +  xlab("Citations - Government Assigned") +
  geom_vline(aes(xintercept=mean(subset(num_citations_in_5yrs,entity == 'government'), na.rm=T)), color="black", linetype="dashed", size=1) 
g21d

grid.arrange(g21a, g21b, g21c, g21d, ncol=2)
ggsave (paste0("out\\citations\\citationDistribution_", script_v, ".png"), arrangeGrob(g21a, g21b, g21c, g21d), device = "png")

mean(non_GI.cites.merged$num_citations_in_5yrs)
mean(subset(num_citations_in_5yrs,entity == 'university')[,1])
mean(subset(num_citations_in_5yrs,entity == 'corporate')[,1])
mean(subset(num_citations_in_5yrs,entity == 'government')[,1])

# 22. Citation boundary analysis
in.boundary <- read.csv("assignee_distance_by_type.csv", header = TRUE, stringsAsFactors = FALSE)


head(in.sector)
head(in.boundary)
merged.boundary <- merge (in.sector, in.boundary, by="patent_id")
colnames(merged.boundary)

merged.boundary[merged.boundary$has_citations == 0, 9] <- "No citations"
# merged.boundary.short <- merged.boundary[which(merged.boundary$has_citations == 1), c(5,9)]
merged.boundary.short <- merged.boundary[, c(5,9)]
head(merged.boundary.short)
boundary.count <- count (merged.boundary.short,  vars = c("thes_types", "degrees"))
boundary.count[boundary.count$degrees == "More than 3", 2] <- "3 or more"

boundary.count$degrees <- factor(boundary.count$degrees, levels = c("No citations", "1", "2", "3 or more"))
colnames(boundary.count)
unique(boundary.count$degrees)

g22 <- ggplot(boundary.count,aes(x = thes_types, y = freq, fill = forcats::fct_rev(degrees))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  coord_flip() + xlab(label="Sector of Government Interest Assignee") +  ylab("Number of Patents") + 
  scale_fill_manual(values=c("blue", "cyan", "light blue", "grey")) +
  guides(fill=guide_legend(title="Proximity to\nPatent-Patent\nCitation Boundary"))
g22
ggsave (paste0("out\\citations\\nonGIFirmCitations_toGISectors_", script_v, ".png"), device = "png")


# 23 (13-2). inventor and assignees by year (update: just inventors)
patent_level.bkp <- in.patent_level
colnames(patent_level.bkp)
patent_level.bkp$year <- as.numeric(as.character(patent_level.bkp$year))
# patent_level.bkp$num_times_cited_by_us_patents <- as.numeric(as.character(patent_level.bkp$num_times_cited_by_us_patents))
patent_level.bkp$num_us_applications_cited <- as.numeric(as.character(patent_level.bkp$num_us_applications_cited))
patent_level.bkp$num_foreign_documents_cited <- as.numeric(as.character(patent_level.bkp$num_foreign_documents_cited))
patent_level.bkp$num_us_patents_cited <- as.numeric(as.character(patent_level.bkp$num_us_patents_cited))
gi.agg <- aggregate(patent_level.bkp[,c(2,3)], list(Year = patent_level.bkp$year), na.rm=TRUE, mean)

all.bkp <- in.all 
colnames (all.bkp)

all.bkp$year <- as.numeric(as.character(all.bkp$year))
all.bkp$num_times_cited_by_us_patents <- as.numeric(as.character(all.bkp$num_times_cited_by_us_patents))
all.bkp$num_us_applications_cited <- as.numeric(as.character(all.bkp$num_us_applications_cited))
all.bkp$num_foreign_documents_cited <- as.numeric(as.character(all.bkp$num_foreign_documents_cited))
all.bkp$num_us_patents_cited <- as.numeric(as.character(all.bkp$num_us_patents_cited))
all.agg <- aggregate(all.bkp[,c(2,3)], list(Year = all.bkp$year), na.rm=TRUE, mean)

# gi sector detail
university.num_inventors <- data.frame(in.patent_level[in.patent_level$patent_id %in% university.patents,c(6,2)], "university")
colnames (university.num_inventors) <- c("year", "num_inventors", "entity")
corporate.num_inventors <- data.frame(in.patent_level[in.patent_level$patent_id %in% corporate.patents,c(6,2)], "corporate")
colnames (corporate.num_inventors) <- c("year", "num_inventors", "entity")
government.num_inventors <- data.frame(in.patent_level[in.patent_level$patent_id %in% government.patents,c(6,2)], "government")
colnames (government.num_inventors) <- c("year", "num_inventors", "entity")

uni.agg <- aggregate(university.num_inventors[,2], list(Year = university.num_inventors$year), na.rm=TRUE, mean)
colnames(uni.agg)[2] <- "mean_num_inventors_uni"
corp.agg <- aggregate(corporate.num_inventors[,2], list(Year = corporate.num_inventors$year), na.rm=TRUE, mean)
colnames(corp.agg)[2] <- "mean_num_inventors_corp"
gov.agg <- aggregate(government.num_inventors[,2], list(Year = government.num_inventors$year), na.rm=TRUE, mean)
colnames(gov.agg)[2] <- "mean_num_inventors_gov"

merged.agg1 <- merge (gi.agg, all.agg, by="Year", suffixes=c("_gi", "_all"))
merged.agg2 <- merge (merged.agg1, uni.agg, by="Year", suffixes=c("_agg1", "_uni"))
merged.agg3 <- merge (merged.agg2, corp.agg, by="Year", suffixes=c("_agg2", "_corp"))
merged.agg <- merge (merged.agg3, gov.agg, by="Year", suffixes=c("_agg3", "_gov"))

head(merged.agg)
View(merged.agg)
colnames(merged.agg)

melt.agg <- melt(merged.agg, id="Year", measure.vars = c(4,2,6:7)) # was c(2:5)
colnames(melt.agg)
head(melt.agg )

g.23 <- ggplot(melt.agg) + 
  geom_line(aes(x = Year, y = value, colour=variable, linetype =variable), size=1.2) + 
  ylab(label="Mean value") +  xlab("Year") +
  scale_colour_manual  (values=c("red","orange", "grey", "dark grey"),  labels=c("Mean inventors\nacross all patents", "Mean government\ninterest inventors", "Mean government\ninterest inventors - universities", "Mean government\ninterest inventors - corporate" )) +
  scale_linetype_manual(values=c("solid", "solid", "twodash", "dotted"), labels=c("Mean inventors\nacross all patents", "Mean government\ninterest inventors", "Mean government\ninterest inventors - universities", "Mean government\ninterest inventors - corporate" )) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.23
ggsave (paste0("out\\inventors\\longInventorWipoFields_v", script_v, ".png"), device = "png")



# 24. look to see if having more than one funder and having more than one assignee (cross sector) correlates with number of inventors 
sectors.tmp <- unique(in.sector[,c(2,5)])
sectors.count <- count(sectors.tmp, 'patent_id')
View(sectors.count)

in.sector[which(in.sector$patent_id == '4393064'), ] 
sectors.tmp[which(sectors.tmp$patent_id == '4393064'), ]

level_one.tmp <- unique(level_one.clnd[,c(1:2)])
level_one.count <- count(level_one.tmp, 'patent_id')
View(level_one.count)

ass_sector_org.merged[which(ass_sector_org.merged$patent_id == '7244498'), ]
level_one.tmp[which(level_one.tmp$patent_id == '7244498'), ]

merge.24a <- merge(in.patent_level.merged, sectors.count, by = "patent_id", all.x = TRUE)
merge.24b <- merge(merge.24a, level_one.count, by = "patent_id", all.x = TRUE)

nrow(merge.24b)

filtered.24 <- merge.24b[which(merge.24b$avg_gi > 0), ]
colnames(filtered.24)[17:18] <- c("num_unique_sectors", "num_unique_funders")
colnames(filtered.24)

print(corr.test(filtered.24[,c(2:3,17:18)], use="pairwise", method="pearson", alpha=.01), short=FALSE)

fill <- "#546F8F"
lines <- "#223B57"

g24a <- ggplot(filtered.24[,c(2,17)], aes(x=num_unique_sectors, y=num_inventors, group=num_unique_sectors)) +
  geom_jitter(colour = "grey", size=.4 ) + 
  geom_boxplot(colour = lines, fill = fill, alpha=.5, size = .8) +
  scale_y_continuous(name = "Number of Inventors",
                     breaks = seq(0, 10, 5),
                     limits=c(0, 10)) +
  scale_x_discrete(name = "Number of Distinct Sectors from Assignees", limits=c("1","2","3","4"), labels=c("1","2","3","4"))
  
fill <- "#7DBAC0"
lines <- "#39767B"

g24b <- ggplot(filtered.24[which(filtered.24$num_unique_funders < 5),c(2,18)], aes(x=num_unique_funders, y=num_inventors, group=num_unique_funders)) +
  geom_jitter(colour = "grey", size=.4 ) + 
  geom_boxplot(colour = lines, fill = fill, alpha=.5, size = .8) +
  scale_y_continuous(name = "Number of Inventors",
                     breaks = seq(0, 10, 5),
                     limits=c(0, 10)) +
  scale_x_discrete(name = "Number of Governent Funders", limits=as.character(c(1:6)), labels=as.character(c(1:6)))

grid.arrange(g24a, g24b, ncol=2)
ggsave (paste0("out\\inventors\\inventorBoxPlots_", script_v, ".png"), arrangeGrob(g24a, g24b), device = "png")


