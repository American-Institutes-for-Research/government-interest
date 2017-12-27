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

setwd("F:\\Govt_Int\\Final_CSVS") 
script_v <- "0.6"

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


# 13. inventor and assignees by year
patent_level.bkp <- in.patent_level
colnames(patent_level.bkp)
patent_level.bkp$year <- as.numeric(as.character(patent_level.bkp$year))
patent_level.bkp$num_times_cited_by_us_patents <- as.numeric(as.character(patent_level.bkp$num_times_cited_by_us_patents))
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

merged.agg <- merge (gi.agg, all.agg, by="Year", suffixes=c("_gi", "_all"))
head(merged.agg)
View(merged.agg)
colnames(merged.agg)

melt.agg <- melt(merged.agg, id="Year", measure.vars = c(2:5))
colnames(melt.agg)
View(melt.agg )

g.13 <- ggplot(melt.agg) + 
  geom_line(aes(x = Year, y = value, colour=variable), size=1.2) + 
  ylab(label="Mean value") +  xlab("Year") +
  scale_colour_manual(values=c("red","#999999","orange", "#333333"), labels=c("Mean government\ninterest inventors", "Mean government\ninterest assignees", "Mean inventors\nacross all patents", "Mean assignees\nacross all patents")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.13

# 14. mean number of inventors for gi patents by top 10 wipo_fields
# need to compute top 10 wipo fields under #5 above (set i <- 1 first)

colnames(in.patent_level.merged)
in.patent_level.merged$wipo_field <- as.character(in.patent_level.merged$wipo_field)
in.patent_level.merged$year <- as.numeric(as.character(in.patent_level.merged$year))
head (in.patent_level.merged)
filtered <- in.patent_level.merged[which (in.patent_level.merged$year >= 1980 & in.patent_level.merged$year <= 2015), ]
in.patent_level.merged.top_wipo <- filtered[which(filtered$wipo_field %in% top10 & filtered$avg_gi == 1), c(5,6,2)]
head(in.patent_level.merged.top_wipo)

inv_wipo.by_year <- aggregate (in.patent_level.merged.top_wipo[,3], by = list (Wipo_Field = in.patent_level.merged.top_wipo$wipo_field, Year = in.patent_level.merged.top_wipo$year), na.rm = TRUE, mean)
head (inv_wipo.by_year)

g.14 <- ggplot(inv_wipo.by_year, aes(x = Year, y = x, colour=Wipo_Field, group = Wipo_Field)) + 
  geom_line(size=1.2) + 
  ylab(label="Mean number of inventors") +  xlab("Year") + scale_x_continuous(breaks=c(1980,1990,2000,2010)) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.14
ggsave (paste0("out\\longOrgNames_v", script_v, "_", i, ".png"), device = "png")

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

# 17. run sankey viz for funders --> assignees 

