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
library(trend)


# import fonts
library(extrafont)
# font_import()
loadfonts(device = "win")

# set color scheme
cyan <- rgb (0, 123, 188,	maxColorValue = 255)
darkRed <- rgb (208, 32, 47, maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (91, 140, 41, maxColorValue=255)
darkPurple <- rgb (135, 30, 110, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (206, 206, 199, maxColorValue=255)

# ['#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac']

setwd("F:\\Govt_Int\\Final_CSVS") 
script_v <- "1.5"

# load patent level data
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)
View(in.patent_level[1:100,])
colnames(in.patent_level)
nrow(in.patent_level)

# filter on years 
in.patent_level.bkp <- in.patent_level
in.patent_level <- subset(in.patent_level, year >= 1980 & year <= 2016)
patents.keep_ids <- in.patent_level$patent_id
length(patents.keep_ids) # 130,557

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

nrow(in.patent_level.merged)
nrow(in.patent_level.merged[in.patent_level.merged$avg_gi > 0, ])
nrow(in.patent_level.merged[in.patent_level.merged$avg_gi == 0, ])

# test this merge looks legit 
View(in.patent_level.merged)
in.patent_level.merged[which(in.patent_level.merged$patent_id=='4180935'), ]
in.gov_level[which(in.gov_level$patent_id=='4180935'),] # 1.0 weight 
in.patent_level.merged[which(in.patent_level.merged$patent_id=='4181062'), ]
in.gov_level[which(in.gov_level$patent_id=='4181062'),] # 0.0 weight 
in.patent_level.merged[which(in.patent_level.merged$patent_id=='4181139'), ]
in.gov_level[which(in.gov_level$patent_id=='4181139'),] # 0.5 weight

# 5. patent classifications -- WIPO fields (below sectors)
in.patent_level.sub <- in.patent_level.merged[which(in.patent_level.merged$avg_gi>0),c(1,5,6)] # subset on whether a patent only has gi or assignee orgs
in.patent_level.sub <- in.patent_level.sub[which(in.patent_level.sub$year >= 2005 & in.patent_level.sub$year <= 2016), ]
  
colnames(in.patent_level.sub)
freq.wipo_field <- count(in.patent_level.sub, 'wipo_field')
freq.wipo_field.nn <- subset (freq.wipo_field, wipo_field != "NULL")
head(freq.wipo_field.nn)
freq.wipo_field.srtd <- freq.wipo_field.nn[order(-freq.wipo_field.nn$freq), ]
head(freq.wipo_field.srtd)
top10 <- as.character(freq.wipo_field.srtd[1:5,1])
top10 

sum.weights <- sum(freq.wipo_field.srtd$freq)
freq.wipo_field.srtd$percentage <- freq.wipo_field.srtd$freq / sum.weights
levels(freq.wipo_field.srtd$wipo_field) <- gsub (" ", "\n", levels(freq.wipo_field.srtd$wipo_field))


g.5 <- ggplot(data=freq.wipo_field.srtd[1:10,] , aes(x=reorder(wipo_field,percentage), y=percentage, fill=percentage)) + 
  geom_bar(stat="identity") + xlab ("WIPO Field") + ylab("Weighted Percentage") + 
  scale_fill_gradient(low="#918DBC", high="#5344D7", guide=FALSE) + scale_y_continuous(labels = scales::percent) + coord_flip()
g.5
ggsave (paste0("out\\topWIPOFields_v", script_v, "_", i, ".png"), device = "png")

# longitudinal (gi_patents)
long.5a <- count(in.patent_level.sub, vars = c("wipo_field", "year"))

# (all patents)
in.all <- read.csv("patent_level_all.csv", header = TRUE)
in.all[1:10,]
in.all.since_2005 <- subset(in.all, year <= 2016 & year >= 2005)
colnames(in.all.since_2005)
freq.wipo_field.all <- count(in.all.since_2005, vars = c("wipo_field"))

# determine overall GI patent ratio vis-a-vis all patents, by wipo class
freq.wipo_field.merge <- merge (freq.wipo_field, freq.wipo_field.all, by="wipo_field")
freq.wipo_field.merge$ratio <-  freq.wipo_field.merge$freq.x / freq.wipo_field.merge$freq.y

long.5b <- count(in.all.since_2005, vars = c("wipo_field", "year"))

merged.long5 <- merge (long.5a, long.5b , by=c("wipo_field","year"), suffixes=c("_5a", "_5b"))
merged.long5$ratio <- merged.long5$freq_5a / merged.long5$freq_5b

# apply trend analysis 
segmented.wipo <- split( merged.long5 , f = merged.long5$wipo_field )
wipo.trend_p <- sapply( segmented.wipo , function(wipo.class) mk.test(wipo.class$ratio)$p.value )
wipo.statistic <- sapply( segmented.wipo , function(wipo.class) mk.test(wipo.class$ratio)$statistic )
wipo.trend_results <- data.frame(wipo.trend_p, wipo.statistic)
wipo.trend_results$wipo_field <- rownames(wipo.trend_results)
View(wipo.trend_results)


colnames(freq.wipo_field.merge)
wipo.merged <- merge(freq.wipo_field.merge[,c(1,4)], wipo.trend_results, by="wipo_field")
View(wipo.merged)
top.growing <- wipo.merged[which(wipo.merged$wipo.statistic > 2) , 1]

# keep only the top ____ wipo_fields, either top occurring (top10) or top growing (top.growing)

keep.5a <- long.5a[which(long.5a$wipo_field %in% top.growing), ] # or in top10
keep.5b <- long.5b[which(long.5b$wipo_field %in% top.growing), ]
keep.5a
keep.5b

merged.keep5 <- merge (keep.5a, keep.5b , by=c("wipo_field","year"), suffixes=c("_5a", "_5b"))
merged.keep5$ratio <- merged.keep5$freq_5a / merged.keep5$freq_5b
# merged.keep5 <- merged.keep5[which(merged.keep5$wipo_field != "Measurement"),]

# percent of GI patents in these four classes as a % of all GI patents 
sum(merged.keep5[merged.keep5$year == 2016, 3]) / freq.gi.by_year[freq.gi.by_year$year == 2016,2]

g.5l <- ggplot(merged.keep5) + 
  geom_line(aes(x = year, y = ratio, colour=wipo_field, group = wipo_field, linetype = wipo_field), size=1.5) + 
  ylab(label="Ratio of government interest patents to all patents") +  xlab("Year") + 
  scale_x_continuous(breaks=c(2005,2010,2015), minor_breaks = seq(2005, 2016, 1)) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  ) 
g.5l
ggsave (paste0("out\\longWipoFields_v", script_v, ".png"), device = "png")


# 9. graph R&D expenditures over time (line) 
in.fund <- read.csv("USFund1_0_imputed.csv", header = TRUE)
in.fund[1:10,]
colnames(in.fund)
in.fund$FederalIndex <- in.fund$Federal / in.fund[in.fund$Year==1980, which(colnames(in.fund)=="Federal")] * 100
g.9 <- ggplot(data=in.fund, aes(x=Year, y=Federal, group=1)) + geom_line(size=1.5, color="#0066CC") + 
  xlab ("Year") + ylab("Federal Expenditures (in billions of dollars)") + scale_y_continuous(labels = scales::dollar)
g.9

# 10. let's look at the increase in all patents over the years
freq.all.by_year <- count(in.all, 'year')
freq.all.by_year
freq.all.by_year$freqIndex <- freq.all.by_year$freq / freq.all.by_year[freq.all.by_year$year=="1980", which(colnames(freq.all.by_year)=="freq")] * 100
freq.all.by_year <- subset(freq.all.by_year, year <= 2016 & year >= 1980)

g.10 <- ggplot(data=freq.all.by_year, aes(x=year, y=freq,group=1)) + 
  geom_line(size=1.5, color="#EC7C27") + xlab ("Number of patents") + ylab("Count") + 
  scale_y_continuous(labels = comma)
g.10

# 11a. increase in GI patents over the years
freq.gi.by_year <- count(in.patent_level.merged[which (in.patent_level.merged$avg_gi > 0),], 'year')
unique(freq.gi.by_year$year)
freq.gi.by_year.clnd <- subset(freq.gi.by_year, !is.na(year) & year <= 2016 & year >= 1980)
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
freq.ga.by_year.clnd <- subset(freq.ga.by_year, !is.na(year) & year <= 2016 & year >= 1980)
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
write.csv (in.patent_level.merged, file="out\\out.patent_level.merged.csv")
write.csv (patents.keep_ids, file="out\\gi_ga.patent_ids.csv")
write.csv (in.patent_level.sub$patent_id, file="out\\gi.patent_ids.csv")

# 12. create index graphs across R&D expenditures, all patents, and gi patents
merged.ind1 <- merge (freq.gi.by_year.clnd, freq.all.by_year , by="year", suffixes=c("_gi", "_all"))
merged.ind2 <- merge (merged.ind1, in.fund, by.x="year", by.y = "Year")
# merged.ind3 <- merge (freq.ga.by_year.clnd, merged.ind2 , by="year", suffixes=c("_ga", "_m2"))
colnames(merged.ind2)
melt.ind <- melt(merged.ind2, id="year", measure.vars = c(3,5,12))
colnames(melt.ind)
View(melt.ind )

g.12 <- ggplot(melt.ind) + 
  geom_line(aes(x = year, y = value, colour=variable, linetype=variable), size=1.5) + 
  ylab(label="Indexed value") +  xlab("Year") +
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
g.12
ggsave (paste0("out\\indexed_", script_v, ".png"), device = "png")


# 13. inventor and assignees by year (update: just inventors)
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

merged.agg <- merge (gi.agg, all.agg, by="Year", suffixes=c("_gi", "_all"))
head(merged.agg)
View(merged.agg)
colnames(merged.agg)

melt.agg <- melt(merged.agg, id="Year", measure.vars = c(2,4)) # was c(2:5)
colnames(melt.agg)
head(melt.agg )

g.13 <- ggplot(melt.agg) + 
  geom_line(aes(x = Year, y = value, colour=variable, linetype=variable), size=1.5) + 
  scale_x_continuous(breaks=c(1980, 1985, 1990,1995,2000,2005,2010,2015)) +
  ylab(label="Mean Value") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkGrey), labels=c("Mean government\ninterest inventors", "Mean inventors\nacross all patents")) +
  scale_linetype_manual(values=c("solid", "twodash"), labels=c("Mean government\ninterest inventors", "Mean inventors\nacross all patents")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(5, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  )
g.13
ggsave (paste0("out\\longInventor_v", script_v, ".png"), device = "png")


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
    title = "Top Government Interest Organizations to Assignees, 1980 - 2016",
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
ass_sector_org.merged <- ass_sector_org.merged[,c(1,2,5,6,9)]
head(ass_sector_org.merged)

ass_sector_org.merged.ratio <- ddply (ass_sector_org.merged, .(patent_id), transform, weight= (1/length(patent_id)))
head(ass_sector_org.merged.ratio)
ass_sector_org.merged.ratio.clnd <- ass_sector_org.merged.ratio
ass_sector_org.merged.ratio.clnd[which(ass_sector_org.merged.ratio.clnd$thes_types == 'Hospital'),  5] <- "Academic"
ass_sector_org.merged.ratio.clnd[which(ass_sector_org.merged.ratio.clnd$thes_types == 'Ambiguous' | ass_sector_org.merged.ratio.clnd$thes_types == 'Person'), 5] <- "Other"

level_one <- in.gov_level[which(in.gov_level$in_gi==1),c(1,3)] # need to collapse on level one orgs to remove within group dups at the parent agency level
level_one.clnd <- level_one[!duplicated(level_one),]
View(level_one.clnd)
level_one.clnd <- ddply (level_one.clnd, .(patent_id), transform, weight= (1/length(patent_id)))

freq.level_one <- count(level_one.clnd, 'level_one', wt_var = "weight")
View(freq.level_one)
freq.level_one.srtd <- freq.level_one[order(-freq.level_one$freq), ]
head(freq.level_one.srtd, 10)

top6 <- as.character(freq.level_one.srtd[1:6,1])
top6

count.ass_sector_org <- count(ass_sector_org.merged.ratio.clnd, vars = c("level_one", "thes_types"), wt_var = "weight")
count.ass_sector_org.short <- count.ass_sector_org[which(count.ass_sector_org$level_one %in% top6), ]
count.ass_sector_org.short <- count.ass_sector_org.short[which(count.ass_sector_org.short$level_one != "United States Government" ) ,]
count.ass_sector_org.short$level_one <- gsub ("([^ ]+ [^ ]+) ", "\\1\n", count.ass_sector_org.short$level_one)

head(count.ass_sector_org.short)

g.18a <- ggplot(aes(y = freq, x = reorder(level_one, -freq), fill = thes_types), data = count.ass_sector_org.short) + 
  geom_bar( stat="identity") +
  geom_text(data=subset(count.ass_sector_org.short, freq > 400), aes(label = format(round(freq, 0), big.mark=",", nsmall = 0)),  position = position_stack(vjust = 0.5) , colour="white", size=4, family = "Cambria") + 
  scale_colour_manual(values=rep("#000000", 4)) +
  ylab(label="Number of Patents") +  xlab("US Federal Department or Agency") +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(values = c(darkPurple, cyan, darkGreen, darkGrey)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        text=element_text(size=16,  family="Cambria"),
        legend.title=element_blank()
  ) 
g.18a
ggsave (paste0("out\\funders-assignees.stacked_", script_v, ".png"), device = "png")

g.18b <- ggplot(aes(y = freq, x = reorder(level_one, -freq), fill = thes_types), data = count.ass_sector_org.short) + 
  geom_bar( stat="identity", position = "dodge") +
  ylab(label="Number of Patents") +  xlab("US Federal Department or Agency") +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(values = c(darkPurple, cyan, darkGreen, darkGrey)) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        text=element_text(size=16,  family="Cambria"),
        legend.title=element_blank()
  )
g.18b
ggsave (paste0("out\\funders-assignees.dodged_", script_v, ".png"), device = "png")

# 19n. Assignees by firm size
head(ass_sector_org.merged.ratio)
in.size <- read.csv("sanjay_file_w_size_at_issue_update.csv", header = TRUE, stringsAsFactors = FALSE)
head(in.size)
patent_size.merged <- merge(ass_sector_org.merged.ratio, in.size, by="patent_id")
patent_size.merged.clnd <- patent_size.merged[which(patent_size.merged$size_issue != ""),]

patent_size.merged.clnd$type <- ""
colnames(patent_size.merged.clnd)
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Academic"), 8] <- "Academic" 
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "micro"), 8] <- "Small firm" 
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "small"), 8] <- "Small firm" 
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "large"), 8] <- "Large firm" 
View(patent_size.merged.clnd)

patent_size.short <- patent_size.merged.clnd[,c(1,3,8)]
patent_size.unique <- unique(patent_size.short)
patent_size.ddply <- ddply (patent_size.unique, .(patent_id), transform, weight= (1/length(patent_id)))
patent_size.ddply.short <- patent_size.ddply[patent_size.ddply$type != "",]
patent_size.cnt <-  count(patent_size.ddply.short, vars = c("type", "year"), wt_var = "weight")

g.19n <- ggplot(patent_size.cnt, aes(x = year, y = freq, colour=type, group = type, linetype=type)) + 
  geom_line(size=1.5) + 
  ylab(label="Number of Patents") +  xlab("Year") + 
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015)) +
  scale_y_continuous(breaks=c(500,1000,1500,2000,2500), minor_breaks =c(500,1000,1500,2000,2500)) +
  scale_colour_manual(values=c(darkGreen, darkBlue, cyan), labels=c("Academic", "Large firm", "Small firm")) +
  scale_linetype_manual(values=c("dashed", "twodash", "solid"), labels=c("Academic", "Large firm", "Small firm")) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  ) 
g.19n
ggsave (paste0("out\\firmSize_", script_v, ".png"), device = "png")


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
ggsave (paste0("out\\citations\\citationDistribution_", script_v, ".png"), arrangeGrob(g21a, g21b, g21c, g21d, ncol=2), device = "png")

mean(non_GI.cites.merged$num_citations_in_5yrs)
mean(subset(num_citations_in_5yrs,entity == 'university')[,1])
mean(subset(num_citations_in_5yrs,entity == 'corporate')[,1])
mean(subset(num_citations_in_5yrs,entity == 'government')[,1])

# 21n. Citation analysi: means over time
# SQL is available at create_distance_metrics.sql in code dir
# Python code for sector assignements for assignees is in code dir: Regex_dev.ipynb

head(ass_sector_org.merged)
university.patents <- unique(ass_sector_org.merged[which(ass_sector_org.merged$thes_types == "Academic"),1])
corporate.patents <- unique(ass_sector_org.merged[which(ass_sector_org.merged$thes_types == "Corporate"),1])
government.patents <- unique(ass_sector_org.merged[which(ass_sector_org.merged$thes_types == "Government"),1])
colnames(in.patent_level)

university.num_citations_in_5yrs <- data.frame(in.patent_level[in.patent_level$patent_id %in% university.patents,c(6,15)], "university")
colnames (university.num_citations_in_5yrs) <- c("year", "num_citations_in_5yrs", "entity")
corporate.num_citations_in_5yrs <- data.frame(in.patent_level[in.patent_level$patent_id %in% corporate.patents,c(6,15)], "corporate")
colnames (corporate.num_citations_in_5yrs) <- c("year", "num_citations_in_5yrs", "entity")
government.num_citations_in_5yrs <- data.frame(in.patent_level[in.patent_level$patent_id %in% government.patents,c(6,15)], "government")
colnames (government.num_citations_in_5yrs) <- c("year", "num_citations_in_5yrs", "entity")

mean(university.num_citations_in_5yrs[university.num_citations_in_5yrs$year <= 2012, 2])
mean(corporate.num_citations_in_5yrs[corporate.num_citations_in_5yrs$year <= 2012, 2])
mean(government.num_citations_in_5yrs[government.num_citations_in_5yrs$year <= 2012, 2])

colnames(in.all)
nrow(in.all)
in.non_GI.patent_cites <- read.csv("non-GI_patent_citations.csv", header = TRUE, stringsAsFactors = FALSE)
nrow(in.non_GI.patent_cites)
colnames(in.non_GI.patent_cites)

non_GI.cites.merged <- merge (in.all[,c(1,6)], in.non_GI.patent_cites[,c(1:2)], by="patent_id", all.y = TRUE)
non_GI.num_citations_in_5yrs <- data.frame(non_GI.cites.merged[which(non_GI.cites.merged$year >= 1980 & non_GI.cites.merged$year <= 2016),c(2:3)], "nonGI")
colnames (non_GI.num_citations_in_5yrs) <- c("year", "num_citations_in_5yrs", "entity")
head(non_GI.num_citations_in_5yrs)

num_citations_in_5yrs <- rbind (university.num_citations_in_5yrs, corporate.num_citations_in_5yrs, government.num_citations_in_5yrs, non_GI.num_citations_in_5yrs)
num_citations_in_5yrs.means  <- aggregate(num_citations_in_5yrs[,c(2)], list(Year = num_citations_in_5yrs$year, Entity = num_citations_in_5yrs$entity), na.rm=TRUE, mean)

num_citations_in_5yrs.small <- num_citations_in_5yrs.means[which(num_citations_in_5yrs.means$Year <= 2012), ]

g.21n <- ggplot(num_citations_in_5yrs.small) + 
  geom_line(aes(x = Year, y = x, colour=Entity, linetype=Entity), size=1.5) + 
  ylab(label="Mean Citations") +  xlab("Year") +
  scale_colour_manual(values=c(darkPurple, darkBlue, darkGreen, darkGrey), labels=c("University\nGovernment Interest Patents", "Corporate\nGovernment Interest Patents", "Government Assigned\nPatents", "Non-Government\nInterest Patents")) +
  scale_linetype_manual(values=c("dashed", "twodash", "dotted", "solid"), labels=c("University\nGovernment Interest Patents", "Corporate\nGovernment Interest Patents", "Government Assigned\nPatents", "Non-Government\nInterest Patents")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.21n
ggsave (paste0("out\\longCitations", script_v, ".png"), device = "png")


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
  scale_x_discrete(name = "Number of Distinct Governent Funders", limits=as.character(c(1:6)), labels=as.character(c(1:6)))

grid.arrange(g24a, g24b, ncol=2)
ggsave (paste0("out\\inventors\\inventorBoxPlots_", script_v, ".png"), arrangeGrob(g24a, g24b, ncol=2), device = "png")


# 25. look at wipo_fields again
wipo.25 <- filtered.24
wipo.25.dd <- ddply(wipo.25, .(wipo_field), transform, avg_inventors = mean(num_inventors), avg_unique_sectors = mean(num_unique_sectors, na.rm = TRUE), avg_unique_funders = mean(num_unique_funders), cnt=length(wipo_field))
colnames(wipo.25.dd)
wipo.25.short <- unique(wipo.25.dd[,c(5,19:22)])

print(corr.test(wipo.25.short[,c(2:5)], use="pairwise", method="pearson", alpha=.05), short=FALSE)

mean(wipo.25$num_inventors)

# 26. examine # of co-funded patents
cofunded.26 <- filtered.24
cofunded.26$morethan1funder <- cofunded.26$num_unique_funders > 1

morethan1funder.count_by_year <- count (cofunded.26[which(cofunded.26$morethan1funder == TRUE),], vars = c("morethan1funder", "year"))


freq.gi.by_year <- count(in.patent_level.merged[which (in.patent_level.merged$avg_gi == 1),], 'year')
unique(freq.gi.by_year$year)
freq.gi.by_year.clnd <- subset(freq.gi.by_year, !is.na(year) & year <= 2016 & year >= 1980)
head(freq.gi.by_year.clnd)
freq.gi.by_year.clnd$freqIndex <- freq.gi.by_year.clnd$freq / freq.gi.by_year.clnd[freq.gi.by_year.clnd$year=="1980", which(colnames(freq.gi.by_year.clnd)=="freq")] * 100
head(freq.gi.by_year.clnd)

merged.26 <- merge (morethan1funder.count_by_year, freq.gi.by_year.clnd, by = "year")
merged.26$percentMoreThanOne <- merged.26$freq.x / merged.26$freq.y
merged.26


# 27. citation analysis where 
# merge in.patent_level gi only with each of the five year counts; left join on all
in.cite_1 <- read.csv("citations\\gov_int_cites_year1.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_2 <- read.csv("citations\\gov_int_cites_year2.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_3 <- read.csv("citations\\gov_int_cites_year3.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_4 <- read.csv("citations\\gov_int_cites_year4.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_5 <- read.csv("citations\\gov_int_cites_year5.csv", header = TRUE, stringsAsFactors = FALSE)

cit.merge_1 <- merge (in.patent_level.sub, in.cite_1[,1:2], by="patent_id", all.x = TRUE)
cit.merge_2 <- merge (cit.merge_1, in.cite_2[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr1", "_yr2"))
cit.merge_3 <- merge (cit.merge_2, in.cite_3[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr2", "_yr3"))
cit.merge_4 <- merge (cit.merge_3, in.cite_4[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr3", "_yr4"))
cit.merge_5 <- merge (cit.merge_4, in.cite_5[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr4", "_yr5"))

sector_merge <- merge (cit.merge_5, ass_sector_org.merged.ratio.clnd, by="patent_id", all.x = TRUE)
sector_merge$First <- sector_merge$num_citations_1 * sector_merge$weight
sector_merge$Second <- sector_merge$num_citations_2 * sector_merge$weight
sector_merge$Third <- sector_merge$num_citations_3 * sector_merge$weight
sector_merge$Fourth <- sector_merge$num_citations_4 * sector_merge$weight
sector_merge$Fifth <- sector_merge$num_citations_5 * sector_merge$weight
head(sector_merge)

cit_sector.count <- aggregate(cbind (weight, First, Second, Third, Fourth, Fifth) ~
                                thes_types, sum, na.rm= TRUE, data = sector_merge[,c(12:18)])
View(cit_sector.count)
cit_sector.count.clnd <- cit_sector.count[!is.na(cit_sector.count$thes_types),]
View(cit_sector.count.clnd)

cit_sector.count.clnd$First <- cit_sector.count.clnd$First / cit_sector.count.clnd$weight
cit_sector.count.clnd$Second <- cit_sector.count.clnd$Second / cit_sector.count.clnd$weight
cit_sector.count.clnd$Third <- cit_sector.count.clnd$Third / cit_sector.count.clnd$weight
cit_sector.count.clnd$Fourth <- cit_sector.count.clnd$Fourth / cit_sector.count.clnd$weight
cit_sector.count.clnd$Fifth <- cit_sector.count.clnd$Fifth / cit_sector.count.clnd$weight

cit_sector.count.melt <- melt(cit_sector.count.clnd, id="thes_types", measure.vars = c(3:7))
View(cit_sector.count.melt)

g.27 <- ggplot(cit_sector.count.melt, aes(x=variable,y=value,fill=thes_types)) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(y= "Average Accrued Weighted Citations", x="Year After Publication") +
  scale_y_continuous(label=comma) +
  scale_fill_manual(values=c(darkPurple, cyan, darkGreen, darkGrey), labels=c("Academic", "Corporate", "Government", "Other")) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        text=element_text(size=16,  family="Cambria"),
        legend.title=element_blank()
  )

g.27
ggsave (paste0("out\\fiveYearCitationImpact_", script_v, ".png"), device = "png")
