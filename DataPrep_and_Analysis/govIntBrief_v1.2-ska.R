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

cyan <- rgb (0, 122, 182,	maxColorValue = 255)
darkRed <- rgb (134, 14, 34,	maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (0, 105, 45, maxColorValue=255)
darkPurple <- rgb (64, 25, 86, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (206, 206, 199, maxColorValue=255)

setwd("F:\\Govt_Int\\Final_CSVS") 
script_v <- "1.1"

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


# 9. graph R&D expenditures over time (line) 
in.fund <- read.csv("USFund1_0.csv", header = TRUE)
in.fund.bkp <- in.fund
in.fund[1:10,]
colnames(in.fund)
in.fund$FederalIndex <- in.fund$Federal / in.fund[in.fund$Year==1980, which(colnames(in.fund)=="Federal")] * 100
g.9 <- ggplot(data=in.fund, aes(x=Year, y=Federal, group=1)) + geom_line(size=1.5, color="#0066CC") + 
  xlab ("Year") + ylab("Federal Expenditures (in billions of dollars)") + scale_y_continuous(labels = scales::dollar)
g.9

# 10. let's look at the increase in all patents over the years
in.all <- read.csv("patent_level_all.csv", header = TRUE)
in.all[1:10,]
nrow(subset(in.all, year <= 2016 & year >= 1980))
colnames(in.all)
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

# 12. create index graphs across R&D expenditures, all patents, and gi patents
merged.ind1 <- merge (freq.gi.by_year.clnd, freq.all.by_year , by="year", suffixes=c("_gi", "_all"))
merged.ind2 <- merge (merged.ind1, in.fund, by.x="year", by.y = "Year")
merged.ind3 <- merge (freq.ga.by_year.clnd, merged.ind2 , by="year", suffixes=c("_ga", "_m2"))
colnames(merged.ind3)
melt.ind <- melt(merged.ind3, id="year", measure.vars = c(3,5,7,14))
colnames(melt.ind)
View(melt.ind )

g.12 <- ggplot(melt.ind) + 
  geom_line(aes(x = year, y = value, colour=variable, linetype=variable), size=1.5) + 
  ylab(label="Indexed value") +  xlab("Year") +
  scale_colour_manual(values=c(darkGreen, cyan, darkGrey, darkRed), labels=c("Government assigned patents", "Government interest patents", "All patents", "R&D funding")) +
  scale_linetype_manual(values=c("dashed", "solid", "twodash", "dotted"), labels=c("Government assigned patents", "Government interest patents", "All patents", "R&D funding")) +
    theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
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
  ylab(label="Mean Value") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkGrey), labels=c("Mean government\ninterest inventors", "Mean inventors\nacross all patents")) +
  scale_linetype_manual(values=c("twodash", "solid"), labels=c("Mean government\ninterest inventors", "Mean inventors\nacross all patents")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.13
ggsave (paste0("out\\longInventor_v", script_v, ".png"), device = "png")



#14n. Create Federal R&D and total US R&D dollars / patent trends
freq.gi.by_year <- count(in.patent_level.merged[which (in.patent_level.merged$avg_gi > 0),], 'year')
freq.ga.by_year <- count(in.patent_level.merged[which (in.patent_level.merged$avg_gi < 1),], 'year')
freq.gi.by_year.clnd <- subset(freq.gi.by_year, !is.na(year) & year <= 2015 & year >= 1980)
freq.ga.by_year.clnd <- subset(freq.ga.by_year, !is.na(year) & year <= 2015 & year >= 1980)

in.fund.applied <- read.csv("US_applied_research.csv", header = TRUE, stringsAsFactors = FALSE)
View(in.fund.applied)
in.fund.applied <- in.fund.applied[in.fund.applied$X >= 1980 & in.fund.applied$X <= 2015,]
head(in.fund.applied)

head(freq.gi.by_year.clnd)
intramuralFundingPerGAPatent <-in.fund.applied$Federal.Federal.intra.mural / freq.ga.by_year.clnd$freq
extramuralFundingPerGAPatent <- (in.fund.applied$Federal.Total - in.fund.applied$Federal.Federal.intra.mural ) / freq.gi.by_year.clnd$freq

us.patents_ids <- read.csv("us_patents.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(us.patents_ids)
colnames(in.all)
us.patents.merged <- merge (us.patents_ids, in.all, by="patent_id")
us.patents.small <- subset (us.patents.merged, year <= 2015 & year >= 1980)
nrow(us.patents.small)
freq.us_patents.by_year <- count(us.patents.small, 'year')
freq.us_patents.by_year
usFundingPerPatent <- in.fund.applied$All.funding.sources.All.per.former / freq.us_patents.by_year$freq

rdDollarsPerPatent.df <- data.frame(seq(1980, 2015), intramuralFundingPerGAPatent, extramuralFundingPerGAPatent, usFundingPerPatent)
colnames(rdDollarsPerPatent.df)[1] <- "Year"
melt.dolPerPat <- melt(rdDollarsPerPatent.df, id="Year", measure.vars = c(2:4))


g.14n <- ggplot(melt.dolPerPat) + 
  geom_line(aes(x = Year, y = value, colour=variable, linetype=variable), size=1.5) + 
  ylab(label="Dollars per Patent (in Millions)") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkBlue, darkGrey), labels=c("Average intramural\nFederal expenditure\nto produce one government\nassigned patent", "Average extramural\nFederal expenditure\nto produce one government\ninterest patent", "Average overall US R&D\nexpenditure to produce\na patent with at least\none US-based assignee")) +
  scale_linetype_manual(values=c("dashed", "twodash", "solid"), labels=c("Average intramural\nFederal expenditure\nto produce one government\nassigned patent", "Average extramural\nFederal expenditure\nto produce one government\ninterest patent", "Average overall US R&D\nexpenditure to produce\na patent with at least\none US-based assignee")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  ) 
g.14n
ggsave (paste0("out\\dollarsPerPatent_", script_v, ".png"), device = "png")

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
ass_sector_org.merged <- ass_sector_org.merged[,c(1,2,5, 6, 9)]
head(ass_sector_org.merged)

ass_sector_org.merged.ratio <- ddply (ass_sector_org.merged, .(patent_id), transform, weight= (1/length(patent_id)))
head(ass_sector_org.merged.ratio)

level_one <- in.gov_level[which(in.gov_level$in_gi==1),c(1,3)] # need to collapse on level one orgs to remove within group dups at the parent agency level
level_one.clnd <- level_one[!duplicated(level_one),]
View(level_one.clnd)
level_one.clnd <- ddply (level_one.clnd, .(patent_id), transform, weight= (1/length(patent_id)))

freq.level_one <- count(level_one.clnd, 'level_one', wt_var = "weight")
View(freq.level_one)
freq.level_one.srtd <- freq.level_one[order(-freq.level_one$freq), ]
head(freq.level_one.srtd, 10)

top6 <- as.character(freq.level_one.srtd[1:6,1])

count.ass_sector_org <- count(ass_sector_org.merged.ratio, vars = c("level_one", "thes_types"), wt_var = "weight")
count.ass_sector_org.short <- count.ass_sector_org[which(count.ass_sector_org$level_one %in% top6), ]
count.ass_sector_org.short <- count.ass_sector_org.short[which(count.ass_sector_org.short$level_one != "United States Government" ) ,]
count.ass_sector_org.short$level_one <- gsub ("([^ ]+ [^ ]+) ", "\\1\n", count.ass_sector_org.short$level_one)
count.ass_sector_org.short[which(count.ass_sector_org.short$thes_types == 'Ambiguous' | count.ass_sector_org.short$thes_types == 'Hospital' | count.ass_sector_org.short$thes_types == 'Person'), 2] <- "Other"
head(count.ass_sector_org.short)


g.18 <- ggplot() + 
  geom_bar(aes(y = freq, x = reorder(level_one, -freq), fill = thes_types), data = count.ass_sector_org.short, stat="identity") + 
  ylab(label="Number of Patents") +  xlab("US Federal Department or Agency") +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(name="Sector of Assignee", values = c(darkPurple, darkBlue, darkGreen, lightGrey))
g.18
ggsave (paste0("out\\funders-assignees.stacked_", script_v, ".png"), device = "png")

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
View(firstdate.patentdate.merged[which(firstdate.patentdate.merged$id == '9492532'), ])

freq.sector <- count(in.sector, vars=c("organization", "thes_types"))
head(freq.sector[duplicated(freq.sector[,1:2]), ])
freq.sector$organization <- toTitleCase(tolower(freq.sector$organization))

new_entrants.merged <-  merge (firstdate.patentdate.merged, freq.sector, by.x = "entity", by.y="organization")
colnames(new_entrants.merged)
View(new_entrants.merged[which(new_entrants.merged$id == '9492532'), ])

colnames(new_entrants.merged)

new_entrants.merged.small <- new_entrants.merged[which(new_entrants.merged$entry == 1),c(1:7, 11:12, 23:24)]
head(new_entrants.merged.small)
nrow(new_entrants.merged.small) # 2482
colnames(new_entrants.merged.small)
View(new_entrants.merged.small[duplicated(new_entrants.merged.small$id),]) #26 
new_entrants.merged.small[which(new_entrants.merged.small$id == '9492532'), ]

in.size <- read.csv("size_at_issue.csv", header = TRUE, stringsAsFactors = FALSE)
new_entrants.size.merged <- merge (new_entrants.merged.small, in.size, by.x = "id", by.y="patent_id")
head(new_entrants.size.merged)
new_entrants.size.merged[which(new_entrants.size.merged$id == '9492532'), ] # missing 2016 patents

new_entrants.size.merged$type <- ""
new_entrants.size.merged[which(new_entrants.size.merged$thes_types == "Academic"),13] <- "Academic" 
new_entrants.size.merged[which(new_entrants.size.merged$thes_types == "Corporate" & new_entrants.size.merged$size_issue == "micro"),13] <- "Small firm" 
new_entrants.size.merged[which(new_entrants.size.merged$thes_types == "Corporate" & new_entrants.size.merged$size_issue == "small"),13] <- "Small firm" 
new_entrants.size.merged[which(new_entrants.size.merged$thes_types == "Corporate" & new_entrants.size.merged$size_issue == "large"),13] <- "Large firm" 

count.new_entrants <- count(new_entrants.size.merged, vars = c("type", "year"), wt_var = "entry")
count.new_entrants.small <- count.new_entrants[which(count.new_entrants$type != "" & count.new_entrants$year >= 1990), ]


g.19a <- ggplot(count.new_entrants.small, aes(x = year, y = freq, colour=type, group = type, linetype=type)) + 
  geom_line(size=1.5) + 
  ylab(label="Number of Patent Entrants Receiving Government Funding") +  xlab("Year") + scale_x_continuous(breaks=c(1990,2000,2010)) +
  scale_colour_manual(values=c(darkPurple, darkBlue, cyan), labels=c("Academic", "Large firm", "Small firm")) +
  scale_linetype_manual(values=c("dotted", "twodash", "solid"), labels=c("Academic", "Large firm", "Small firm")) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),
        legend.title=element_blank()
  )
g.19a
ggsave (paste0("out\\newEntrants_", script_v, ".png"), device = "png")

wipo.classes <- data.frame(table(new_entrants.size.merged[which(new_entrants.size.merged$type == "Small firm" & new_entrants.size.merged$year > 2014),8]))
wipo.classes[order(wipo.classes$Freq, decreasing = TRUE),]

# 20. Number of new entrants over time by funder
colnames(new_entrants.merged.small)[2] <- "patent_id" 
new_entrants.funded.merged <- merge (new_entrants.merged.small, ass_org.merged, by = "patent_id")
colnames(new_entrants.funded.merged)                                               
new_entrants.funded.small <- new_entrants.funded.merged[,c(16, 9, 10)] # level_one, year, entry
head(new_entrants.funded.small)

count.new_entrants.funded <- count(new_entrants.funded.small, vars = c("level_one", "year.x"), wt_var = "entry")
colnames(count.new_entrants.funded)

count.new_entrants.funded.short <- count.new_entrants.funded[which(count.new_entrants.funded$level_one %in% top6), ]
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

g.21 <- ggplot(num_citations_in_5yrs.small) + 
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
g.21
ggsave (paste0("out\\longCitations", script_v, ".png"), device = "png")


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
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = comma) +
  coord_flip() + xlab(label="Sector of Government Interest Assignee") +  ylab("Number of Patents") + 
  scale_fill_manual(values=c("blue", "cyan", "light blue", "grey")) +
  guides(fill=guide_legend(title="Proximity to\nPatent-Patent\nCitation Boundary"))
g22
ggsave (paste0("out\\citations\\nonGIFirmCitations_toGISectors_", script_v, ".png"), device = "png")


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


