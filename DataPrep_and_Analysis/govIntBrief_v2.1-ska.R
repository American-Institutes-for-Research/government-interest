# create visualizations for government interest issue brief
# sarora@air.org
# march 2017
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
lightGrey <- rgb (152, 152, 152, maxColorValue=255)

setwd("F:\\Govt_Int\\Final_CSVS") 
script_v <- "2.0"

# read.csvs
in.patent_level <- read.csv("patent_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.gov_level <- read.csv("government_interest_level.csv", header = TRUE, stringsAsFactors = FALSE)
in.all <- read.csv("patent_level_all.csv", header = TRUE)
in.assignees.all <- read.csv("all_assignees.csv", header = TRUE, stringsAsFactors = FALSE) # note this is all assignees! 
in.sector <- read.csv("assignees_lookedup_types.csv", header = TRUE, stringsAsFactors = FALSE)
in.size <- read.csv("government_interest_patents_1980-2017_returned.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_1 <- read.csv("citations\\gov_int_cites_year1.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_2 <- read.csv("citations\\gov_int_cites_year2.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_3 <- read.csv("citations\\gov_int_cites_year3.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_4 <- read.csv("citations\\gov_int_cites_year4.csv", header = TRUE, stringsAsFactors = FALSE)
in.cite_5 <- read.csv("citations\\gov_int_cites_year5.csv", header = TRUE, stringsAsFactors = FALSE)

# add patents without organizations as US government to in.gov_level (fixing a data problem here)
set.diff <- setdiff(sort(in.patent_level$patent_id), unique(sort(in.gov_level$patent_id)))
length(set.diff) # 3,233
new.rows <- cbind (set.diff, rep("United States Government"), rep("United States Government"), rep("NULL"), rep("NULL"))
# if you look like at these government interest patents, there are some full text fields which have organizations in there; not fixing here

# process patent level data
in.patent_level.bkp_all_years <- in.patent_level
in.patent_level <- in.patent_level[in.patent_level$year != "NULL",]
in.patent_level$year <- as.numeric(as.character(in.patent_level$year))
in.patent_level$patent_id <- as.character(in.patent_level$patent_id)

View(in.patent_level[1:100,])
colnames(in.patent_level)

nrow(in.patent_level.bkp_all_years)

# filter on years 
in.patent_level <- subset(in.patent_level, year >= 1980 & year <= 2017)
patents.keep_ids <- in.patent_level$patent_id
length(patents.keep_ids) # 125,249
nrow(in.patent_level)

# load government interest data 
View(in.gov_level[1:100,])
colnames(in.gov_level)
in.gov_level.short <- in.gov_level
colnames(new.rows) <- colnames(in.gov_level)
in.gov_level <- rbind(in.gov_level, new.rows)
in.gov_level$patent_id <- as.character(in.gov_level$patent_id)
nrow(in.gov_level)
length(unique(in.gov_level$patent_id)) 


# filter on gov_level ids to get the right years 
in.gov_level.bkp <- in.gov_level
in.gov_level <- in.gov_level[which(in.gov_level$patent_id %in% patents.keep_ids), ]
length(unique(in.gov_level$patent_id)) # 125,249

# merge data two main data files
in.patent_level.merged <- merge(in.patent_level, in.gov_level, by="patent_id")
nrow(in.patent_level.merged) # 151,090

# test this merge looks legit 
View(in.patent_level.merged)
in.patent_level.merged[which(in.patent_level.merged$patent_id=='4180935'), ]
in.gov_level[which(in.gov_level$patent_id=='4180935'),] # 1.0 weight 
in.patent_level.merged[which(in.patent_level.merged$patent_id=='4181062'), ]
in.gov_level[which(in.gov_level$patent_id=='4181062'),] # 0.0 weight 
in.patent_level.merged[which(in.patent_level.merged$patent_id=='4181139'), ]
in.gov_level[which(in.gov_level$patent_id=='4181139'),] # 0.5 weight

# 5. patent classifications -- WIPO fields (below sectors)
freq.wipo_field <- count(in.patent_level, 'wipo_field')
freq.wipo_field.nn <- subset (freq.wipo_field, wipo_field != "NULL")
head(freq.wipo_field.nn)
freq.wipo_field.srtd <- freq.wipo_field.nn[order(-freq.wipo_field.nn$freq), ]
head(freq.wipo_field.srtd)
top5 <- as.character(freq.wipo_field.srtd[1:6,1])
top5

sum.weights <- sum(freq.wipo_field.srtd$freq)
freq.wipo_field.srtd$percentage <- freq.wipo_field.srtd$freq / sum.weights
levels(freq.wipo_field.srtd$wipo_field) <- gsub (" ", "\n", levels(freq.wipo_field.srtd$wipo_field))


# longitudinal (gi_patents)
long.5a <- count(in.patent_level, vars = c("wipo_field", "year"))
keep.5a <- long.5a[which(long.5a$wipo_field %in% top5), ]
head(keep.5a)

g.5l <- ggplot(keep.5a, aes(x = year, y = freq, colour=wipo_field, group = wipo_field, linetype=wipo_field)) + 
  geom_line( size=1.5) + 
  ylab(label="Number of Patents") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkGrey, darkGreen, darkRed, darkPurple, lightGrey)) +
  scale_linetype_manual(values=c("solid", "twodash", "dashed", "dotted", "longdash", "dotdash")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  theme_set(theme_gray(base_size = 16)) + 
  guides(colour=guide_legend(nrow=3,byrow=TRUE)) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  )
g.5l
ggsave (paste0("out\\longWipoFields_v", script_v, ".png"), device = "png")


# get wipo_field frequencies across all patents
in.all[1:10,]
in.all.since_1980 <- subset(in.all, year >= 1980 & year <= 2017)
in.all.since_2005 <- subset(in.all, year >= 2005 & year <= 2017)
colnames(in.all.since_2005)
freq.wipo_field.all <- count(in.all.since_2005, vars = c("wipo_field"))

# determine overall GI patent ratio vis-a-vis all patents, by wipo field
freq.wipo_field.merge <- merge (freq.wipo_field, freq.wipo_field.all, by="wipo_field")
freq.wipo_field.merge$ratio <-  freq.wipo_field.merge$freq.x / freq.wipo_field.merge$freq.y

long.5b <- count(in.all.since_2005, vars = c("wipo_field", "year"))

merged.long5 <- merge (long.5a, long.5b , by=c("wipo_field","year"), suffixes=c("_5a", "_5b"))
merged.long5$ratio <- merged.long5$freq_5a / merged.long5$freq_5b

# keep only the top 5 wipo_fields
keep.5a <- long.5a[which(long.5a$wipo_field %in% top5), ] # or in top10
keep.5b <- long.5b[which(long.5b$wipo_field %in% top5), ]
head(keep.5a)
head(keep.5b)

merged.keep5 <- merge (keep.5a, keep.5b , by=c("wipo_field","year"), suffixes=c("_5a", "_5b"))
merged.keep5$ratio <- merged.keep5$freq_5a / merged.keep5$freq_5b

g.5lp <- ggplot(merged.keep5) + 
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
g.5lp
ggsave (paste0("out\\longWipoFieldsPercent_v", script_v, ".png"), device = "png")


# 9. graph R&D expenditures over time (line) 
in.fund <- read.csv("Agencies.csv", header = TRUE, stringsAsFactors = FALSE)
View(in.fund)
colnames(in.fund)
in.fund$FederalIndex <- in.fund$Total.R.D / in.fund[in.fund$Fiscal.Year==1980, which(colnames(in.fund)=="Total.R.D")] * 100
g.9 <- ggplot(data=in.fund, aes(x=Fiscal.Year, y=Total.R.D, group=1)) + geom_line(size=1.5, color="#0066CC") + 
  xlab ("Year") + ylab("Federal Expenditures (in billions of dollars)") + scale_y_continuous(labels = scales::dollar)
g.9

# 10. let's look at the increase in all patents over the years
freq.all.by_year <- count(in.all, 'year')
freq.all.by_year
freq.all.by_year$freqIndex <- freq.all.by_year$freq / freq.all.by_year[freq.all.by_year$year=="1980", which(colnames(freq.all.by_year)=="freq")] * 100
freq.all.by_year <- subset(freq.all.by_year, year <= 2017 & year >= 1980)

g.10 <- ggplot(data=freq.all.by_year, aes(x=year, y=freq,group=1)) + 
  geom_line(size=1.5, color="#EC7C27") + xlab ("Number of patents") + ylab("Count") + 
  scale_y_continuous(labels = comma)
g.10

# 11a. increase in GI patents over the years
freq.gi.by_year <- count(in.patent_level, 'year')
unique(freq.gi.by_year$year)
freq.gi.by_year.clnd <- subset(freq.gi.by_year, !is.na(year) & year <= 2017 & year >= 1980)
head(freq.gi.by_year.clnd)
freq.gi.by_year.clnd$freqIndex <- freq.gi.by_year.clnd$freq / freq.gi.by_year.clnd[freq.gi.by_year.clnd$year=="1980", which(colnames(freq.gi.by_year.clnd)=="freq")] * 100

unique(freq.gi.by_year.clnd$year)
g.11a <- ggplot(data=freq.gi.by_year.clnd, aes(x=year, y=freq,group=1)) + 
  geom_line(size=1.5, color="#EC272a") + xlab ("Number of patents") + ylab("Count") + 
  scale_y_continuous(labels = comma)
g.11a


# 12. create index graphs across R&D expenditures, all patents, and gi patents
merged.ind1 <- merge (freq.gi.by_year.clnd, freq.all.by_year , by="year", suffixes=c("_gi", "_all"))
merged.ind2 <- merge (merged.ind1, in.fund, by.x="year", by.y = "Fiscal.Year")

colnames(merged.ind2)
melt.ind <- melt(merged.ind2, id="year", measure.vars = c(3,5,21))
colnames(melt.ind)
View(melt.ind )

g.12 <- ggplot(melt.ind) + 
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
g.12
ggsave (paste0("out\\indexed_", script_v, ".png"), device = "png")


# 13. inventors by year
patent_level.bkp <- in.patent_level
colnames(patent_level.bkp)
nrow(patent_level.bkp)
patent_level.bkp$year <- as.numeric(as.character(patent_level.bkp$year))
gi.agg <- aggregate(patent_level.bkp[,c(2,3)], list(Year = patent_level.bkp$year), na.rm=TRUE, mean)

all.bkp <- in.all 
colnames (all.bkp)
all.bkp$year <- as.numeric(as.character(all.bkp$year))
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
g.13
ggsave (paste0("out\\longInventor_v", script_v, ".png"), device = "png")


# process assignees
in.assignees <- in.assignees.all[which(in.assignees.all$patent_id %in% patents.keep_ids), ]
nrow(in.assignees)
length(unique(in.assignees$patent_id))
length (patents.keep_ids[which (!patents.keep_ids %in% in.assignees$patent_id)]) # patents with missing assignees 
View (in.assignees)

in.assignees$name <- paste (in.assignees$name_first, in.assignees$name_last, sep = " ")
in.assignees$entity <- in.assignees$organization
in.assignees$entity[which(in.assignees$entity == "")] <- in.assignees$name[which(in.assignees$entity == "")]
colnames(in.assignees)

assignees.clnd <- in.assignees[,c(1,2,8)]
assignees.clnd <- ddply (assignees.clnd, .(patent_id), transform, weight= (1/length(patent_id)))
View(assignees.clnd)

assignees.merged <- merge (assignees.clnd, in.patent_level.merged, by="patent_id")
assignees.merged$entity <- toTitleCase(tolower(assignees.merged$entity))
head(assignees.merged[order(assignees.merged$weight),])
head(assignees.merged)


# 17. sankey viz for funders --> assignees
colnames(assignees.merged)
assignees.merged.sub <- assignees.merged[,c(1, 3, 20)] # patent_id, entity, level_one
head(assignees.merged.sub)

ass_org.merged.ratio <- ddply (unique(assignees.merged.sub), .(patent_id), transform, weight= (1/length(patent_id))) # weight by patent_id
head(ass_org.merged.ratio[order(ass_org.merged.ratio$patent_id),])

count.ass_org <- count(ass_org.merged.ratio, vars = c("entity", "level_one"), wt_var = "weight")
count.ass_org.srtd <- count.ass_org[order(-count.ass_org$freq),]
colnames(count.ass_org.srtd)

top.rows <- 30
count.ass_org.srtd <- count.ass_org.srtd[1:top.rows,]
count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the United States Department of Energy", count.ass_org.srtd$entity), ]
count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Administrator of the National Aeronautics and Space Administration", count.ass_org.srtd$entity), ]
count.ass_org.srtd <- count.ass_org.srtd[!grepl("The United States of America as Represented by the Secretary of the Navy", count.ass_org.srtd$entity), ]
View(count.ass_org.srtd)

# create nodes 
top.rows <- nrow(count.ass_org.srtd)
top.funders <- unique(count.ass_org.srtd[1:top.rows,2])
top.assignees <- unique(count.ass_org.srtd[1:top.rows,1])
nodes = c (top.funders, top.assignees)

# impute funder node ids onto original counts
funders.links <- sapply(1:length(top.funders), function(x) grep(nodes[x], count.ass_org.srtd[,2]))
fl.df <- data.frame(ID = rep(seq(funders.links), sapply(funders.links, length)), Obs = unlist(funders.links))
fl.df[, 1] <- fl.df[,1] - 1
count.ass_org.srtd$source <- fl.df[order(fl.df$Obs), 1]

# impute assignee node ids onto original counts
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
    title = "Top Government Interest Organizations to Assignees, 1980 - 2017",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
  )

p.sk
export(p.sk, file = paste0("out\\assignees\\Sankey_", script_v, "_", ".png"))
htmlwidgets::saveWidget(p.sk, file = paste0("F:\\Govt_Int\\Final_CSVS\\out\\assignees\\Sankey org name_", script_v, "_", ".html"))

# 18. merge sector type for assignees and create dodged bar chart
View(count(in.sector[in.sector$thes_types == "Government",], 'organization')) # QA, sort by freq
View(count(in.sector[in.sector$thes_types == "Academic",], 'organization')) # QA, sort by freq
View(count(in.sector[in.sector$thes_types == "Hospital",], 'organization')) # QA, sort by freq
View(count(in.sector[in.sector$thes_types == "Corporate",], 'organization')) # QA, sort by freq
View(count(in.sector[in.sector$thes_types == "Person",], 'organization')) # QA, sort by freq
in.sector$organization <- trimws(in.sector$organization)
in.sector$organization <- toTitleCase(tolower(in.sector$organization))

level_one.sub <- unique(in.patent_level.merged [, c(1,17) ]) # just patent_id, level_one org

sector_org.merged <-  merge (level_one.sub, in.sector, by="patent_id")
head(sector_org.merged)
sector_org.merged <- sector_org.merged[,c(1,2,5:6)]
head(sector_org.merged)
View(count(sector_org.merged, 'patent_id'))

in.sector[in.sector$patent_id == '7138569', ]  # for testing try 9203030, 7595023, 6824972, 7138569, and 4180935
level_one.sub[level_one.sub$patent_id == '7138569', ]
sector_org.merged[sector_org.merged$patent_id == '7138569' , ]

sector_org.merged.ratio <- ddply (sector_org.merged, .(patent_id), transform, weight= (1/length(patent_id)))
head(sector_org.merged.ratio)
sector_org.merged.ratio[sector_org.merged.ratio$patent_id == '9203030',] # spot testing

sector_org.merged.ratio.clnd <- sector_org.merged.ratio # collapse some categories per USPTO
sector_org.merged.ratio.clnd[which(sector_org.merged.ratio.clnd$thes_types == 'Hospital'),  4] <- "Academic or Hospital"
sector_org.merged.ratio.clnd[which(sector_org.merged.ratio.clnd$thes_types == 'Academic'),  4] <- "Academic or Hospital"
sector_org.merged.ratio.clnd[which(sector_org.merged.ratio.clnd$thes_types == 'Ambiguous'),  4] <- "Corporate"
sector_org.merged.ratio.clnd[which(sector_org.merged.ratio.clnd$thes_types == 'Person'), 4] <- "Other"
sector_org.merged.ratio.clnd[sector_org.merged.ratio.clnd$patent_id == '6824972',] # spot testing

level_one <- in.gov_level[,c(1,3)] # get top funders
level_one.clnd <- level_one[!duplicated(level_one),]
View(level_one.clnd)
level_one.clnd <- ddply (level_one.clnd, .(patent_id), transform, weight= (1/length(patent_id)))

freq.level_one <- count(level_one.clnd, 'level_one', wt_var = "weight")
View(freq.level_one)
freq.level_one.srtd <- freq.level_one[order(-freq.level_one$freq), ]
head(freq.level_one.srtd, 10)

top6 <- as.character(freq.level_one.srtd[1:6,1])
top6

count.sector_org <- count(sector_org.merged.ratio.clnd, vars = c("level_one", "thes_types"), wt_var = "weight")
count.sector_org.short <- count.sector_org[which(count.sector_org$level_one %in% top6), ]
count.sector_org.short <- count.sector_org.short[which(count.sector_org.short$level_one != "United States Government" ) ,]
count.sector_org.short <- count.sector_org.short[which(count.sector_org.short$thes_types != "Other" ) ,]
count.sector_org.short$level_one <- gsub ("([^ ]+) ", "\\1\n", count.sector_org.short$level_one)

head(count.sector_org.short)

g.18b <- ggplot(aes(y = freq, x = reorder(level_one, -freq), fill = thes_types), data = count.sector_org.short) + 
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
head(sector_org.merged.ratio.clnd)
head(in.size)
patent_size.merged <- merge(sector_org.merged.ratio.clnd, in.size, by="patent_id")
patent_size_year.merged <- merge(patent_size.merged, in.patent_level, by="patent_id") # grab year
colnames(patent_size_year.merged)
patent_size_year.merged <- patent_size_year.merged[,c(1:4,6,11)] # reduce number of attributes
head(patent_size_year.merged)
patent_size.merged.clnd <- patent_size_year.merged[which(patent_size_year.merged$size_issue != ""),]
head(patent_size.merged.clnd)
patent_size.merged.clnd[patent_size.merged.clnd$patent_id == '6824972',] # spot testing

# rename some fields
patent_size.merged.clnd$type <- ""
colnames(patent_size.merged.clnd)
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Academic or Hospital"), 7] <- "Academic or Hospital" 
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "micro"), 7] <- "Small firm" 
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "small"), 7] <- "Small firm" 
patent_size.merged.clnd[which(patent_size.merged.clnd$thes_types == "Corporate" & patent_size.merged.clnd$size_issue == "large"), 7] <- "Large firm" 
View(patent_size.merged.clnd)

colnames(patent_size.merged.clnd)
patent_size.merged.clnd[patent_size.merged.clnd$patent_id == '4929418',] # spot testing
patent_size.short <- patent_size.merged.clnd[,c(1,6,7)] # patent_id, thes_types (sector), and type
nrow(patent_size.short)
patent_size.short[patent_size.short$patent_id == '4929418',] # spot testing
patent_size.unique <- unique(patent_size.short) # unique because sector_org contains gi orgs and we don't need that info here
nrow(patent_size.unique)
patent_size.ddply <- ddply (patent_size.unique, .(patent_id), transform, weight= (1/length(patent_id))) # needed because patents have multiple assignees which belong to select sectors we care about
View(patent_size.ddply)
patent_size.ddply.short <- patent_size.ddply[patent_size.ddply$type != "",]
View(patent_size.ddply.short)
colnames(patent_size.ddply.short)
patent_size.cnt <-  count(patent_size.ddply.short, vars = c("type", "year"), wt_var = "weight")
head(patent_size.cnt)
max(patent_size.cnt$freq)
patent_size.cnt <- patent_size.cnt[patent_size.cnt$year >= 1990,]

g.19n <- ggplot(patent_size.cnt, aes(x = year, y = freq, colour=type, group = type, linetype=type)) + 
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
g.19n
ggsave (paste0("out\\firmSize_", script_v, ".png"), device = "png")





# 27. Five year citation analysis 
# merge in.patent_level gi only with each of the five year counts; left join on all.  Must remove 2014 and later patents
cit.merge_1 <- merge (in.patent_level[which (in.patent_level$year < 2014),], in.cite_1[,1:2], by="patent_id", all.x = TRUE)
cit.merge_2 <- merge (cit.merge_1, in.cite_2[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr1", "_yr2"))
cit.merge_3 <- merge (cit.merge_2, in.cite_3[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr2", "_yr3"))
cit.merge_4 <- merge (cit.merge_3, in.cite_4[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr3", "_yr4"))
cit.merge_5 <- merge (cit.merge_4, in.cite_5[,1:2], by="patent_id", all.x = TRUE, suffixes = c("_yr4", "_yr5"))
View(cit.merge_5)
nrow(cit.merge_5)

head(cit.merge_5)
head(sector_org.merged.ratio.clnd)

# quick test
cit.merge_5[cit.merge_5$patent_id == '7138569', ]
sector_org.merged.ratio.clnd[sector_org.merged.ratio.clnd$patent_id == '7138569' , ]

sector_merge <- merge (cit.merge_5, sector_org.merged.ratio.clnd, by="patent_id", all.x = TRUE)
sector_merge$First <- sector_merge$num_citations_1 * sector_merge$weight
sector_merge$Second <- sector_merge$num_citations_2 * sector_merge$weight
sector_merge$Third <- sector_merge$num_citations_3 * sector_merge$weight
sector_merge$Fourth <- sector_merge$num_citations_4 * sector_merge$weight
sector_merge$Fifth <- sector_merge$num_citations_5 * sector_merge$weight
head(sector_merge)

colnames(sector_merge)
cit_sector.count <- aggregate(cbind (weight, First, Second, Third, Fourth, Fifth) ~
                                thes_types, sum, na.rm= TRUE, data = sector_merge[,c(23:29)])
View(cit_sector.count)
cit_sector.count.clnd <- cit_sector.count[!is.na(cit_sector.count$thes_types),]
View(cit_sector.count.clnd)

cit_sector.count.clnd$First <- cit_sector.count.clnd$First / cit_sector.count.clnd$weight
cit_sector.count.clnd$Second <- cit_sector.count.clnd$Second / cit_sector.count.clnd$weight
cit_sector.count.clnd$Third <- cit_sector.count.clnd$Third / cit_sector.count.clnd$weight
cit_sector.count.clnd$Fourth <- cit_sector.count.clnd$Fourth / cit_sector.count.clnd$weight
cit_sector.count.clnd$Fifth <- cit_sector.count.clnd$Fifth / cit_sector.count.clnd$weight

colnames(cit_sector.count.clnd)
cit_sector.count.melt <- melt(cit_sector.count.clnd, id="thes_types", measure.vars = c(3:7))
View(cit_sector.count.melt)

cit_sector.count.melt.clnd <- cit_sector.count.melt[which(cit_sector.count.melt$thes_types != "Other"), ]
cit_sector.count.melt.clnd

g.27 <- ggplot(cit_sector.count.melt.clnd, aes(x=variable,y=value,fill=thes_types)) +
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

g.27
ggsave (paste0("out\\fiveYearCitationImpact_", script_v, ".png"), device = "png")



# 28. data table alinging sector, agency, and field, by year
sector_org_field.by_year.merged <- merge(sector_org.merged.ratio.clnd, in.patent_level, by="patent_id")
colnames (sector_org_field.by_year.merged)
head(sector_org_field.by_year.merged)
sector_org_field.by_year.small <- sector_org_field.by_year.merged[c(1,2,4,9,10)]
sector_org_field.by_year.ratio <- assignees.clnd <- ddply (sector_org_field.by_year.small, .(patent_id), transform, weight= (1/length(patent_id)))
head(sector_org_field.by_year.ratio)

sector_org_field.by_year.clnd <- sector_org_field.by_year.ratio
sector_org_field.by_year.clnd[which (! sector_org_field.by_year.clnd$level_one %in% top6 ), 2] <- "Other"
sector_org_field.by_year.clnd[which (sector_org_field.by_year.clnd$level_one == "United States Government"), 2] <- "Other"

sector_org_field.by_year.count <- count(sector_org_field.by_year.clnd, vars = c("level_one", "thes_types", "wipo_field", "year"), wt_var = "weight")
View (sector_org_field.by_year.count)

# create patent_level-gov_level merged dataset and weight by level_one org 
# 5/2/2018 for Andy's DOE presentation 
in.patent_level.ratio <- ddply (in.patent_level.merged, .(patent_id), transform, weight= (1/length(patent_id)))
nrow(in.patent_level.ratio) # 125,249
in.patent_level.ratio %>% summarize (count = sum(weight))
patents.by_agency <- in.patent_level.ratio %>% group_by (level_one) %>% summarize (count = sum(weight))

# export select datasets
write.csv (sector_org_field.by_year.count, file="out\\sector_org_field.by_year.count.csv")
write.csv (freq.all.by_year, file="out\\sector_org_field.by_year.count")
write.csv (freq.all.by_year, file="out\\freq.all.by_year.csv")
write.csv (freq.gi.by_year, file="out\\freq.gi.by_year.csv")
write.csv (in.patent_level.merged, file="out\\out.patent_level.merged.csv")
write.csv (patents.keep_ids, file="out\\patents.keep_ids.csv")
write.csv (merged.keep5, file = "out\\merged_wipo_fields.keep6.csv")
write.csv (long.5a, file = "out\\gi.wipo_fields.keep6.csv")
write.csv (patent_size.cnt, file = "out\\patent_size.cnt.csv")
write.csv (cit_sector.count.melt.clnd, file = "out\\citations_accrued_years_1_thru_5.csv")
write.csv (in.patent_level.ratio, file = "out\\weighted_gi_patents.by_funding_agency.csv")
 
# check sectors before and after my improvements
in.sector.archive %>% group_by(thes_types) %>% summarise(count = length(patent_id))
in.sector %>% group_by(thes_types) %>% summarise(count = length(patent_id))
gov.archive <- in.sector.archive[which(in.sector.archive$thes_types == "Government"), ]
View(gov.archive %>% group_by(organization) %>% summarise(count = length(patent_id)))
                       