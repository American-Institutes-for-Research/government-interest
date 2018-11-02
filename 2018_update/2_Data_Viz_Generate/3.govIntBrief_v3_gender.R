
source("requirements.R")
script_v <- "3.0"

#########################################################################################################

# connect to the aws mySQL server
source("config.r")
my_db=src_mysql(dbname=dbname,host=host,port=port,user=user,password=password)
#########################################################################################################

#read in helper data
in.patent_level <- as.data.frame(tbl(my_db, "temp_patent_level_gi_v3"))

temp <- in.patent_level %>% 
          filter(year != "NULL" &  year >= 1980 & year <= 2018) 
          
#load inventor gender
in.inventor_has_fem <- as.data.frame(tbl(my_db, "temp_gi_has_female_inv_v3"))
in.gov_level <- as.data.frame(tbl(my_db, "temp_gi_level_gi_v3"))

# dont need to run if the length of set.diff is 0
#set.diff <- setdiff(sort(in.patent_level$patent_id), unique(sort(in.gov_level$patent_id)))
#new.rows <- cbind (set.diff, rep("United States Government"), rep("United States Government"), rep("NULL"), rep("NULL"))

#drop unknown gender and years outside range
gi_inventor_has_fem <- in.inventor_has_fem %>% 
                        inner_join(in.patent_level, by ="patent_id") 

gi_inventor_has_fem.sub <- gi_inventor_has_fem %>% 
                        filter(year >= 1980 & year <= 2018) %>% 
                        filter(has_fem_inv==0 | has_fem_inv==1)

gi_inventor_has_fem$year <- as.numeric(gi_inventor_has_fem$year)
gi_inventor_has_fem$has_fem_inv <- as.numeric(gi_inventor_has_fem$has_fem_inv)

#organize the data to get the percentage of patents in each year with atleast one woman **
grouped_gi_inventor_has_fem <- gi_inventor_has_fem.sub %>% 
                                  group_by(year, has_fem_inv) %>% 
                                  summarise(count_inv = n())


total_count_gi_inventor_has_fem <- gi_inventor_has_fem %>% 
                                      group_by(year) %>% 
                                      summarise(total = length(year))
for_graph <- grouped_gi_inventor_has_fem %>% 
              inner_join(total_count_gi_inventor_has_fem, by = 'year') %>% 
              mutate(PercentageofPatents = count_inv/total)
for_graph$has_fem_inv <- as.character(for_graph$has_fem_inv)


gi_teams_female_inv <- ggplot(for_graph) +
  geom_line(aes(x=year,y=PercentageofPatents,color=has_fem_inv,linetype=has_fem_inv), size = 1.5) +
  ylab(label="Percentage of Patents") +  xlab("Year") +
  scale_colour_manual(values=c(cyan, darkPurple, darkGreen), labels=c("All Male Inventors", "At Least One Woman",  "All Unknown")) +
  scale_linetype_manual(values=c("solid", "dashed", "twodash"), labels=c("All Male Inventors",  "At Least One Woman", "All Unknown")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y = 0) +
  theme_set(theme_gray(base_size = 16)) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.title=element_blank(),
        text=element_text(size=16,  family="Cambria")
  )
gi_teams_female_inv 
ggsave(paste0("data_viz\\gi_teams_with_female_inventors_over_time", script_v, ".pdf"), plot=gi_teams_female_inv)
write.csv (for_graph, file="out\\teams_with_at_least_one_woman.csv")

##################################################
# percent of teams with at least one woman by team size **
# AMyers: - I want to reformat this one as a scatter plot team size (number of inventors) on the x-axis, 
# average women's share of the inventor team (mean proportion of the team that is female for each team size) 
# and size the markers based on the number of patents. Make sense?

in.gender_wipo <- as.data.frame(tbl(my_db, "temp_gi_inventor_gender_v3"))
in.gender_wipo.sub <- in.gender_wipo %>% 
                        filter(year >= 1980 & year <= 2018) %>% 
                        select(patent_id, num_inventors, dumale) %>%  # patent_id, team_size, dumale
                        filter(dumale==0 | dumale==1) 
in.gender_wipo.sub$dumale <- as.numeric(in.gender_wipo.sub$dumale)

women_in_teams <- in.gender_wipo.sub %>% 
                    group_by(patent_id, num_inventors) %>% 
                    summarize(num_men = sum(dumale)) %>% 
                    mutate(percentWomen = (1 - num_men/num_inventors))

grouped_by.num_inventors <- women_in_teams %>% 
                        filter(percentWomen > 0) %>% 
                        group_by(num_inventors) %>% 
                        summarise(meanPercWomenOnTeam = mean(percentWomen), 
                                  numPatents = length(patent_id))  

for_graph.num_inventors <- grouped_by.num_inventors %>% 
                              filter(num_inventors <= 15)

perc_female_inv <- ggplot(data=for_graph.num_inventors, aes(x=num_inventors, y=meanPercWomenOnTeam)) +
  geom_point(aes(colour = numPatents, size=numPatents)) + 
  labs(y= "Mean Percentage of Women\non Teams with At Least One Woman", x="Inventor Team Size", colour="Number of\nPatents", size="Number of\nPatents") +
  theme_set(theme_gray(base_size = 16)) + 
  scale_size_continuous(range = c(2, 16), labels = comma) +
  scale_colour_gradient(low = "blue", labels = comma) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::percent) + 
  guides(color=guide_legend(), size = guide_legend()) + 
  scale_x_continuous(breaks = rep(1:15), minor_breaks = rep(1:15)) +
  theme(
    legend.direction = 'horizontal', 
    legend.position = 'bottom',
    legend.key = element_blank(),
    legend.title.align = 0.5,
    legend.key.width = unit(3, 'lines'),
    text=element_text(size=12),
    legend.box.background = element_rect(colour = lightGrey), 
    legend.background = element_blank()
  ) 
perc_female_inv
# pdf
ggsave (paste0("data_viz\\percent_with_female_inventor_by_team_size", script_v,".pdf"), plot = perc_female_inv)

# png 
ggsave (paste0("data_viz\\percent_with_female_inventor_by_team_size", script_v,".png"), plot = perc_female_inv)



write.csv(for_graph.num_inventors, "out\\mean_percent_of_teams_with_a_female_inventor.csv")

