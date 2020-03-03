  # devtools::install_github("meysubb/cfbscrapR")
# remotes::install_github("rstudio/gt")
library(tidyverse)
library(cfbscrapR)
library(gt)


pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df)
}
# write.csv(pbp_2019,"pbp_2019.csv",row.names=F)
head(pbp_2019)
tail(pbp_2019)

pbp_2019 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained) %>% head()

glimpse(pbp_2019)

levels(factor(pbp_2019$play_type))
pbp_2019 %>% count(play_type, sort = TRUE)

plays <- pbp_2019 %>% filter(rush == 1 | pass == 1)

offense <- plays %>% group_by(offense_play) %>% summarise(ypa = mean(yards_gained[pass==1]), ypr = mean(yards_gained[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

offense %>% arrange(desc(ypr))

offense %>% arrange(desc(ypa))

offense %>% arrange(ypa)

offense %>% arrange(ypr)

offense <- plays %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

defense <- plays %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

#Offensive Team Pass/Rush EPA
team_epa <- left_join(offense, defense, by = c("offense_play" = "defense_play")) 
cfblogos <- read.csv("https://raw.githubusercontent.com/saiemgilani/NCAA_FB_EPA/master/logos.csv") %>% select(school, logo)

team.epa <- team_epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

head(team.epa)

team.epa %>% ggplot(aes(x=epa.rush.off, y=epa.pass.off)) + geom_image(image = team.epa$logo, asp = 16/9)

team.epa %>% ggplot(aes(x=epa.rush.off, y=epa.pass.off)) + geom_image(image = team.epa$logo, asp = 16/9) +
  geom_vline(xintercept = mean(team.epa$epa.rush.off), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team.epa$epa.pass.off), linetype = "dashed", color = "blue") +
  labs(x = "Rush EPA/Play", y= "Pass EPA/Play",
       title = "2019 NCAA Team Efficiency",
       caption = "Figure: @SaiemGilani | Data: @CFB_data with #cfbscrapR") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))
ggsave('team_epa_logos.png')

#Defensive Team Pass/Rush EPA
pbp_2019 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained) %>% head()

glimpse(pbp_2019)

levels(factor(pbp_2019$play_type))
pbp_2019 %>% count(play_type, sort = TRUE)

plays <- pbp_2019 %>% filter(rush == 1 | pass == 1)

offense <- plays %>% group_by(offense_play) %>% summarise(ypa = mean(yards_gained[pass==1]), ypr = mean(yards_gained[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

offense %>% arrange(desc(ypr))

offense %>% arrange(desc(ypa))

offense %>% arrange(ypa)

offense %>% arrange(ypr)

offense <- plays %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

defense <- plays %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

team_depa <- left_join(defense, offense, by = c("defense_play" = "offense_play")) 
team.depa <- team_depa %>% left_join(cfblogos, by = c("defense_play" = "school"))

head(team.depa)

team.depa %>% ggplot(aes(x=epa.rush.def, y=epa.pass.def)) + geom_image(image = team.depa$logo, asp = 16/9)

team.depa %>% ggplot(aes(x=epa.rush.def, y=epa.pass.def)) + geom_image(image = team.depa$logo, asp = 16/9) +
  geom_vline(xintercept = mean(team.depa$epa.rush.def), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team.depa$epa.pass.def), linetype = "dashed", color = "blue") +
  labs(x = "Defensive Rush EPA/Play", y= "Defensive Pass EPA/Play",
       title = "2019 NCAA Team Defensive Efficiency",
       caption = "Figure: @SaiemGilani | Data: @CFB_data with #cfbscrapR") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))
ggsave('team_depa_logos.png')

pbp_2019 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained) %>% head()

glimpse(pbp_2019)

levels(factor(pbp_2019$play_type))
pbp_2019 %>% count(play_type, sort = TRUE)

plays <- pbp_2019 %>% filter(rush == 1 | pass == 1)

offense <- plays %>% group_by(offense_play) %>% summarise(ypa = mean(yards_gained[pass==1]), ypr = mean(yards_gained[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

offense %>% arrange(desc(ypr))

offense %>% arrange(desc(ypa))

offense %>% arrange(ypa)

offense %>% arrange(ypr)

offense <- plays %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

defense <- plays %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)

team_depa <- left_join(defense, offense, by = c("defense_play" = "offense_play")) 
team.depa <- team_depa %>% left_join(cfblogos, by = c("defense_play" = "school"))

head(team.depa)

team.depa %>% ggplot(aes(x=epa.rush.def, y=epa.pass.def)) + geom_image(image = team.depa$logo, asp = 16/9)

team.depa %>% ggplot(aes(x=epa.rush.def, y=epa.pass.def)) + geom_image(image = team.depa$logo, asp = 16/9) +
  geom_vline(xintercept = mean(team.depa$epa.rush.def), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team.depa$epa.pass.def), linetype = "dashed", color = "blue") +
  labs(x = "Defensive Rush EPA/Play", y= "Defensive Pass EPA/Play",
       title = "2019 NCAA Team Defensive Efficiency",
       caption = "Figure: @SaiemGilani | Data: @CFB_data with #cfbscrapR") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))
ggsave('team_depa_logos.png')


fsu_ <- plays %>% filter(offense_play == "Florida State") 
fsu <- fsu_ %>% left_join(cfblogos, by = c("offense_play" = "school"))

fsu %>%
  ggplot(aes(x=adj_yd_line, y=EPA)) +
  geom_point() +
  labs(x = "Yard Line",
       y = "EPA",
       title = "Expected Points Added by Field Position",
       subtitle = "FSU Offense 2019",
       caption = "Figure: @SaiemGilani | Data: @CFB_data with #cfbscrapR") +
  geom_abline(slope=0, intercept = 0, alpha = 0.5, col = "red") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))
ggsave('FSU_epa.png')

fsu_d <- plays %>% filter(defense_play == "Florida State") 
fsu <- fsu_d %>% left_join(cfblogos, by = c("defense_play" = "school"))

fsu %>%
  ggplot(aes(x=adj_yd_line, y=EPA)) +
  geom_point() +
  labs(x = "Yard Line",
       y = "EPA",
       title = "Expected Points Added by Field Position",
       subtitle = "FSU Defense 2019",
       caption = "Figure: @SaiemGilani | Data: @CFB_data with #cfbscrapR") +
  geom_abline(slope=0, intercept = 0, alpha = 0.5, col = "red") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))
ggsave('FSU_depa.png')

plays %>% filter(offense_play == "Florida State" & EPA < -4) %>% select(offense_play, defense_play, play_text, down, distance, adj_yd_line)


plays %>% 
  filter(offense_play %in% c("LSU", "Ohio State", "Clemson", "Oklahoma") & pass == 1) %>%  
  group_by(offense_play) %>%
  summarize(mean_epa = mean(EPA)) %>%
  arrange(desc(mean_epa))

plays %>% 
  filter(offense_play %in% c("LSU", "Ohio State", "Clemson", "Oklahoma") & pass == 1) %>%  
  group_by(offense_play) %>%
  summarize(success.rate = mean(success)) %>%
  arrange(desc(success.rate))

#Passing
team.epa %>% arrange(desc(epa.pass.off)) %>% mutate(rank = dense_rank(desc(epa.pass.off))) %>% 
  filter(rank < 10) %>% gt()

team.epa %>% arrange(desc(epa.pass.off)) %>% mutate(rank = dense_rank(desc(epa.pass.off))) %>%
  select(rank, offense_play, epa.pass.off) %>% 
  filter(rank < 11) %>% gt() %>%
  tab_header(title = "Best Passing Teams") %>%
  cols_label(rank = "Rank", offense_play = "Offense", epa.pass.off = "EPA/Attempt")

library(tidyverse)
library(ggimage)
library(cfbscrapR)
library(ncaahoopR)

pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df)
}

yards.per.play <- pbp_2019 %>% filter(rush == 1 | pass == 1) %>% 
  group_by(offense_play) %>% 
  summarise(ypp.rush = mean(yards_gained[rush==1]), 
            ypp.pass = mean(yards_gained[pass==1]),
            num.plays = n()) %>% filter(num.plays > 600) 

cfblogos <- read.csv("https://raw.githubusercontent.com/saiemgilani/NCAA_FB_EPA/master/logos.csv") %>% select(school, logo)

chartdata <- yards.per.play %>% left_join(cfblogos, by = c("offense_play" = "school"))

chartdata %>% ggplot(aes(x=ypp.rush, y=ypp.pass)) + geom_image(image = chartdata$logo, asp = 16/9) +
  geom_vline(xintercept = mean(chartdata$ypp.rush), linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(chartdata$ypp.pass), linetype = "dashed", color = "red") +
  labs(y = "Yards per Pass",
       x = "Yards per Rush",
       caption = "Figure: @SaiemGilani | Data: @CFB_data with #cfbscrapR",
       title = "Team Yards Per Rush and Yards Per Pass",
       subtitle = "2019 Season") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10))

ggsave('ypp.png')

epa.off <- pbp_2019 %>% filter(rush == 1 | pass == 1) %>%
  group_by(offense_play) %>% 
  summarise(epa.off = mean(EPA),
            num.plays = n()) %>% filter(num.plays > 600) 

epa.def <- pbp_2019 %>% filter(rush == 1 | pass == 1) %>%
  group_by(defense_play) %>% 
  summarise(epa.def = mean(EPA),
            num.plays = n()) %>% filter(num.plays > 600) 

team.epa <- left_join(epa.off, epa.def, by = c("offense_play" = "defense_play"))

cfblogos <- read.csv("https://raw.githubusercontent.com/saiemgilani/NCAA_FB_EPA/master/logos.csv") %>% select(school, logo)

chartdata <- team.epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

chartdata %>% ggplot(aes(x= -epa.def, y= epa.off )) + geom_image(image = chartdata$logo) +
  geom_vline(xintercept = -mean(chartdata$epa.def), linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(chartdata$epa.off), linetype = "dashed", color = "red") +
  labs(y = "Offensive EPA/Play",
       x = "Defensive EPA/Play",
       caption = "Figure: @SaiemGilani | Data: @CFB_data with #cfbscrapR",
       title = "Team Efficiency",
       subtitle = "2019 Season") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10))

ggsave('teamepa.png')

