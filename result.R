
# load packages -----------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------------------

prior <- read_csv("data/prior.csv")
rating <- read_csv("data/eng1_2122_singleapm_rating.csv")
eng2122 <- read_csv("data/eng2122_boxscores.csv")



# plotting prior distribution ---------------------------------------------

library(cowplot)

FIFA_distribution <- prior %>% ggplot(aes(x = adj_overall, y = ..density..)) + 
  geom_histogram(fill = "#ab82c5", alpha= 0.5) +
  geom_density(color = "purple") +
  labs(x = "FIFA Rating") +
  theme_bw()

prior_distribution <- prior %>% filter(total_min >= 900) %>%
  ggplot(aes(x = weighted_pred, y = ..density..)) + 
  geom_histogram(fill = "#ab82c5", alpha= 0.5) +
  geom_density(color = "purple") +
  labs(x = "Prior Value") +
  theme_bw()

plot_grid(FIFA_distribution, prior_distribution, axis = "lr")


# position manipulation -------------------------------------------------------

#"GK": 23
#"DF", "DF,MF", "DF,FW" : 112 + 11 + 2 = 125
#"MF,DF", "MF", "MF,FW": 9 + 72 + 22 = 103
#"FW,DF", "FW", "FW,MF": 3 + 48 + 26 = 77

DF <- c("DF", "DF,MF", "DF,FW")
MF <- c("MF,DF", "MF", "MF,FW")
FW <- c("FW,DF", "FW", "FW,MF")

player_team <- eng2122 %>%
  select(Player, Squad, Pos) %>%
  rename(Team = Squad) 

player_team$Pos <- replace(player_team$Pos, player_team$Pos %in% DF, "DF") 
player_team$Pos <- replace(player_team$Pos, player_team$Pos %in% MF, "MF") 
player_team$Pos <- replace(player_team$Pos, player_team$Pos %in% FW, "FW") 

player_team %>% ggplot(aes(x = Pos)) + 
  geom_bar(fill = "purple", alpha = 0.6) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_x_discrete(limit = c("GK", "DF", "MF", "FW")) +
  labs(title = "Number of Players by Position Group") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# table -------------------------------------------------------------------




new_prior <- prior %>%
  select(Player, total_min, adj_overall, adj_scaled, weighted_pred, pred_scaled) %>%
  left_join(player_team, by = "Player") %>%
  rename(Min = total_min,
         FIFA = adj_overall,
         FIFA_scaled = adj_scaled,
         Box = weighted_pred,
         Box_scaled = pred_scaled)

FIFA_table <- new_prior %>%
  select(-Box, -Box_scaled) %>%
  left_join(select(rating, c(Player, FIFA_coef, FIFA_rating))) %>%
  select(Player, Team, Pos, Min, FIFA, FIFA_rating) %>%
  arrange(desc(FIFA_rating)) 
#%>%
#  head(20)

box_table <- new_prior %>%
  select(-FIFA, -FIFA_scaled) %>%
  left_join(select(rating, c(Player, box_coef, box_rating))) %>%
  select(Player, Team, Pos, Min, Box, box_rating) %>%
  arrange(desc(box_rating)) #%>%
  #head(20)

master_table <- FIFA_table %>% 
  left_join(select(box_table, c(Player, Team, Box, box_rating)),
             by = c("Player", "Team"))


# gt ----------------------------------------------------------------------

library(gt)

# top 20
master_table %>%
  arrange(desc(box_rating)) %>%
  head(20) %>%
  gt(rownames_to_stub = TRUE) %>%
  data_color(
    columns = c(FIFA_rating, box_rating),
    colors = scales::col_numeric(
      palette(c("#eee6f3", "#ab82c5", "#7b5aa6", "#702b9d", "#330662")),
      domain = NULL)(sort(rexp(5))),
    alpha = 0.8
  )


# bottom 10
master_table %>%
  arrange(box_rating) %>%
  head(20) %>%
  gt(rownames_to_stub = TRUE) %>%
  data_color(
    columns = c(FIFA_rating, box_rating),
    #colors = scales::col_numeric("purple", domain = NULL)(sort(rexp(0.01))),
    colors = scales::col_numeric(
      palette(c("#eee6f3", "#ab82c5", "#7b5aa6", "#702b9d", "#330662")),
      domain = NULL)(sort(rexp(5))),
    alpha = 0.8
  )


# distribution ------------------------------------------------------------

master_table %>% ggplot(aes(x = FIFA_rating, y = box_rating)) +
  geom_point(color = "purple") + theme_bw()

master_table %>% ggplot() +
  geom_density(aes(x = FIFA_rating), color = "#1E0635") +
  geom_density(aes(x = box_rating), color = "#fb0f78") +
  xlim(-0.2, 0.2) +
  labs(x = "Rating", title = "Distribution of Model Output") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# group by position -------------------------------------------------------

master_table %>% group_by(Pos) %>%
  summarise(FIFA_rating_avg = mean(FIFA_rating), 
            box_rating_avg = mean(box_rating)) %>%
  ungroup()

master_table %>% 
  filter(!is.na(Pos)) %>%
  ggplot(aes(x = Pos, y = box_rating)) +
  geom_violin(fill = "#ab82c5") +
  geom_boxplot(width = 0.2, color = "#330662") +
  scale_x_discrete(limit = c("GK", "DF", "MF", "FW")) +
  labs(title = "Comparing Model Output Across Position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
    
