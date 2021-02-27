###################################################
# postseason QB performance

# s. sullivan
# 2/27/2021
###################################################

###################################################
# install packages
###################################################

# get rid of scientific notation
options(scipen=999)

# devtools::install_github("mrcaseb/nflfastR")

library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(DBI)
library(RSQLite)
library(reshape2)
library(bayesboot)
library(gt)

###################################################
# connect to SQLite dbo, limit data
###################################################

update_db()
connection <- dbConnect(SQLite(), "./pbp_db")
connection

dbListTables(connection)

pbp_db <- tbl(connection, "nflfastR_pbp")

# create lu for postseason teams
playoffTeams <- as_tibble(pbp_db %>%
  filter(season_type == "POST", posteam != "", !is.na(posteam), season >= 2004) %>%
  distinct(posteam, season)
  )

# limit to teams that made the playoffs
pbpData <- pbp_db %>%
  filter(season >= 2010, pass == 1, !is.na(qb_epa), !is.na(passer_player_name)) %>%
  collect() %>%  
  inner_join(playoffTeams, by = c("posteam","season")) %>%
  select(season, season_type, posteam, passer_player_name, passer_player_id, qb_epa)

dbDisconnect(connection)

###################################################
# bootstrap sample each passer season
###################################################

# create list of unique passers/season with at least 200 regular season attempts
unique <- pbpData %>%
  filter(season_type == "REG") %>%
  group_by(passer_player_name, season) %>%
  summarize(plays = n()) %>%
  filter(plays >= 200) %>%
  distinct(passer_player_name, season) %>%
  mutate(id = paste(passer_player_name, season))

plays <- pbpData %>%
  filter(season_type == "REG") %>%
  inner_join(unique, by = c("season","passer_player_name"))

# create empty list to load everything into
lst = list()

# create ID field to loop through
ids = unique(plays$id)

# loop bayes bootstrap for each QB regular season
set.seed(123) # so results can be replicated

for (qb in ids){
  pbpID = plays %>% filter(id == qb)
  b <- bayesboot(as.vector(pbpID$qb_epa), mean, r = 200)
  s = summary(b)
  mean_epa = s$value[1]
  sd = s$value[2]
  lci = s$value[3]
  uci = s$value[4]
  df = data.frame('mean'=mean_epa,'sd'=sd,'LCI'=lci,'UCI'=uci)
  lst[[qb]] = df
}

# put each row into a df
df = dplyr::bind_rows(lst)

# add QB/season identifier
df$QB = ids

# order by mean EPA
df = df %>% arrange(mean)

###################################################
# add postseason EPA per dropback
###################################################

# mean EPA for postseason
postseasonEPA <- pbpData %>%
  filter(season_type == "POST") %>%
  group_by(season, posteam, passer_player_name) %>%
  summarize(postseason_epa = mean(qb_epa),
            total_dropbacks = n()) %>%
  filter(total_dropbacks >= 10) %>%
  mutate(id = paste(passer_player_name, season))

# join to bootstrap sample data
joined <- df %>%
  inner_join(postseasonEPA, by = c("QB" = "id")) # removes some QBs that didn't play in postseason (i.e. Wentz 2017)

# calculate p-value for each postseason performance
pValues <- joined %>%
  mutate(p = pnorm(postseason_epa, mean = mean, sd = sd, lower.tail = FALSE)) # just want over-performance


###################################################
# join in photos
###################################################

# pull roster data
rosters <- nflfastR::fast_scraper_roster(2004:2020) %>%
  filter(position == "QB") %>% 
  mutate(player = paste0(substr(first_name, 1, 1), ".",last_name)) %>%
  distinct(headshot_url, player, season)

pValues <- pValues %>%
  left_join(rosters, by = c("passer_player_name" = "player","season")) %>%
  left_join(select(teams_colors_logos,team_abbr, team_color, team_color2), by = c("posteam" = "team_abbr"))

###################################################
# table for top 10 or whatever
###################################################

tab_data <- pValues %>% 
  mutate(RK = rank(p),
         RK = as.integer(RK)) %>% 
  select(RK,  headshot_url, passer_player_name, season, mean, postseason_epa, p, team_color, team_color2) %>%
  mutate(mean = round(mean,2),
          postseason_epa = round(postseason_epa,2), 
          p = round(p,4)) %>%
  arrange(RK)

tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(headshot_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>% 
    cols_label(
      RK = "Rank",
      headshot_url = "",      
      passer_player_name = "Quarterback",
      season = "Season",
      mean = "Regular Season Est. EPA/Play",
      postseason_epa = "Postseason Actual EPA/Play",
      p = "P-Value of Performance") %>%
    data_color(
      columns = vars(p),
      colors = scales::col_numeric(
        palette = c("#af8dc3", "#f7f7f7", "#7fbf7b"),
        domain = c(0, 1)
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(RK, passer_player_name)
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) %>%
    opt_table_font(
      font = list(
        default_fonts()
      )
    ) 
}


gt_tab1 <- tab_data %>% 
  slice(1:15) %>% 
  tab_function()
gt_tab1

###################################################
# density plot? and then put in postseason as bar?
###################################################

flacco <- pbpData %>%
  filter(season == "2012", passer_player_name == "J.Flacco")
  
flaccoBoot <- bayesboot(as.vector(flacco$qb_epa), mean, r = 200)

flaccoData <- tab_data %>%
  filter(season == "2012" & passer_player_name == "J.Flacco")

team_color <- flaccoData %>%
  select(all_of(team_color)) %>% 
  pull()

team_color2 <- df %>% 
  select(all_of(color2)) %>% 
  pull()

flaccoBoot %>%
  ggplot(aes(V1)) +
  geom_density(fill = flaccoData$team_color, color = "white") +
  # add vertical bar for postseason
  geom_vline(xintercept = flaccoData$postseason_epa %>%
               unlist(),
             linetype = "dashed", alpha=0.5,
             color = flaccoData$team_color2,
             size = 2) +
  # add pic
  geom_image(x = -0.15, y = 1, aes(image = flaccoData$headshot_url), size = 0.2, asp = 16/9) +
  # add theme
  theme(
    # remove fill legend
    legend.position = "none",
    plot.caption = ggtext::element_markdown(size = 12, color = "white"),
    panel.background = element_rect(fill = flaccoData$team_color),
    plot.background = element_rect(fill = flaccoData$team_color),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(color = "white", size = 16),
    axis.text.x =  element_text(color = "white", size = 16),
    axis.title.x =  element_text(color = "white", size = 16),
    axis.line.y = element_line(color = flaccoData$team_color2, size = 1),
    axis.line.x = element_line(color = flaccoData$team_color2, size = 1),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 24, 
                              hjust = 0.5,
                              face = "bold",
                              color = flaccoData$team_color2))

