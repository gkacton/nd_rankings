# Load Packages

library(dplyr)
library(stats)

# Import Vote4Holt rankings spreadsheet

nd_rankings <- read.csv("V4H_NDRankings.csv")
nd_rankings$game_abv[23] <- "SAW"

# Calculate simple average

nd_rankings <- nd_rankings %>% 
  rowwise() %>% 
  mutate(simple_avg = mean(c(story, suspects, puzzles, music, atmosphere, ending)))

# Weighted Averages
## Vibes Ranking: weight music + atmosphere higher than other categories

vibe_weights <- c(1, 1, 1, 3, 3, 1)

nd_rankings <- nd_rankings %>% 
  rowwise() %>% 
  mutate(vibes_avg = weighted.mean(c(story, suspects, puzzles, music, atmosphere, ending), vibe_weights))

## Writing Ranking: weight story, suspects, ending higher than other categories

writing_weights <- c(3, 3, 1, 1, 1, 3)

nd_rankings <- nd_rankings %>% 
  rowwise() %>% 
  mutate(writing_avg = weighted.mean(c(story, suspects, puzzles, music, atmosphere, ending), writing_weights))

## Interaction Ranking: weight puzzle and suspects higher than other categories

interaction_weights <- c(1, 3, 3, 1, 1, 1)

nd_rankings <- nd_rankings %>% 
  rowwise() %>% 
  mutate(interaction_avg = weighted.mean(c(story, suspects, puzzles, music, atmosphere, ending), interaction_weights))

#------- GGPLOT VISUALIZATIONS ---------------
# Vibes

vibes_ggplot <- nd_rankings %>% 
  ggplot() +
  geom_point(aes(x = simple_avg,
                           y = vibes_avg,
                           color = ui_era,
                           text = game_abv)
             
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Average Score") +
  ylab("Vibes Score") +
  labs(title = "Vibes Score vs Average Score",
       color = "UI Era") +
  theme_light() +
  theme(
    text = element_text(
                        family = "Courier"
    ) 
  ) +
  scale_color_manual(values = c("#7A0700", "#FED5A6", "#D2AF26", "#043846", "#B3C2C5"))

vibes_ggplot <- ggplotly(vibes_ggplot,
         tooltip = c("text"))

vibes_ggplot

# Writing

writing_ggplot <- nd_rankings %>% 
  ggplot() +
  geom_point(aes(x = simple_avg,
                 y = writing_avg,
                 color = ui_era,
                 text = game_abv)
             
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Average Score") +
  ylab("Writing Score") +
  labs(title = "Writing Score vs Average Score",
       color = "UI Era") +
  theme_light() +
  theme(
    text = element_text(
      family = "Courier"
    ) 
  ) +
  scale_color_manual(values = c("#7A0700", "#FED5A6", "#D2AF26", "#043846", "#B3C2C5"))

writing_ggplot <- ggplotly(writing_ggplot,
                         tooltip = c("text"))

writing_ggplot

# Interaction

interaction_ggplot <- nd_rankings %>% 
  ggplot() 
  geom_point(aes(x = simple_avg,
                 y = interaction_avg,
                 color = ui_era,
                 text = game_abv)
             
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Average Score") +
  ylab("Interaction Score") +
  labs(title = "Interaction Score vs Average Score",
       color = "UI Era") +
  theme_light() +
  theme(
    text = element_text(
      family = "Courier"
    ) 
  ) +
  scale_color_manual(values = c("#7A0700", "#FED5A6", "#D2AF26", "#043846", "#B3C2C5"))

interaction_ggplot <- ggplotly(interaction_ggplot,
                           tooltip = c("text"))

interaction_ggplot

#---------AN ATTEMPT AT PLOTLY---------------
# IGNORE

# ref_line <- data.frame(x=1:30, y=1:30)
# 
# vibes_plot <- nd_rankings %>% 
#   plot_ly(x = ~simple_avg,
#           y = ~vibes_avg,
#           type = "scatter",
#           text = ~game_abv,
#           color = ~ui_era
#           ) %>% 
#   layout(xaxis = list(
#                   autorange = "reversed"
#                   ),
#          yaxis = list(
#                   autorange = "reversed"
#                   )
#          )
# 
# vibes_plot %>% 
#   layout(shapes = list(list(
#   type = "line", 
#   x0 = 0, 
#   x1 = ~max(simple_avg, vibes_avg), 
#   xref = "x",
#   y0 = 0, 
#   y1 = ~max(simple_avg, vibes_avg),
#   yref = "y",
#   line = list(color = "black")
# )))
#   
