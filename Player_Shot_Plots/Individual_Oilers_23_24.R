# Setup ------------------------------------------------------------------------

# Clear memory
rm(list = ls(all=T))

# install.packages("devtools")
# devtools::install_github("danmorse314/hockeyR")

# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  sportyR,        # shot plot
  hockeyR,        # hockey stats
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  colorspace,     # fancy stuff with colors 
  ggpointdensity, # For putting the logos on the plot
  viridis,        # Color palettes
  gridExtra,      # Arrange multiple grobs
  glue            # glue together formatted text
)  

# Load and wrangle data---------------------------------------------------------

# Save shot events for filtering
fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")

season <- load_pbp("2023-24") %>%
  filter(event_type %in% fenwick_events)%>%
  # Show the shots as though they were all at the same end
  mutate(x_one_end =  if_else(event_team_type == "away", -x_fixed, x_fixed), 
         y_one_end = if_else(event_team_type == "away", -y_fixed, y_fixed)) 


# Set Up Some Aesthetic Elements -----------------------------------------------


back_colour =  lighten("#EFEFEF",0.25)
strong_text = "black"
weak_text = lighten(strong_text, 0.15)

# Fonts

# Main Font
font_add(family = "Roboto", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Roboto-Regular.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Roboto-Bold.ttf")

# Symbols
font_add(family = "Font Awesome 6 Brands",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")

# Make the fonts work
showtext_auto()

main_font = "Roboto"



# Save Some Stuff for the plot -------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne  "

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne  "

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"


my_caption <- glue("<b>Data: </b> The hockeyR package ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #000000'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #000000'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #000000'>{linkedin_username}</span>")

my_subtitle <- paste(subtitle = "A description of the plot")


# The Actual Plot---------------------------------------------------------------

# Make Individual Plots
plot_function <- function(player_name) {
  player <-  season %>%
    filter(event_player_1_name == player_name) 

  geom_hockey("nhl", 
              xlims = c(-2, 102)) +
    geom_pointdensity(
      data = player, 
      aes(x_one_end, y_one_end), 
      size = 2, 
      alpha = 0.75
    ) + 
    scale_colour_viridis() + 
    labs(
      title = paste(player_name, "Shot Attempts"),
      subtitle = "2023-24 Season Including Playoffs",
      caption = my_caption
    ) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_rect(fill = back_colour,
                                      color = back_colour),
      plot.background = element_rect(fill = back_colour, 
                                     colour = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(1.75),
                                          family = main_font,
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(4, 8, 8, 8)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = main_font,
                                             colour = weak_text,
                                             margin = margin(0, 8, 8, 8)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = main_font,
                                      hjust = c(0), 
                                      margin = margin(1,8,4,8))
    )
}

draisaitl <- plot_function("Leon Draisaitl")
hyman <- plot_function("Zach Hyman")
mcdavid <- plot_function("Connor McDavid")
bouchard <- plot_function("Evan Bouchard")


# combined_plot <- grid.arrange(mcdavid, draisaitl, hyman, bouchard, 
#                               ncol = 2, bottom = my_caption_grob)



# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("Player_Shot_Plots/Draisailt_23_24.png"), plot = draisaitl, height = 6.5, width = 5)
ggsave(here("Player_Shot_Plots/Hyman_23_24.png"), plot = hyman, height = 6.5, width = 5)
ggsave(here("Player_Shot_Plots/McDavid_23_24.png"), plot = mcdavid, height = 6.5, width = 5)
ggsave(here("Player_Shot_Plots/Bouchard_23_24.png"), plot = bouchard, height = 6.55, width = 5)
