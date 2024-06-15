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
  camcorder,      # record the making of the plot into a gif
  ggpointdensity, # For putting the logos on the plot
  viridis, 
  glue            # glue together formatted text
)  

# Load and wrangle data---------------------------------------------------------

# Save shot events for filtering
# get only shot events
fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")

season <- load_pbp("2023-24")


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


my_caption <- glue("<b>Data: </b> The hockeyR package",
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
    filter(event_player_1_name == player_name, 
           event_type %in% fenwick_events) %>%
    mutate(x_one_end =  abs(x), 
           y_one_end = if_else(x < 0, -y, y))  

  geom_hockey("nhl", 
              xlims = c(-1, 101)) +
    geom_pointdensity(
      data = player, 
      aes(x_one_end, y_one_end), 
      size = 2, 
      alpha = 0.75
    ) + 
    scale_colour_viridis() + 
    labs(
      title = paste(player_name, "Shot Attempts 2023-24"),
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

# Plot Faceted by Player

players <-  season %>%
  filter(event_player_1_name %in% c("Leon Draisaitl", "Zach Hyman", 
                                    "Connor McDavid", "Evan Bouchard"), 
         event_type %in% fenwick_events) %>%
  mutate(x_one_end =  abs(x), 
         y_one_end = if_else(x < 0, -y, y))  

geom_hockey("nhl") +
  geom_pointdensity(
    data = players, 
    aes(x_one_end, y_one_end), 
    size = 2, 
    alpha = 0.75
  ) + 
  facet_wrap(~event_player_1_name, 
             scales = "free_color") + 
  scale_colour_viridis() + 
  labs(
    title = paste("Shot Attempts in 2023-24"),
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
                                    margin = margin(1,8,4,8)), 
    strip.background = element_blank(), 
    strip.text = element_text(size = rel(1),
                              family = main_font,
                              colour = strong_text)
  )


# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("Player_Shot_Plots/Draisailt_23_24.png"), plot = draisaitl, height = 5, width = 8)
ggsave(here("Player_Shot_Plots/Hyman_23_24.png"), plot = hyman, height = 5, width = 8)
ggsave(here("Player_Shot_Plots/McDavid_23_24.png"), plot = mcdavid, height = 5, width = 8)
ggsave(here("Player_Shot_Plots/Bouchard_23_24.png"), plot = bouchard, height = 5, width = 8)
