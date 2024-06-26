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
  janitor,        # some efficient data cleaning stuff
  camcorder,      # record the making of the plot into a gif
  paletteer,      # color palette
  ggimage,
  glue            # glue together formatted text
)  

# Load and wrangle data---------------------------------------------------------

# Save shot events for filtering
# get only shot events
fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")

season <- load_pbp("2023-24")

game <-  season %>%
  filter(game_date == "2024-06-02", 
         event_type %in% fenwick_events)


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette


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


# grab team logos & colors
team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbr) | team_abbr == unique(game$away_abbr)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

# add transparency to logo
transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

# adding team colors
game <- game %>%
  left_join(team_logos, by = c("event_team_abbr" = "team_abbr"))

# The Actual Plot---------------------------------------------------------------
geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
  ) +
  geom_point(
    data = game,
    aes(x_fixed, y_fixed),
    size = 4,
    color = game$team_color1,
    shape = ifelse(game$event_type == "GOAL", 19, 1)
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)} {unique(game$game_date)}"),
    subtitle = glue::glue(
      "{unique(game$away_abbr)} {unique(game$away_final)} - {unique(game$home_final)} {unique(game$home_abbr) }\n
      Each circle in this graphic represents a shot on net. Circles that are filled in represent goals."
    ),
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
    plot.title = element_textbox_simple(size = rel(2.0),
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



# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("HockeyR Learning/Oilers_Stars2024-06-02.png"), height = 6, width = 8)


