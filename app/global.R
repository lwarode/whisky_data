library(shiny)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(sf)
library(leaflet)

# shiny ui ----------------------------------------------------------------
plotDownloadButton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("chart-bar"), label)
}

# data management ---------------------------------------------------------
if (! "whiskies.txt" %in% here::here("app", list.files())) {
  whisky_url <- "https://outreach.mathstat.strath.ac.uk/outreach/nessie/datasets/whiskies.txt"
  download.file(whisky_url, "whiskies.txt")
  whisky_data_raw <- read.delim(
    "https://outreach.mathstat.strath.ac.uk/outreach/nessie/datasets/whiskies.txt",
    sep = ","
  )
} else {
  whisky_data_raw <- read.delim(
    "whiskies.txt",
    sep = ","
  )
}

dominant_flavor <- whisky_data_raw %>% 
  select(-c(RowID, Postcode, Longitude, Latitude)) %>% 
  pivot_longer(cols = "Body":"Floral", names_to = "taste_category") %>% 
  group_by(Distillery) %>% 
  filter(value == max(value)) %>% 
  mutate(value_label = toString(taste_category)) %>% 
  distinct(Distillery, value_label) %>% 
  mutate(Distillery = gsub("([a-z])([A-Z])", "\\1 \\2", Distillery)) %>% 
  ungroup

whisky_data <- whisky_data_raw %>% 
  select(-RowID, -Postcode) %>% 
  st_as_sf(coords = c("Latitude", "Longitude")) %>%
  st_jitter(factor = 0.01) %>%
  st_set_crs(27700) %>%
  st_transform(crs = 4326) %>% 
  mutate(
    lat = st_coordinates(.)[, 2],
    long = st_coordinates(.)[, 1],
    Distillery = gsub("([a-z])([A-Z])", "\\1 \\2", Distillery)
  ) %>% 
  left_join(dominant_flavor)

dist_names <- whisky_data %>% 
  add_row(Distillery = "", .before = 1) %>%
  pull(Distillery)

# radar plot --------------------------------------------------------------
# coord function for plot
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# normalize variables
normalize <- function(x) {
  if (is.numeric(x)) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else {
    print("Vector is not numeric")
  }
}

