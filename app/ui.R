ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML('* {font-family: "EB Garamond"};'))),
  fluidRow(
    column(
      width = 4,
      titlePanel(
        h1(HTML("<b>Whisky Dashboard </b>"),
        # h1(HTML("<b>"), "Whisky Dashboard", HTML("</b>"), 
           HTML("<hr style='height:10px; margin:10px;'>"),
           h2("Mapping 86 Scottish Distilleries"),
           HTML("<hr style='height:10px;'>")
        )
      ),
      fluidRow(
        column(
          width = 6,
          selectInput("dist_input",
                      "Select Distillery:",
                      choices = dist_names
          )
        ),
        column(
          width = 6,
          radioButtons(
            "normalize_input",
            "Use Normalized Variables for Radar Plot",
            choices = c("Original Variables", "Normalized Variables"),
            selected = "Normalized Variables"
          )
        )
      ),
      plotOutput("radar_plot_final") %>% 
        withSpinner(color = "black")
    ),
    column(
      width = 8,
      leafletOutput("whisky_map") %>% 
        withSpinner(color = "black"),
      br(),
      plotDownloadButton("radar_plot_download", label = "Download Radar Plot")
    )
  )
)
