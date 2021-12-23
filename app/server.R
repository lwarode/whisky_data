server <- function(input, output, session) {
  # reactive df
  whisky_data_reactive <- reactive(
    if (is.null(input$dist_input) || input$dist_input == "") {
      whisky_data
    } else {
      whisky_data %>% 
        filter(Distillery %in% input$dist_input)
    }
  )
  
  # map ---------------------------------------------------------------------
  reactive_icon <- reactive({
    if (is.null(input$dist_input) || input$dist_input == "") {
      makeIcon(
        iconUrl = "images/barrel_wood_icon.png",
        iconWidth = 20,
        iconHeight = 20
      ) 
    } else {
      makeIcon(
        iconUrl = "images/barrel_wood_icon.png",
        iconWidth = 50,
        iconHeight = 50
      )
    }
  })
  
  min_zoom_reactive <- reactive(
    if (is.null(input$dist_input) || input$dist_input == "") {
      6
    } else {
      7
    }
  )
  
  max_zoom_reactive <- reactive(
    if (is.null(input$dist_input) || input$dist_input == "") {
      9
    } else {
      8
    }
  )
  
  output$whisky_map <- renderLeaflet(
    leaflet(whisky_data_reactive(),
            options = leafletOptions(
              minZoom = min_zoom_reactive(),
              maxZoom = max_zoom_reactive()
            )
    ) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addMarkers(
        lat = ~ lat,
        lng = ~ long,
        popup = ~ paste0(
          "<b>",
          Distillery,
          "</b>",
          "<hr style='height:2px; margin:1px;'>",
          "Dominant Taste Categories:",
          "</br>",
          value_label
        ),
        icon = reactive_icon()
      )
  )

  # radar plot --------------------------------------------------------------
  radar_plot <- reactive({
    if (input$normalize_input == "Original Variables") {
      whisky_data %>% 
        as_tibble %>% 
        select(Distillery, Body:Floral) %>%
        pivot_longer(
          cols = -Distillery,
          names_to = "taste_category",
          values_to = "value"
        ) %>% 
        filter(Distillery %in% input$dist_input) %>% 
        arrange(taste_category) %>% 
        ggplot(
          aes(
            x = taste_category,
            y = value,
            group = Distillery,
            color = Distillery,
            fill = Distillery,
          )
        ) +
        geom_point(show.legend = FALSE) +
        geom_polygon(alpha = 0.5) +
        coord_radar() +
        scale_y_continuous(limits = c(0, 4)) +
        scale_fill_manual(values = "grey20") +
        scale_color_manual(values = "grey30") +
        labs(title = input$dist_input, x = "", y = "") +
        theme_minimal() +
        theme(
          text = element_text(family = "EB Garamond"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)
        )
      
    } else if (input$normalize_input == "Normalized Variables") {
      df <- whisky_data %>% 
        as_tibble %>% 
        select(Distillery, Body:Floral) %>%
        mutate(
          across(where(is.numeric), ~ normalize(.x))
        ) %>%
        pivot_longer(
          cols = -Distillery,
          names_to = "taste_category",
          values_to = "value"
        ) 
      
      y_minimum <- df %>% pull(value) %>% min
      y_maximum <- df %>% pull(value) %>% max
      
      df %>% 
        filter(Distillery %in% input$dist_input) %>% 
        arrange(taste_category) %>% 
        ggplot(
          aes(
            x = taste_category,
            y = value,
            group = Distillery,
            color = Distillery,
            fill = Distillery,
          )
        ) +
        geom_point(show.legend = FALSE) +
        geom_polygon(alpha = 0.5) +
        coord_radar() +
        scale_y_continuous(limits = c(y_minimum, y_maximum)) +
        scale_fill_manual(values = "grey20") +
        scale_color_manual(values = "grey30") +
        labs(title = input$dist_input, x = "", y = "") +
        theme_minimal() +
        theme(
          text = element_text(family = "EB Garamond"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)
        )
    }
  })
  
  output$radar_plot_final <- renderPlot({
    validate(
      need(input$dist_input, "\n\nSelect Distillery")
    )
    radar_plot()
  })
  
  name_dist <- eventReactive(input$dist_input, {
    if (!is.null(input$dist_input) || input$dist_input != "") {
      input$dist_input %>% 
        unlist %>% 
        tolower %>% 
        str_replace_all(" ", "_")
    } 
  })
  
  output$debug_name <- renderPrint(name_dist())

  output$radar_plot_download <- downloadHandler(
    filename = function() {
      paste0(name_dist(),  "_radar_plot.png")
    },
    content = function(file) {
      ggsave(
        radar_plot(),
        filename = file,
        device = "png",
        width = 6,
        height = 6
      )
    }
  )
  
  observe({
    if (is.null(input$dist_input) || input$dist_input == "") {
      disable("radar_plot_download")
    } else {
      enable("radar_plot_download")
    }
  })
 
}
