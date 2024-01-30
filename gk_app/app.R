# load df and libraries ----
library(shiny)
library(plotly)
library(tidyverse)
library(DT)

df <- readxl::read_xlsx("~/Library/CloudStorage/GoogleDrive-aoife.ryan@sharecreative.com/.shortcut-targets-by-id/0BwEyzS8OvJgreXdPNGZKV2tyRjg/Share_Clients/data_science_project_work/google_keywords_analysis/data/USA - Keyword Stats 2024-01-16 at 09_12_04.xlsx") %>%
  janitor::row_to_names(row_number = 36, remove_rows_above = FALSE) %>% # column names 
  janitor::clean_names() %>% 
  filter(avg_monthly_searches != 0) %>%
  mutate_at(15:26, as.numeric) %>% # monthly searches stored as character
  mutate(yo_y_change = as.numeric(str_remove_all(yo_y_change, "%")),
         three_month_change = as.numeric(str_remove_all(three_month_change, "%")),
         # yo_y_change = yo_y_change/100,
         # three_month_change = three_month_change/100,
         avg_monthly_searches = as.numeric(avg_monthly_searches),
         log_avg_posts = log(avg_monthly_searches)) %>%
  dplyr::select(-c(competition:in_plan)) 

df <- df%>%
  mutate(total2023 = rowSums(df %>% dplyr::select(searches_jan_2023:searches_dec_2023), 
                             na.rm = TRUE)) %>%# add a total searches column
  rename_with(~sub("searches_(\\w+)_2023", "\\1", .), starts_with("searches_")) %>%
  drop_na()

# end of df load ----
rand_coord <- function(n, xmin, xmax, ymin, ymax, min_distance) {
  set.seed(12)
  # result <- numeric(n)
  result <- data.frame(x = numeric(), y = numeric())
  
  if (n > 0){
    for (i in 1:n){
      
      if (i == 1){
        attempt <- 0
        repeat {
          coord <- data.frame(x = runif(1, xmin, xmax), y = runif(1, ymin, ymax))
          if (abs(coord[1] - 0) > 20 & abs(coord[2] - 0) > 20){
            result[i,] <- coord
            break
          }
          
          attempts <- attempts + 1
          if (attempts > 100){
            warning("Unable to find a suitable number, adjust parameters.")
            result[i,] <- coord
            break
          }
        }
        
      } else {
        attempts <- 0
        
        repeat {
          coord <- data.frame(x = runif(1, xmin, xmax), y = runif(1, ymin, ymax))
          
          if (all(abs(dist(rbind(result,coord))) > min_distance) &
              abs(coord[1] - 0) > 20 & abs(coord[2] - 0) > 20){
            result <- rbind(result, coord)
            break
          } 
          attempts <- attempts + 1
          if (attempts > 100){
            warning("Unable to find a suitable number, adjust parameters.")
            result <- rbind(result, coord)
            break
          }
        }
      }
      
    }
  }
  # return(data.frame(result))
  return(result)
}
rand_coord_opt2 <- function(n, xmin, xmax, ymin, ymax, min_distance, random_state = 12) {
  set.seed(random_state)
  # result <- numeric(n)
  result <- data.frame(x = numeric(), y = numeric())
  
  if (n > 0){
    for (i in 1:n){
      
      if (i == 1){
        attempts <- 0
        repeat {
          coord <- data.frame(x = runif(1, xmin, xmax), y = runif(1, ymin, ymax))
          if (abs(coord[2] - 0) > 20){
            result[i,] <- coord
            break
          }
          
          attempts <- attempts + 1
          if (attempts > 200){
            warning("Unable to find a suitable number, adjust parameters.")
            result[i,] <- coord
            break
          }
        }
        
      } else {
        attempts <- 0
        
        repeat {
          coord <- data.frame(x = runif(1, xmin, xmax), y = runif(1, ymin, ymax))
          
          if (all(apply(result, 1, function(row) abs(coord - row) > min_distance))&
              abs(coord[2] - 0) > 40){
            result <- rbind(result, coord)
            break
          } 
          attempts <- attempts + 1
          if (attempts > 100){
            warning("Unable to find a suitable number, adjust parameters.")
            result <- rbind(result, coord)
            break
          }
        }
      }
      
    }
  }
  # return(data.frame(result))
  return(result)
}
hline <- function(y = 0, color = "#4d4d4d") {
  list(
    type = "line", 
    x0 =0.1,
    x1 = 400000, 
    # xref = "paper",
    y0 = y, 
    y1 = y, 
    width = 0.5,
    line = list(color = color, dash = "dash")
  )
}

# logifySlider javascript function ----
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('4*10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (4*Math.pow(10, num)).toFixed(0); }
    })
  }
}"

# call logifySlider for each relevant sliderInput ----
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    // logifySlider('num_posts', sci = false)
    logifySlider('num_posts', sci = false)
  }, 5)})
"
# ----

ui <- fluidPage(
  
  tags$head(tags$script(HTML(JS.logify))),
  tags$head(tags$script(HTML(JS.onload))),
  
  # Application title
  titlePanel("Explore Google Search Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("xaxis", "x-axis timeframe:",
                  choices = c("Average monthly searches" = "avg_monthly_searches", setNames(tolower(month.abb), month.name)),
                  selected = "avg_monthly_searches", multiple = FALSE),
      selectInput("yaxis", "Growth Metric:",
                  choices = c("Yearly growth" = "yo_y_change", "Three month growth" = "three_month_change"),
                  selected = "three_month_change", multiple = FALSE),
      selectInput("keyword", "Search Term",
                  choices = unique(df$keyword), selected = NULL, multiple = TRUE),
      conditionalPanel(condition = "input.chart_type == 1",
        sliderInput("num_posts", div(HTML("Number of Posts <i>(note log scale)</i>")),
                  min = 0.1, max = 5, step = 0.05, value = c(0.1,5), round = TRUE, tick = FALSE),
      sliderInput("growth_range", "Growth (%)",
                  min = -1.5, max = ceiling(log10(max(df %>% select(avg_monthly_searches, jan:dec)))), 
                  value = c(-1.5, ceiling(log10(max(df %>% select(avg_monthly_searches, jan:dec))))), 
                  tick = FALSE)),
      selectInput("static_keyword_callout", "Static Chart Callouts",
                  choices = unique(df$keyword), selected = NULL, multiple = TRUE),
      actionButton("shuffle_label", "Shuffle Label Position")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Dynamic Chart",
                           absolutePanel(top = 10, right = 10, downloadButton("scatter_download")),
                           plotlyOutput("keyword_dynamic_plot"),
                           dataTableOutput("filter_selected_tab1"),
                           downloadButton("data_download_tab1"),
                           value = 1),
                  tabPanel("Volume Over Time",
                           absolutePanel(top = 10, right = 10, downloadButton("vot_download")),
                           plotlyOutput("vot_plot"), dataTableOutput("filter_selected_tab2"),
                           downloadButton("data_download_tab2"), value = 2),
                  id = "chart_type")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # observe({
  #   max_xval <- ceiling(log10(max(df %>% select(avg_monthly_searches, jan:dec))))
  #  
  #   updateSliderInput(inputId = "num_posts", max = max_xval,
  #                     value = c(0.1, max_xval))
  #  
  # })
  
  observe({
    max_yval <- max(c(df$yo_y_change, df$three_month_change))
    min_yval <- min(c(df$yo_y_change, df$three_month_change))
    range_yval <- max_yval - min_yval
    updateSliderInput(inputId = "growth_range", min = min_yval - range_yval*0.05, max = max_yval + range_yval*0.05,
                      value = c(min_yval - range_yval*0.05, max_yval + range_yval*0.05))
  })
  
  random_state <- shiny::eventReactive(input$shuffle_label, {
    sample(1:1000, 1)
  }, ignoreNULL = FALSE)
  
  plot_df <- shiny::reactive({
    x_var <- rlang::sym(input$xaxis)
    y_var <- rlang::sym(input$yaxis)
    
    if(length(input$keyword)>0){
      df %>%
        arrange(log_avg_posts) %>% 
        mutate(rowid = row_number()) %>%
        filter(keyword %in% input$keyword) %>%
        mutate(x = !!x_var, y = !!y_var)
    } else {
      df %>%  
        arrange(log_avg_posts) %>% 
        mutate(rowid = row_number(),
               x = !!x_var, y = !!y_var)
      
    }
  })
  
  hover_df <- shiny::reactive({
    hover_info <- plot_df() %>%
      group_by(x,y) %>% mutate(group_number = cur_group_id(),
                               n = n()) %>%
      slice(1:3) %>%
      mutate(combined_info = case_when(n > 3 ~
                                         paste0("<b>Keyword (Average monthly searches):</b> ", 
                                                paste0("<br>", paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", "), "...")),
                                       TRUE ~ 
                                         paste0("<b>Keyword (Average monthly searches):</b> ", "<br>",
                                                paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", ")))) %>% 
      ungroup() %>% select(group_number, combined_info) %>% distinct()
    
    joined_df <- plot_df() %>%
      group_by(x,y) %>% mutate(group_number = cur_group_id()) %>%
      left_join(hover_info, by = "group_number")
    
    joined_df
  })
  
  df_highlighted <- shiny::reactive({ 
    plotly::event_data("plotly_selected")
  })
  
  coords <- shiny::reactive({
    # if (input$shuffle_label>1){
    #   rand_coord_opt2(length(input$static_keyword_callout), xmin = -200, xmax = 20, ymin = -250, ymax = 80, min_distance = 15, random_state = sample(0:100,1))
    # } else {
    rand_coord_opt2(length(input$static_keyword_callout), xmin = -200, xmax = 200, ymin = -150, ymax = 35, min_distance = 25, random_state = random_state())
    
    # }
    
  })
  
  label_df <- shiny::reactive({
    x_var <- rlang::sym(input$xaxis)
    y_var <- rlang::sym(input$yaxis)
    
    df %>% 
      filter(keyword %in% input$static_keyword_callout) %>%
      mutate(x = !!x_var, y = !!y_var,
             label = paste0(keyword,
                            "<br><i>3 month growth: ", three_month_change*100, "%</i>",
                            "<br><i> Yearly growth: ", yo_y_change*100, "%</i>"), 
             NA,
             ax_col = coords()[,1],
             ay_col = coords()[,2])
  })
  
  df_longer <- shiny::reactive({
    
    if (!is.null(df_highlighted())) {
      selected_indices <- df_highlighted()$pointNumber
      plot_df()[selected_indices + 1, ] %>%
        pivot_longer(jan:dec, values_to = "n", names_to = "month") %>%
        mutate(date = as.Date(paste0("01-", stringr::str_to_title(month), "-2022"), format = "%d-%b-%Y")) %>% 
        group_by(keyword)
    } 
    else {
      plot_df() %>% pivot_longer(jan:dec, values_to = "n", names_to = "month") %>%
        mutate(date = as.Date(paste0("01-", stringr::str_to_title(month), "-2022"), format = "%d-%b-%Y")) %>%
        group_by(keyword)
    }
  })
  
  basic_scatter <- shiny::reactive({

    plot_ly(data = hover_df(), x = ~x, y = ~y,
            type = "scatter", mode = "markers",
            color = ~log_avg_posts,
            size = ~log_avg_posts, sizes = c(5, 40),
            colors = viridis::viridis_pal(option = "A")(nrow(df))[plot_df()$rowid],
            hoverinfo = "text",
            text = ~combined_info,
            marker = list(opacity = 0.5,
                          showscale = FALSE,
                          sizemode = "diameter",
                          line = list(color = "black",
                                      width = 0.5)
            )) %>%
      colorbar(title = list(text = "Log of Average Posts")) %>%
      layout(
        dragmode = "lasso",
        shapes = list(hline(0)),
        margin = list(b=100),
        xaxis = list( type = "log",
                      title = "Number of Posts*",
                      range = c(x_min(), x_max()),
                      zeroline = FALSE, showline = TRUE, mirror = TRUE),
        yaxis = list(zeroline = FALSE, 
                     # tickformat = ',%',
                     showline = TRUE, mirror = TRUE,
                     range = c(input$growth_range),
                     title = "Growth"),
        showlegend = FALSE) %>%
      # group_by(xaxis, change_time) %>%
      add_annotations(text = "<span style='font-style:italic;'>*Note that this is a logarithmic scale, values 1, 10, 100, 1000 etc. are equally spaced on the graph.\nA log scale means that that keywords with 0 posts will not be displayed as points on the graph</span>",
                      # "*",
                      xref = "paper", yref = "paper",
                      x = 0.5, y = -0.3, size = 3, showarrow = FALSE,
                      font = list(size = 10)) %>%
      plotly::config(
        displaylogo = FALSE,
        edits = list(
          shapePosition = TRUE,
          annotation = TRUE
        )
      )
    
  })
  
  x_min <- shiny::reactive({
    log10(4*10^input$num_posts[1])
  })
  
  x_max <- shiny::reactive({
    log10(4*10^input$num_posts[2])
  })
  
  display_table <- shiny::reactive({
    # if (!is.null(df_highlighted())) {
      selected_indices <- df_highlighted()$pointNumber
      plot_df()[selected_indices + 1, ] %>%
        select(keyword, "Average Monthly Searches" = avg_monthly_searches,
               "Three Month Growth" = three_month_change, "Year on Year Growth" = yo_y_change,
               !!!setNames(colnames(plot_df())[6:17], month.name))
      
    # } else {
    #   plot_df() %>%
    #     select(keyword, "Average Monthly Searches" = avg_monthly_searches,
    #            "Three Month Growth" = three_month_change, "Year on Year Growth" = yo_y_change,
    #            !!!setNames(colnames(plot_df())[6:17], month.name)) 
    # }
  })
  
  output$filter_selected_tab1 <- DT::renderDataTable({
   display_table() %>%
      DT::datatable(options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$filter_selected_tab2 <- DT::renderDataTable({
    display_table() %>%
      DT::datatable(options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$keyword_dynamic_plot <- plotly::renderPlotly({
    if (length(input$static_keyword_callout) == 0){
      p <- basic_scatter()
      event_register(p, "plotly_selected")
      p
    } else {
      p <- basic_scatter() %>%
        add_annotations(
          x = log10(label_df()$x), y = label_df()$y,
          text = label_df()$label,
          font = list(size = 8, color = "grey80"),
          xref = "x",
          yref = "y",
          bgcolor = "rgba(255,255,255,0.6)",  # Background color of the box
          bordercolor = "rgba(0,0,0,0.6)",  # Border color of the box
          # ay = ~label_df()$ay_col,
          # ax = ~label_df()$ax_col,
          standoff = 0.3,
          borderwidth = 0.5) 
      
      event_register(p, "plotly_selected")
      p
    }
  })
  
  output$vot_plot <- plotly::renderPlotly({
    
    plot_ly(df_longer(), x = ~date, y = ~n, color = ~keyword,
            type = "scatter", mode = "markers + line",
            colors = viridis::viridis_pal(option = "A", end = 0.92)(nrow(df_longer()))) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Posts"))
  })
  
  output$data_download_tab1 <- shiny::downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      write.csv(display_table(), file)
    }
  )
  
  output$data_download_tab1 <- shiny::downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      write.csv(display_table(), file)
    }
  )
  
  output$data_download_tab1 <- shiny::downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      write.csv(display_table(), file)
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)
