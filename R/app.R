# load df and libraries ----
library(shiny)
library(plotly)
library(tidyverse)
library(DT)

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
JS.observeSliderChange <-
  "
// execute whenever slider input changes
$(document).on('change', '#data_upload', function() {
// wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    // logifySlider('num_posts', sci = false)
    logifySlider('num_posts', sci = false);
}, 5)});
"
JS.sendCustomMessage <-
  "
$(document).on('shiny:connected', function(event) {
  Shiny.addCustomMessageHandler('jsCode', function(message) {
    eval(message.code);
  });
});
"
# ----

ui <- fluidPage(
  
  tags$head(tags$script(HTML(JS.logify))),
  tags$head(tags$script(HTML(JS.onload))),
  tags$head(tags$script(HTML(JS.sendCustomMessage))),

  # Application title
  titlePanel("Explore Google Search Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      shiny::fileInput("data_upload", "Upload your data", accept = c(".xlsx", ".csv", ".tsv"),
                       multiple = FALSE),
      shiny::selectInput("xaxis", "x-axis timeframe:",
                         choices = c("Average monthly searches" = "avg_monthly_searches", setNames(tolower(month.abb), month.name)),
                         selected = "avg_monthly_searches", multiple = FALSE),
      shiny::selectInput("yaxis", "Growth Metric:",
                         choices = c("Yearly growth" = "yo_y_change", "Three month growth" = "three_month_change"),
                         selected = "three_month_change", multiple = FALSE),
      shiny::selectInput("keyword", "Search Term",
                         choices = NULL, selected = NULL, multiple = TRUE),
      shiny::conditionalPanel(condition = "input.chart_type == 1",
                              sliderInput("num_posts", div(HTML("Number of Posts <i>(note log scale)</i>")),
                                          min = 0.1, max = 3, step = 0.005, value = c(0.1,3), round = TRUE, tick = FALSE),
                              sliderInput("growth_range", "Growth (%)",
                                          min = -1.5, max = 10, 
                                          value = c(-1.5, 10), 
                                          tick = FALSE)),
      shiny::selectInput("grouping_variable", "Grouping Variable",
                         choices = NULL, selected = NULL, multiple = TRUE),
      shiny::selectInput("static_keyword_callout", "Static Chart Callouts",
                         choices = NULL, selected = NULL, multiple = TRUE),
      actionButton("shuffle_label", "Shuffle Label Position")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      shiny::tabsetPanel(type = "tabs",
                         tabPanel("hover chart", dataTableOutput("hover_chart")),
                         tabPanel("Dynamic Chart",
                                  # absolutePanel(top = 10, right = 10, downloadButton("scatter_download")),
                                  plotlyOutput("keyword_dynamic_plot"),
                                  dataTableOutput("filter_selected_tab1"),
                                  downloadButton("data_download_tab1", label = "Download Data Table"),
                                  value = 1),
                         tabPanel("Volume Over Time",
                                  # absolutePanel(top = 10, right = 10, downloadButton("vot_download")),
                                  plotlyOutput("vot_plot"), 
                                  dataTableOutput("filter_selected_tab2"),
                                  downloadButton("data_download_tab2",  label = "Download Data Table"), 
                                  value = 2),
                         id = "chart_type")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$hover_chart <- renderDataTable({
    hover_df()
  })
  
  # Reactive values ----
  data <- reactive({
    req(input$data_upload)
    
    ext <- tools::file_ext(input$data_upload$name)
    df <- switch(ext,
                 csv = vroom::vroom(input$data_upload$datapath, delim = ","),
                 tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                 xlsx = readxl::read_xlsx(input$data_upload$datapath),
                 validate("Invalid file; Please upload a .xlsx, .csv or .tsv file")
    ) %>%
      janitor::row_to_names(row_number = 36, remove_rows_above = FALSE) %>% # column names
      janitor::clean_names() %>%
      filter(avg_monthly_searches != 0) %>%
      mutate_at(15:26, as.numeric) %>% # monthly searches stored as character
      mutate(yo_y_change = as.numeric(str_remove_all(yo_y_change, "%")),
             three_month_change = as.numeric(str_remove_all(three_month_change, "%")),
             avg_monthly_searches = as.numeric(avg_monthly_searches),
             log_avg_posts = log(avg_monthly_searches)) %>%
      dplyr::select(-c(competition:in_plan)) %>%
      rename_with(~sub("searches_(\\w+)_2023", "\\1", .), starts_with("searches_")) %>%
      drop_na()
    
  })
  
  observe({
    # dropdowns
    keyword_choices <- unique(data()$keyword)
    grouping_choices <- group_info() %>% 
      filter(!word %in% tm::stopwords(kind = "SMART")) %>%
      arrange(desc(n)) %>% pull(word) %>% unique()

    updateSelectInput(inputId = "keyword", choices = keyword_choices)
    updateSelectInput(inputId = "static_keyword_callout", choices = keyword_choices)
    updateSelectInput(inputId = "grouping_variable", selected = NULL, choices = grouping_choices)
    
    # sliders 
    max_yval <- max(c(data()$yo_y_change, data()$three_month_change))
    min_yval <- min(c(data()$yo_y_change, data()$three_month_change))
    range_yval <- max_yval - min_yval
    updateSliderInput(inputId = "growth_range", min = min_yval - range_yval*0.05, max = max_yval + range_yval*0.05,
                      value = c(min_yval - range_yval*0.05, max_yval + range_yval*0.05))
    
    # max_xval <- ceiling(log10(max(data() %>% select(avg_monthly_searches, jan:dec))))
    # updateSliderInput(inputId = "num_posts", max = max_xval, value = c(0.1, max_xval))
  })

  observeEvent(input$data_upload, {
    max_searches <- max(data() %>% select(avg_monthly_searches, jan:dec))
    normaliser <- 10^(floor(log10(abs(max_searches))))
    adjusted_max_searches <- ceiling(max_searches/normaliser)*normaliser
    max_xval <- floor(log10(adjusted_max_searches))
    updateSliderInput(inputId = "num_posts", max = max_xval, value = c(0.1, max_xval))
    
    js <- "
  setTimeout(function() {
    logifySlider('num_posts', sci = false);
  }, 5);
  "
    session$sendCustomMessage("jsCode", list(code = js))
  })
  
  random_state <- shiny::eventReactive(input$shuffle_label, {
    sample(1:1000, 1)
  }, ignoreNULL = FALSE)
  
  group_info <- reactive({
    data() %>% 
      select(keyword) %>% 
      tidytext::unnest_tokens(word, keyword, drop = FALSE) %>% 
      group_by(word) %>%
      filter(!word %in% tm::stopwords(kind = "SMART")) %>% 
      mutate(n=n()) 
  })
  
  hover_df <- shiny::reactive({
    
    x_var <- rlang::sym(input$xaxis)
    y_var <- rlang::sym(input$yaxis)
    
    if(length(input$keyword)>0){
      plot_df <- data() %>%
        arrange(log_avg_posts) %>% 
        mutate(rowid = row_number()) %>%
        filter(keyword %in% input$keyword) %>%
        mutate(x = !!x_var, y = !!y_var)
    } else {
      plot_df <- data() %>%  
        arrange(log_avg_posts) %>% 
        mutate(rowid = row_number(),
               x = !!x_var, y = !!y_var)
      
    }
    
    hover_info <- plot_df %>%
      group_by(x,y) %>% mutate(group_number = cur_group_id(), n = n()) %>%
      slice(1:3) %>%
      mutate(combined_info = case_when(n > 3 ~
                                         paste0("<b>Keyword (Average monthly searches):</b> ", 
                                                paste0("<br>", paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", "), "...")),
                                       TRUE ~ 
                                         paste0("<b>Keyword (Average monthly searches):</b> ", "<br>",
                                                paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", ")))) %>% 
      ungroup() %>% select(group_number, combined_info) %>% distinct()
    
    if(length(input$grouping_variable > 0)){

      keywords_to_highlight <- group_info() %>% 
        filter(word %in% input$grouping_variable) %>% 
        group_by(keyword) %>%
        arrange(word) %>%
        distinct(keyword, word) %>%
        mutate(group_var = paste0(word, collapse = ", "))

      joined_df <- plot_df %>%
        group_by(x,y) %>% mutate(group_number = cur_group_id()) %>%
        left_join(hover_info, by = "group_number") %>% ungroup() %>%
        left_join(keywords_to_highlight %>% select(keyword, group_var), by = "keyword") %>%
        # mutate(selected_terms = case_when(keyword %in% keywords_to_highlight ~ input$grouping_variable, TRUE ~ "Other")) %>%
        mutate(group_var = if_else(is.na(group_var), "Other", group_var)) %>%
        mutate(rowid = row_number())
    } else {
      joined_df <- plot_df %>%
        group_by(x,y) %>% mutate(group_number = cur_group_id()) %>%
        left_join(hover_info, by = "group_number") %>% ungroup() %>%
        mutate(group_var = 1) %>%
        mutate(rowid = row_number())
    }
    
    
    joined_df
  }) 
  
  df_highlighted <- shiny::reactive({ 
    plotly::event_data("plotly_selected")
  })
  
  coords <- shiny::reactive({
    rand_coord_opt2(length(input$static_keyword_callout), xmin = -200, xmax = 200, ymin = -150, ymax = 35, min_distance = 25, random_state = random_state())
  })
  
  label_df <- shiny::reactive({
    x_var <- rlang::sym(input$xaxis)
    y_var <- rlang::sym(input$yaxis)
    
    data()%>% 
      filter(keyword %in% input$static_keyword_callout) %>%
      mutate(x = !!x_var, y = !!y_var,
             label = paste0(keyword,
                            "<br><i>3 month growth: ", three_month_change, "%</i>",
                            "<br><i> Yearly growth: ", yo_y_change, "%</i>"), 
             NA,
             ax_col = coords()[,1],
             ay_col = coords()[,2])
  })
  
  df_longer <- shiny::reactive({
    hover_df()[hover_df()$rowid %in% df_highlighted()$customdata, ] %>%
      pivot_longer(jan:dec, values_to = "n", names_to = "month") %>%
      mutate(date = as.Date(paste0("01-", stringr::str_to_title(month), "-2022"), format = "%d-%b-%Y")) %>%
      group_by(keyword)
    
  })
  
  basic_scatter <- shiny::reactive({
    
    plot_ly(data = hover_df(), x = ~x, y = ~y,
            type = "scatter", mode = "markers",
            # color = ~log_avg_posts,
            color = ~as.factor(group_var),
            customdata = ~rowid,
            size = ~log_avg_posts, sizes = c(5, 30),
            # colors = viridis::viridis_pal(option = "A")(n_distinct(hover_df()$group_var)),
            hoverinfo = "text",
            text = ~combined_info,
            marker = list(opacity = 0.5,
                          # size = 12,
                          showscale = FALSE,
                          showlegend = TRUE,
                          sizemode = "diameter",
                          line = list(color = "black",
                                      width = 0.5)
            )) %>%
      layout(
        dragmode = "lasso",
        shapes = list(hline(0)),
        margin = list(b=100),
        xaxis = list( type = "log",
                      title = ~ifelse(input$xaxis == "avg_monthly_searches", "Average Number of Keyword Searches", 
                                      paste0(month.name[match(input$xaxis, tolower(month.abb))], " Number of Keyword Searches")),
                      range = c(x_min(), x_max()),
                      zeroline = FALSE, showline = TRUE, mirror = TRUE),
        yaxis = list(zeroline = FALSE, 
                     ticksuffix = "%",
                     showline = TRUE, mirror = TRUE,
                     range = c(input$growth_range),
                     title = ifelse(input$yaxis == "three_month_change", "Three Month Growth", "Year-on-Year Growth")),
        showlegend = ~ifelse(all(group_var == 1), FALSE, TRUE),
        legend = list(title = list(
          text = "<b>Word(s) in search term:</b>",
          font = list(size = 12)
        ) )) %>%
      add_annotations(text = "<span style='font-style:italic;text-decoration:underline;'>Note:</span><span style='font-style:italic;'> Bubble size corresponds to average monthly searches, a larger bubble represents a larger average monthly search</span>\n<span style='font-style:italic;'>*This is a logarithmic scale, values 1, 10, 100, 1000 etc. are equally spaced on the graph.\nA log scale means that that keywords with 0 posts will not be displayed as points on the graph</span>",
                      # "*",
                      xref = "paper", yref = "paper",
                      x = 0.5, y = -0.35, size = 3, showarrow = FALSE,
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
    selected_indices <- df_highlighted()$customdata
    hover_df()[hover_df()$rowid %in% selected_indices, ] %>%
      select(keyword, "Average Monthly Searches" = avg_monthly_searches,
             "Three Month Growth" = three_month_change, "Year on Year Growth" = yo_y_change,
             !!!setNames(colnames(hover_df())[6:17], month.name))
  })
  
  # Outputs ----
  
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
          ay = ~label_df()$ay_col,
          ax = ~label_df()$ax_col,
          standoff = 0.3,
          borderwidth = 0.5) 
    }
    
    event_register(p, "plotly_selected")
    p
  })
  
  output$vot_plot <- plotly::renderPlotly({
    
    plot_ly(df_longer(), x = ~date, y = ~jitter(n), color = ~keyword,
            type = "scatter", mode = "markers + line",
            hovertemplate = ~paste("<b>", keyword, "</b><br>",
                                   "Month: %{x|%b}, Searches:", n, "<br><extra></extra>"),
            colors = viridis::viridis_pal(option = "A", end = 0.92)(nrow(df_longer()))) %>%
      layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Searches"))
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
  
  # end ----
  
}


# Run the application 
shinyApp(ui = ui, server = server)
