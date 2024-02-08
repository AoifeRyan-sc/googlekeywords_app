
#' Call function to launch app
#'
#' @return launches app in which you can upload data exported from google keywords for analysis
#' @export
#'
#' @examples \dontrun{run_app()}
run_app <- function(){
 
  # logifySlider javascript function ----
  JS.logify <-
    "
// function to logify a shiny::sliderInput
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
  
  # call logifySlider for each relevant shiny::sliderInput ----
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
  
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$script(htmltools::HTML(JS.logify))),
    shiny::tags$head(shiny::tags$script(htmltools::HTML(JS.onload))),
    shiny::tags$head(shiny::tags$script(htmltools::HTML(JS.sendCustomMessage))),
    
    # Application title
    shiny::titlePanel("Explore Google Search Data"),
    
    # Sidebar with a slider input for number of bins 
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("data_upload", "Upload your data", accept = c(".xlsx", ".csv", ".tsv"),
                         multiple = FALSE),
        shiny::selectInput("xaxis", "x-axis timeframe:",
                           choices = c("Average monthly searches" = "avg_monthly_searches", stats::setNames(tolower(month.abb), month.name)),
                           selected = "avg_monthly_searches", multiple = FALSE),
        shiny::selectInput("yaxis", "Growth Metric:",
                           choices = c("Yearly growth" = "yo_y_change", "Three month growth" = "three_month_change"),
                           selected = "three_month_change", multiple = FALSE),
        shiny::selectInput("keyword", "Search Term",
                           choices = NULL, selected = NULL, multiple = TRUE),
        shiny::conditionalPanel(condition = "input.chart_type == 1",
                                shiny::sliderInput("num_posts", htmltools::div(htmltools::HTML("Number of Posts <i>(note log scale)</i>")),
                                            min = 0.1, max = 3, step = 0.005, value = c(0.1,3), round = TRUE, tick = FALSE),
                                shiny::sliderInput("growth_range", "Growth (%)",
                                            min = -1.5, max = 10, 
                                            value = c(-1.5, 10), 
                                            tick = FALSE)),
        shiny::selectInput("grouping_variable", "Grouping Variable",
                           choices = NULL, selected = NULL, multiple = TRUE),
        shiny::selectInput("static_keyword_callout", "Static Chart Callouts",
                           choices = NULL, selected = NULL, multiple = TRUE),
        shiny::actionButton("shuffle_label", "Shuffle Label Position")
      ),
      
      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::tabsetPanel(type = "tabs",
                           shiny::tabPanel("Dynamic Chart",
                                    # absolutePanel(top = 10, right = 10, shiny::downloadButton("scatter_download")),
                                     plotly::plotlyOutput("keyword_dynamic_plot"),
                                     DT::dataTableOutput("filter_selected_tab1"),
                                    # verbatimTextOutput("test"),
                                    shiny::downloadButton("data_download_tab1", label = "Download Data Table"),
                                    value = 1),
                           shiny::tabPanel("Volume Over Time",
                                    # absolutePanel(top = 10, right = 10, shiny::downloadButton("vot_download")),
                                     plotly::plotlyOutput("vot_plot"), 
                                     DT::dataTableOutput("filter_selected_tab2"),
                                    shiny::downloadButton("data_download_tab2",  label = "Download Data Table"), 
                                    value = 2),
                           id = "chart_type")
      )
    )
  )
  
  # Define server logic shiny::required to draw a histogram
  server <- function(input, output, session) {
    
    # Reactive values ----
    data <- shiny::reactive({
      shiny::req(input$data_upload)

      ext <- tools::file_ext(input$data_upload$name)
      df <- switch(ext,
                   csv = vroom::vroom(input$data_upload$datapath, delim = ","),
                   tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                   xlsx = readxl::read_xlsx(input$data_upload$datapath),
                   shiny::validate("Invalid file; Please upload a .xlsx, .csv or .tsv file")
      ) %>%
        janitor::row_to_names(row_number = 36, remove_rows_above = FALSE) %>% # column names
        janitor::clean_names() %>%
        dplyr::filter(avg_monthly_searches != 0) %>%
        dplyr::mutate_at(15:26, as.numeric) %>% # monthly searches stored as character
        dplyr::mutate(yo_y_change = as.numeric(stringr::str_remove_all(yo_y_change, "%")),
               three_month_change = as.numeric(stringr::str_remove_all(three_month_change, "%")),
               avg_monthly_searches = as.numeric(avg_monthly_searches),
               log_avg_posts = log(avg_monthly_searches)) %>%
        dplyr::select(-c(competition:in_plan)) %>%
        dplyr::rename_with(~sub("searches_(\\w+)_2023", "\\1", .), tidyselect::starts_with("searches_")) %>%
        tidyr::drop_na()

    })

    shiny::observe({
      # dropdowns
      keyword_choices <- unique(data()$keyword)
      grouping_choices <- group_info() %>%
        dplyr::filter(!word %in% tm::stopwords(kind = "SMART")) %>%
        dplyr::arrange(dplyr::desc(n)) %>% dplyr::pull(word) %>% unique()

      shiny::updateSelectInput(inputId = "keyword", choices = keyword_choices)
      shiny::updateSelectInput(inputId = "static_keyword_callout", choices = keyword_choices)
      shiny::updateSelectInput(inputId = "grouping_variable", selected = NULL, choices = grouping_choices)

      # sliders
      max_yval <- max(c(data()$yo_y_change, data()$three_month_change))
      min_yval <- min(c(data()$yo_y_change, data()$three_month_change))
      range_yval <- max_yval - min_yval
      shiny::updateSliderInput(inputId = "growth_range", min = min_yval - range_yval*0.05, max = max_yval + range_yval*0.05,
                        value = c(min_yval - range_yval*0.05, max_yval + range_yval*0.05))

    })

    shiny::observeEvent(input$data_upload, {
      max_searches <- max(data() %>% dplyr::select(avg_monthly_searches, jan:dec))
      normaliser <- 10^(floor(log10(abs(max_searches))))
      adjusted_max_searches <- ceiling(max_searches/normaliser)*normaliser
      max_xval <- floor(log10(adjusted_max_searches))
      shiny::updateSliderInput(inputId = "num_posts", max = max_xval, value = c(0.1, max_xval))

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

    group_info <- shiny::reactive({
      data() %>%
        dplyr::select(keyword) %>%
        tidytext::unnest_tokens(word, keyword, drop = FALSE) %>%
        dplyr::group_by(word) %>%
        dplyr::filter(!word %in% tm::stopwords(kind = "SMART")) %>%
        dplyr::mutate(n=dplyr::n())
    })

  hover_df <- shiny::reactive({

    x_var <- rlang::sym(input$xaxis)
    y_var <- rlang::sym(input$yaxis)

    if(length(input$keyword)>0){
      plot_df <- data() %>%
        dplyr::arrange(log_avg_posts) %>%
        dplyr::mutate(rowid = dplyr::row_number()) %>%
        dplyr::filter(keyword %in% input$keyword) %>%
        dplyr::mutate(x = !!x_var, y = !!y_var)
    } else {
      plot_df <- data() %>%
        dplyr::arrange(log_avg_posts) %>%
        dplyr::mutate(rowid = dplyr::row_number(),
               x = !!x_var, y = !!y_var)

    }

    hover_info <- plot_df %>%
      dplyr::group_by(x,y) %>% dplyr::mutate(group_number = dplyr::cur_group_id(), n = dplyr::n()) %>%
      dplyr::slice(1:3) %>%
      dplyr::mutate(combined_info = dplyr::case_when(n > 3 ~
                                         paste0("<b>Keyword (Average monthly searches):</b> ",
                                                paste0("<br>", paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", "), "...")),
                                       TRUE ~
                                         paste0("<b>Keyword (Average monthly searches):</b> ", "<br>",
                                                paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", ")))) %>%
      dplyr::ungroup() %>% dplyr::select(group_number, combined_info) %>% dplyr::distinct()

    if(length(input$grouping_variable > 0)){

      keywords_to_highlight <- group_info() %>%
        dplyr::filter(word %in% input$grouping_variable) %>%
        dplyr::group_by(keyword) %>%
        dplyr::arrange(word) %>%
        dplyr::distinct(keyword, word) %>%
        dplyr::mutate(group_var = paste0(word, collapse = ", "))

      joined_df <- plot_df %>%
        dplyr::group_by(x,y) %>% dplyr::mutate(group_number = dplyr::cur_group_id()) %>%
        dplyr::left_join(hover_info, by = "group_number") %>% dplyr::ungroup() %>%
        dplyr::left_join(keywords_to_highlight %>% dplyr::select(keyword, group_var), by = "keyword") %>%
        # dplyr::mutate(selected_terms = dplyr::case_when(keyword %in% keywords_to_highlight ~ input$grouping_variable, TRUE ~ "Other")) %>%
        dplyr::mutate(group_var = dplyr::if_else(is.na(group_var), "Other", group_var)) %>%
        dplyr::mutate(rowid = dplyr::row_number())
    } else {
      joined_df <- plot_df %>%
        dplyr::group_by(x,y) %>% dplyr::mutate(group_number = dplyr::cur_group_id()) %>%
        dplyr::left_join(hover_info, by = "group_number") %>% dplyr::ungroup() %>%
        dplyr::mutate(group_var = 1) %>%
        dplyr::mutate(rowid = dplyr::row_number())
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
      dplyr::filter(keyword %in% input$static_keyword_callout) %>%
      dplyr::mutate(x = !!x_var, y = !!y_var,
             label = paste0(keyword,
                            "<br><i>3 month growth: ", three_month_change, "%</i>",
                            "<br><i> Yearly growth: ", yo_y_change, "%</i>"),
             NA,
             ax_col = coords()[,1],
             ay_col = coords()[,2])
  })

  df_longer <- shiny::reactive({
    hover_df()[hover_df()$rowid %in% df_highlighted()$customdata, ] %>%
      tidyr::pivot_longer(jan:dec, values_to = "n", names_to = "month") %>%
      dplyr::mutate(date = as.Date(paste0("01-", stringr::str_to_title(month), "-2022"), format = "%d-%b-%Y")) %>%
      dplyr::group_by(keyword)

  })

  basic_scatter <- shiny::reactive({

    plotly::plot_ly(data = hover_df(), x = ~x, y = ~y,
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
      plotly::layout(
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
      plotly::add_annotations(text = "<span style='font-style:italic;text-decoration:underline;'>Note:</span><span style='font-style:italic;'> Bubble size corresponds to average monthly searches, a larger bubble represents a larger average monthly search</span>\n<span style='font-style:italic;'>*This is a logarithmic scale, values 1, 10, 100, 1000 etc. are equally spaced on the graph.\nA log scale means that that keywords with 0 posts will not be displayed as points on the graph</span>",
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
      dplyr::select(keyword, "Average Monthly Searches" = avg_monthly_searches,
             "Three Month Growth" = three_month_change, "Year on Year Growth" = yo_y_change,
             !!!stats::setNames(colnames(hover_df())[6:17], month.name))
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
        plotly::add_annotations(
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

    plotly::event_register(p, "plotly_selected")
    p
  })

  output$vot_plot <- plotly::renderPlotly({

    plotly::plot_ly(df_longer(), x = ~date, y = ~jitter(n), color = ~keyword,
            type = "scatter", mode = "markers + line",
            hovertemplate = ~paste("<b>", keyword, "</b><br>",
                                   "Month: %{x|%b}, Searches:", n, "<br><extra></extra>"),
            colors = viridis::viridis_pal(option = "A", end = 0.92)(nrow(df_longer()))) %>%
      plotly::layout(xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Searches"))
  })

  output$data_download_tab1 <- shiny::downloadHandler(
    filename = function() {
      paste0("selected_data_", format(Sys.time(), "%d-%m-%Y_%H:%M:%S"), ".csv")
    },
    content = function(file) {
      utils::write.csv(display_table(), file, row.names = FALSE)
    }
  )
  
  output$data_download_tab2 <- shiny::downloadHandler(
    filename = function() {
      paste0("selected_data_", format(Sys.time(), "%d-%m-%Y_%H.%M.%S"), ".csv")
    },
    content = function(file) {
      utils::write.csv(display_table(), file, row.names = FALSE)
    }
  )
  

    # end ----
    
  }
  
  
  # Run the application 
  shiny::shinyApp(ui = ui, server = server)
}