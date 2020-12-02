# Side Menu ---------------------------------------------------------------
main_menu_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    menuItem("About", tabName = "about", icon = icon("book")),      
    menuItem("Filters", tabName = "filters", startExpanded = FALSE,
             icon = icon("filter"), global_filters_UI(ns("filtmenu1"))),
    menuItem("Compare reviews", tabName = "descriptives", icon = icon("chart-bar")),
   menuItem("Explore topics", tabName = "ldavis", icon = icon("chart-bar")),
    menuItem("Topics against responses", tabName = 'apply_topic', icon = icon("wordpress-simple"))
  )
}


# About Tab ---------------------------------------------------------------
about_tab_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    mainPanel(img(src = "prequels.png", height = 458 * 1.5, width = 1010 * 1.5))
  )
  
}

# Descriptives Tab --------------------------------------------------------
descriptives_tab_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    
    # Filters
    fluidRow(
      column(4,
             box(
               title = textOutput(ns("filt1_title")),
               "Set filters to explore and compare reviews",
               summaryfilter_UI(ns("filt1"), 1, 11), solidHeader = TRUE, collapsed = TRUE, 
               collapsible = TRUE, width = '100%'
               
             )
             
      ),
      column(4,
             box(
               title = textOutput(ns("filt2_title")),
               "Set filters to explore and compare the topics",
               summaryfilter_UI(ns("filt2"), 2, 11), solidHeader = TRUE, collapsed = TRUE, 
               collapsible = TRUE, width = '100%'
             )
             
      ),

    ),
    
    # Plot outputs
    fluidRow(
      column(4,
             box(width = '100%', status = 'primary',
                 plotlyOutput(ns("plotly_summary1")) %>%
                   shinycssloaders::withSpinner())),
      column(4,
             box(width = '100%', status = 'primary',
                 plotlyOutput(ns("plotly_summary2")) %>%
                   shinycssloaders::withSpinner()))
     )

  )
}

descriptives_tab_server <- function(id) {
  moduleServer(                                   
    
    id,   
    
    function(input, output, session) {     
        
      # Global filters
      global_filters_server("filtmenu1")
      
      # Reactive Titles
      output$filt1_title <- renderText({

        return_grouping_("filt1")()
        
      })
      
      output$filt2_title <- renderText({

        return_grouping_("filt2")()
      })
      
      # Filter to use
      grouping1 <- return_grouping_("filt1")
      grouping2 <- return_grouping_("filt2")

        
       # Data to use
      dat1_summary <- load_dat_react_("filtmenu1", grouping1, type = "data")
      dat2_summary <- load_dat_react_("filtmenu1", grouping2, type = "data")
 
        

        # Plots
        output$plotly_summary1 <- renderPlotly({
          validate(
            need(nrow(dat1_summary()) > 0, 'No data found')
          )

          p <- plot_summary(dat1_summary(), blue_cols)
          plotly_plot(p)
        })
 
        output$plotly_summary2 <- renderPlotly({
          validate(
            need(nrow(dat2_summary()) > 0, 'No data found')
          )

          p <- plot_summary(dat2_summary(), green_cols)
          plotly_plot(p)
        })
    }
  )
  
  
}


# LDA VIZ -----------------------------------------------------------------

ldaviz_tab_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    # Description and filter
    fluidRow(
      column(6, 
             div(

               fluidRow(
                 box(
                   title = "Rating filter", 
                   "Use this page to get a more detailed understanding of the topics 
                     and how they relate to one another. Where ever topics are clustered together, 
                     this would suggest that there is some semantic relationship  between.",
                   br(), br(),
                   summaryfilter_UI(ns("filt_lda"), 1, 4, 0), collapsible = TRUE, width = '100%'
                 )
               ), 
               style = "height:250px;")
      ) 
      

     ), 

   # LDAvis output
    fluidRow(
      column(12,
             box(title = "LDAvis", visOutput(ns('LDAvis1')), width = "100%")
      )
     )

  )
  
}


ldaviz_tab_server <- function(id) {
  moduleServer(                                   
    
    id,   
    
    function(input, output, session) {     
    

      # Grouping filter
      groupinglda <- return_grouping_("filt_lda")

      
      # Data
      dat1_ldavis <- load_dat_react_("filtmenu1", groupinglda, type = "ldavis")
      summary_ldavis <- load_dat_react_("filtmenu1", groupinglda, type = "data")
      
      # LDA vis output
      output$LDAvis1 <- renderVis({

        
        dat1_ldavis()

        
      })
      
      output$summary_table <- renderDataTable({
        datatable(summary_ldavis() %>% 
                    select(topic, summary, prevalence) %>% 
                    arrange(desc(prevalence)) %>% 
                    rename_all(toupper), rownames = FALSE)
      })
      
    }
    
  )
  
}




# Apply Topics ------------------------------------------------------------

applytopics_tab_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    # Filters
    ## Rating
    fluidRow(
      column(6, 
             
             box(
               title = "Filter by rating", width = '100%', collapsible = TRUE,
               
               summaryfilter_UI(ns("filt_table"), 1, 12, offset = 0), 
             )
      ),
     ## Topic
      column(6,
             box(
               title = "Filter by topic", width = '100%', collapsible = TRUE,
               
               uiOutput(ns("main_topics")), 
             )
      ), 
      
    ),
    
    # Verbatim table
    fluidRow(
      
      column(12,
             box(title = "Topic", solidHeader = TRUE,
                 width = "100%",
                 shinycssloaders::withSpinner(dataTableOutput(ns("main_topics_table")))
             )
      )
    ),
    
    
  )
  
  
}


applytopics_tab_server <- function(id) {
  
  moduleServer(
    id, 
    
    function(input, output, session) {
      
      ns <- session$ns
      

      # Grouping filter
       grouping_table <- return_grouping_("filt_table")
       
      
       # Data
       main_topic <- load_dat_react_("filtmenu1", grouping_table, type = "data")
       main_topic_sub <- topic_table_sub("topic", topic_table = main_topic,
                                          reactive({input$topic}))
    
       # Dynamic topic UI
       output$main_topics <- renderUI({
         ns <- session$ns
         topics <- unique(main_topic() %>% rename_all(tolower) %>% pull("summary")) # Pull the summarised topic name
         selectInput(ns("topic"),
                     label = "Topic:",
                     choices = topics,
                     selected = topics[1])
       })

       # Reactive verbatim table
       output$main_topics_table <- renderDataTable({
         datatable(main_topic_sub() %>% 
                     transmute(topic_strength = value,
                                reviewText, 
                                reviewerName, 
                                reviewer_rating = overall), rownames = FALSE)
       })
       
    }
  )
  
  
}
