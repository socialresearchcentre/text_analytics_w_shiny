# Modules ====

global_filters_UI <- function(id) {
  # Side menu filter UI
  
  ns <- NS(id)
  tagList(
      map(

        filter_spec[['variables']] %>% filter(use %in% 1) %>% pull(variable),
        
        function(var) {
          selectInput(
            inputId = ns(paste0("select_filter_", var)), 
            label = filter_spec[['variables']] %>% filter(variable == var) %>% pull(display.name), 
            choices= filter_spec[['values']] %>% filter(variable == var) %>% pull(display.name),
            selected= filter_spec[['values']] %>% filter(variable == var, default_selected == 1) %>% pull(display.name)
          )
        }
      )
  )
}



global_filters_server <- function(id) {
  # Side menu filter module
  
  moduleServer(
    id,
    
    function(input, output, session) {
      reactive({

        tmp <- main_text
        
        for (var in (filter_spec[['variables']] %>% filter(use %in% 1) %>% pull(variable))) {
          
          input_id <- paste0("select_filter_", var)
          vals <- filter_spec[['values']] %>% 
            filter(display.name %in% input[[input_id]]) %>%
            pull(value)
          
           
          tmp <- tmp %>%
            filter(!!sym(var) %in% vals)
        }
        
        tmp
      })    
    }
  )
}

summaryfilter_UI <- function(id, version, width, offset = 1) {
  # UI for filtering grouping variable
  
  ns <- NS(id)
  
  fluidRow(
    

    column(width, offset = offset,
           selectInput(ns('group_var'),
                       label = 'Rating:',
                       choices = unique(dat$group_var),
                       selected = case_when(version == 1 ~ unique(dat$group_var)[1],
                                            version == 2 ~ unique(dat$group_var)[2]),
                       width = '90%',
                       multiple = FALSE)
    )
  )
}





# Modules Server Functions ===
load_dat_react_ <- function(id, group_var_, type){
  # Filter data to grouping var
  
  moduleServer(                                    
    
    id,                                           
    
    function(input, output, session){              
  
      
    
     if (type == "data") {
       summary <- reactive(dat %>%
                             filter(group_var %in% group_var_()
                             )
       )
 
     }  
        
      if (type == "ldavis") {

        summary <- reactive({movie_ldavis[[glue("group_var-{group_var_()}")]]})

      }
      
      
      
      return(summary)
      
    }
  )
}


return_grouping_ <- function(id) {
  # Return any grouping variable from summaryfilter_UI
  
  moduleServer(                                   
    id,                                           
    
    function(input, output, session){              
      
      observe({input$group_var})  
      out <- reactive({input$group_var})

      return(out)
      
    }
  )
}


ldavis_ <- function(id){                   
  # Return a reactive expression to use inside renderTable
  
  moduleServer(                                   
    
    id,                                           
    
    function(input, output, session){              
      


      dat <- reactive() 
      
      return(dat)
      
    }
  )
}


topic_table_sub <- function(id, topic_table, topic) {
  # Return reactive table containing verbatims corresponding to a topic
  
  moduleServer(                                   
    
    id,                                            
    
    function(input, output, session){              
      

      
      reactive({
        topic_table() %>%
          filter(summary %in% topic()) 
      })
      
      
    }
  )
}


get_title <- function(id, group_var_) {
  # Return reactive text corresponding to name of grouping variable
  
  moduleServer(
    id, 
    function(input, output, session) {
      reactive(group_var_())
    }
  )
  
  
}


