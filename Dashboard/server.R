server <- function(input, output, session) {
  

  # Main tabs ====

  descriptives_tab_server('tabs')
  
  ldaviz_tab_server('tabs')
  
  applytopics_tab_server('tabs')
  

  
  
}
