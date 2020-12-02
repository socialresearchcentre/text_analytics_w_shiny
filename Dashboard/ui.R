ui <- dashboardPage(
  dashboardHeader(title = "Star Wars Reviews"),
  dashboardSidebar(
    sidebarMenu(
      main_menu_UI("tabs")

    )
  ),
  dashboardBody(
    
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    
    tags$head(        
      # In line styling
      tags$style(HTML(
        
        # Sidebar text colour
        '.skin-blue .main-sidebar .sidebar label {
          color: white !important;
        }'
      )
      )
    ),
    
    # Tabs ====
    tabItems(
      tabItem(
        tabName = 'about',
        about_tab_ui('about')
      ),
      
      tabItem(
        tabName = 'descriptives', 
        descriptives_tab_ui('tabs')
      ),
      
      tabItem(
        tabName = 'ldavis',
        ldaviz_tab_ui('tabs')
      ),
      
      tabItem(
        tabName = 'apply_topic',
        applytopics_tab_ui('tabs')
      )
      
    )
)
)

  