# Functions ====
read_spec <- function(fp) {
  ## Read a spreadsheet
  
  out <- list()
  sheets <- getSheetNames(fp)
  for (sheetName in sheets) {
    out[[sheetName]] <- read.xlsx(fp, sheet = sheetName)
  }
  
  return(out)
  
}





plot_summary <- function(dat, colour) {
  #browser()
  dat <- dat %>% 
    mutate(top_terms = str_split(top_terms, ",\\s")) %>% 
    unnest(top_terms) %>% 
    group_by(topic, summary, coherence, prevalence#, year, stage, var
    ) %>% 
    mutate(rnk = 1:n()) %>% 
    filter(rnk <= 10) %>%
    summarise(top_terms = paste(top_terms, collapse = ", ")) %>% 
    mutate(top_terms = str_replace_all(top_terms, "(([^,]*,){4})([^,]*)(,\\s)", "\\1\\3\n")) %>% 
    ungroup()
  
  plot_summary <- function(dat, colours) {
    dat %>% 
      mutate(topic = fct_reorder(factor(topic), prevalence)) %>% 
      ggplot(aes( prevalence, topic, fill = factor(topic), text = str_glue("<b>Summary: {summary}</b>
                                                    Top terms: {top_terms}"))) +
      # geom_bar(position = "dodge", stat = "identity", show.legend = FALSE) +
      geom_segment(aes(x = 0, xend = prevalence, y = topic, yend = topic),  linetype = 2, colour = "black", alpha = 0.2) +
      geom_point(size = 4, alpha = 0.6) +
      geom_text(aes(label = toupper(summary), x = max(prevalence) * 0.5), nudge_y = 0.3) + 
      labs(y = "Topics", 
           x = "Prevalence"#,
           # title = str_glue("{var}, {year}, {stage}")
      ) + 
      theme_minimal() + 
      #  scale_fill_manual(values = colours) +
      theme(legend.position = "none", 
            axis.text.y = element_blank(), panel.grid = element_blank()
      )
  }
  
  
  plot_summary(dat, colour)
}



plotly_plot <- function(plot)
  ggplotly(plot, tooltip = "text") %>% 
  config(displayModeBar = FALSE)


blue_cols <- c( "#6495ED" , "#6698FF", 
                "#38ACEC", "#56A5EC", 
                "#5CB3FF", "#3BB9FF", 
                "#79BAEC", "#82CAFA", 
                "#82CAFF", "#A0CFEC")

green_cols <- c("#4CC552", "#54C571", 
                "#99C68E", "#89C35C", 
                "#85BB65", "#8BB381", 
                "#9CB071", "#B2C248", 
                "#9DC209", "#A1C935")

red_cols <- c("#7f0000", "#990000", 
              "#cc0000", "#ff4c4c", 
              "#ff7f7f", "#ff1919", 
              "#ff9999", "#990000", 
              "#ff3232", "#4c0000"
              
)