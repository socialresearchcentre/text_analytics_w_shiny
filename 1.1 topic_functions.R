## Parent Syntax: 1 topic_model.R

#NLP Functions
run_lda <- function(dfm, filt, k, outpath) {

  usethis::ui_info("{Sys.time()}: {deparse(filt)}")
  
  usethis::ui_info("Preparing DFM")
  
  dfm_sub <- dfm_subset(dfm, eval(filt))
  
  set.seed(1)
  
  usethis::ui_info("Fitting LDA")
  
  filt_fn <- deparse(filt) %>% str_replace_all("\\s", "-") %>% str_replace_all('\\"', "") %>% str_replace_all("%in%-", "")
  out_dir <- str_glue("{outpath}/{filt_fn}/")
  dir.create(str_glue("{out_dir}/"), FALSE)
  
  saveRDS(dfm_sub, str_glue("{out_dir}/dfm.rds"))
  
  
  model <- FitLdaModel(dtm = dfm_sub,
                       k = k,
                       iterations = 1000, # I usually recommend at least 500 iterations or more
                       burnin = 500, 
                       alpha = 0.1, # Lower means each document has less topics
                       beta = 0.01, # Lower means a topic has only few words
                       optimize_alpha = TRUE,
                       calc_likelihood = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = TRUE,
                       cpus = 2)
  
  summary <- lda_summary(model, dfm_sub)
  
  # Ensure unique topic names
  dups <- duplicated(summary$summary)
  for (i in which(dups)) {
    summary$summary[i] <- glue("{summary$summary[i]}2")
  }
  
  
  saveRDS(model, str_glue("{out_dir}/lda.rds"))
  saveRDS(summary, str_glue("{out_dir}/summary.rds"))
  
  usethis::ui_info("{Sys.time()}: Model and DFM written out")
  
}

lda_summary <- function(model, dtm) {
  # Get the top terms of each topic
  model$top_terms <- GetTopTerms(phi = model$phi, M = 100)
  
  # Get the prevalence of each topic
  # You can make this discrete by applying a threshold, say 0.05, for
  # topics in/out of docuemnts. 
  model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
  
  # textmineR has a naive topic labeling tool based on probable bigrams
  model$labels <- LabelTopics(assignments = model$theta > 0.25, # 0.05, 
                              dtm = dtm,
                              M = 5) 
  
  # put them together, with coherence into a summary table
  tibble(topic = str_remove(rownames(model$phi), "t_"),
         summary = str_replace_all(model$labels %>% as_tibble() %>% pull, "_", " "),
         coherence = round(model$coherence, 3),
         prevalence = round(model$prevalence,3),
         top_terms = apply(model$top_terms, 2, function(x){
           paste(x, collapse = ", ")
         }))
}

filter_join <- function(filtvars) {
  # Filtvars is a list of named character vectors. 
  # Each character vector name represents a variable, and the vector contents represent the conditions to filter to for that variable
  
  # Use list name to create expression
  filters <- lapply(names(filtvars), function(x) paste0(x, " %in% ", "'{", x, "}'"))
  
  # Join expressions
  filt_str <- paste0(filters, collapse = " & ")
  
  
  return(filt_str)
}


save_dfm <- function(dfm, filt, outpath) {
  

  usethis::ui_info("{Sys.time()}: {deparse(filt)}")
  
  usethis::ui_info("Preparing DFM")
  
  dfm_sub <- dfm_subset(dfm, eval(filt))
  
  set.seed(1)
  
  usethis::ui_info("Fitting LDA")
  
  filt_fn <- deparse(filt) %>% str_replace_all("\\s", "-") %>% str_replace_all('\\"', "") %>% str_replace_all("%in%-", "")
  out_dir <- str_glue("{outpath}/{filt_fn}/")
  dir.create(str_glue("{out_dir}/"), FALSE)
  
  saveRDS(dfm_sub, str_glue("{out_dir}/dfm.rds"))
}
