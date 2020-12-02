## ---
## title: 2 tag_verbs_w_topics
## author: Gabriel Ong, Paddy Tobias
## email: gabriel.ong@srcentre.com.au, paddy.tobias@srcentre.com.au
## created: 2020-11-19
## overview: |
##     Tag the original verbatim responses with their dominant topic
## ---

library(LDAvis)
library(topicmodels)
library(servr)


# Functions ----

createJSON2 <- function (phi = matrix(), theta = matrix(), doc.length = integer(), 
          vocab = character(), term.frequency = integer(), R = 30, 
          lambda.step = 0.01, mds.method = jsPCA, cluster, plot.opts = list(xlab = "PC1", 
                                                                            ylab = "PC2"), 
         ...) {
  

  
  dp <- dim(phi)
  dt <- dim(theta)
  N <- sum(doc.length)
  W <- length(vocab)
  D <- length(doc.length)
  K <- dt[2]
  if (dp[1] != K) 
    stop("Number of rows of phi does not match \n      number of columns of theta; both should be equal to the number of topics \n      in the model.")
  if (D != dt[1]) 
    stop("Length of doc.length not equal \n      to the number of rows in theta; both should be equal to the number of \n      documents in the data.")
  if (dp[2] != W) 
    stop("Number of terms in vocabulary does \n      not match the number of columns of phi (where each row of phi is a\n      probability distribution of terms for a given topic).")
  if (length(term.frequency) != W) 
    stop("Length of term.frequency \n      not equal to the number of terms in the vocabulary.")
  if (any(nchar(vocab) == 0)) 
    stop("One or more terms in the vocabulary\n      has zero characters -- all terms must have at least one character.")
  phi.test <- all.equal(rowSums(phi), rep(1, K), check.attributes = FALSE)
  theta.test <- all.equal(rowSums(theta), rep(1, dt[1]), check.attributes = FALSE)
  if (!isTRUE(phi.test)) 
    stop("Rows of phi don't all sum to 1.")
  if (!isTRUE(theta.test)) 
    stop("Rows of theta don't all sum to 1.")
  topic.frequency <- colSums(theta * doc.length)
  topic.proportion <- topic.frequency/sum(topic.frequency)
  o <- order(topic.proportion, decreasing = TRUE)
  phi <- phi[o, ]
  theta <- theta[, o]
  topic.frequency <- topic.frequency[o]
  topic.proportion <- topic.proportion[o]
  
  mds.res <- mds.method(phi)
  

  
  if (is.matrix(mds.res)) {
    colnames(mds.res) <- c("x", "y")
  }
  else if (is.data.frame(mds.res)) {
    names(mds.res) <- c("x", "y")
  }
  else {
    warning("Result of mds.method should be a matrix or data.frame.")
  }
  mds.df <- data.frame(mds.res, topics = seq_len(K), Freq = topic.proportion * 
                         100, cluster = 1, stringsAsFactors = FALSE)
 
  term.topic.frequency <- phi * topic.frequency
  term.frequency <- colSums(term.topic.frequency)
  stopifnot(all(term.frequency > 0))
  term.proportion <- term.frequency/sum(term.frequency)
  phi <- t(phi)
  topic.given.term <- phi/rowSums(phi)
  kernel <- topic.given.term * log(sweep(topic.given.term, 
                                         MARGIN = 2, topic.proportion, `/`))
  distinctiveness <- rowSums(kernel)
  saliency <- term.proportion * distinctiveness
  default.terms <- vocab[order(saliency, decreasing = TRUE)][1:R]
  counts <- as.integer(term.frequency[match(default.terms, 
                                            vocab)])
  Rs <- rev(seq_len(R))
  default <- data.frame(Term = default.terms, logprob = Rs, 
                        loglift = Rs, Freq = counts, Total = counts, Category = "Default", 
                        stringsAsFactors = FALSE)
  topic_seq <- rep(seq_len(K), each = R)
  category <- paste0("Topic", topic_seq)
  lift <- phi/term.proportion
  find_relevance <- function(i) {
    relevance <- i * log(phi) + (1 - i) * log(lift)
    idx <- apply(relevance, 2, function(x) order(x, decreasing = TRUE)[seq_len(R)])
    indices <- cbind(c(idx), topic_seq)
    data.frame(Term = vocab[idx], Category = category, logprob = round(log(phi[indices]), 
                                                                       4), loglift = round(log(lift[indices]), 4), stringsAsFactors = FALSE)
  }
  lambda.seq <- seq(0, 1, by = lambda.step)
  if (missing(cluster)) {
    tinfo <- lapply(as.list(lambda.seq), find_relevance)
  }
  else {
    tinfo <- parallel::parLapply(cluster, as.list(lambda.seq), 
                                 find_relevance)
  }
  tinfo <- unique(do.call("rbind", tinfo))
  tinfo$Total <- term.frequency[match(tinfo$Term, vocab)]
  rownames(term.topic.frequency) <- paste0("Topic", seq_len(K))
  colnames(term.topic.frequency) <- vocab
  tinfo$Freq <- term.topic.frequency[as.matrix(tinfo[c("Category", 
                                                       "Term")])]
  
  
  map_topics <- data.frame(new = str_extract(rownames(mds.df), "\\d+"), 
                           Topic = mds.df$topics, stringsAsFactors = FALSE)
  
  tinfo <- rbind(default, tinfo)
  ut <- sort(unique(tinfo$Term))
  m <- sort(match(ut, vocab))
  tmp <- term.topic.frequency[, m]
  r <- row(tmp)[tmp >= 0.5]
  c <- col(tmp)[tmp >= 0.5]
  dd <- data.frame(Term = vocab[m][c], Topic = r, Freq = round(tmp[cbind(r, 
                                                                         c)]), stringsAsFactors = FALSE)
  dd[, "Freq"] <- dd[, "Freq"]/term.frequency[match(dd[, "Term"], 
                                                    vocab)]
  token.table <- dd[order(dd[, 1], dd[, 2]), ] 
 
  # Incorporate topic labels
  rownames(mds.df) <- summary$summary[match(rownames(mds.df), summary$topic)]
  
  map_topics <- data.frame(new = str_c("Topic", str_remove(rownames(mds.df), "t_")), 
                           Category =  str_c("Topic", mds.df$topics), stringsAsFactors = FALSE)
  
  tinfo <- tinfo %>% 
    left_join(map_topics) %>% 
    mutate(Category = ifelse(!is.na(new), new, Category)) %>% 
    select(-new)
  

  # Incorporate topic labels
  mds.df$topics <- rownames(mds.df)

  
  RJSONIO::toJSON(list(mdsDat = mds.df, tinfo = tinfo, token.table = token.table, 
                       R = R, lambda.step = lambda.step, plot.opts = plot.opts, 
                       topic.order = o))
}

# Adapted for handling model of class "lda_topic_model" produced from textmineR
# from : https://gist.github.com/trinker/477d7ae65ff6ca73cace
tm2LDAvis <- function(model, R, summary){
  
  
  
  if ("lda_topic_model" %in% class(model)){
    if (ncol(model$theta) < 3) stop("The model must contain > 2 topics")
    ldavis <- createJSON2(
      R = R,
      phi = model$phi, 
      theta = model$theta,
      vocab = colnames(model$phi),
      doc.length = slam::row_sums(model$data, na.rm = TRUE),
      term.frequency = slam::col_sums(model$data, na.rm = TRUE),
      summary = summary)
  } else if ("LDA_VEM" %in% class(model)){
    post <- topicmodels::posterior(model)
    if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
    mat <- model@wordassignments
    ldavis <- createJSON2(
      R = R, 
      phi = post[["terms"]], 
      theta = post[["topics"]],
      vocab = colnames(post[["terms"]]),
      doc.length = slam::row_sums(mat, na.rm = TRUE),
      term.frequency = slam::col_sums(mat, na.rm = TRUE))
  } else {
    stop("Class of topic model not recognised. Must be of class LDA_VEM or lda_topic_model")
  }
  return(ldavis)
}



# Create LDAvis object ----
dirs_ <- list.files("data/processed/models/", full.names = TRUE)

movie_ldavis <- list()

for (i in 1:length(dirs_)) {
  filt <- dirs_[[i]]
    print(filt)
    model <- readRDS(str_glue("{filt}/lda.rds"))
    
    summary <- readRDS(str_glue("{filt}/summary.rds")) %>%
      mutate(topic = paste0('t_', topic)) %>%
      select(topic, summary)
    
    
    tm_lda_vis <- tm2LDAvis(model, R = 15, summary)
    
  mod_name <- str_extract(filt, '[a-z_]+-.*$')  
  mod_name
  movie_ldavis[[mod_name]] <- tm_lda_vis

}

# Save ----
saveRDS(movie_ldavis, 'Dashboard/data/movie/db_imdb_vis.RDS')

#---
#EOF
#---