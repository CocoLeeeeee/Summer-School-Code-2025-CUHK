install.packages ("quanteda")
install.packages ("readtext")
install.packages ("stringi")
install.packages("ggplot2")
install.packages("tm")

install.packages("quanteda.textplots")
install.packages("quanteda.textstats")

install.packages("text2vec")
install.packages("uwot")
install.packages("syuzhet")
install.packages("stm")
install.packages("udpipe")
install.packages("reshape2")
install.packages("topicmodels")

library(quanteda)
library(readtext)
library(stringi)
library(ggplot2)
library(quanteda.textplots)
library(quanteda.textstats)
library(text2vec)
library(uwot)
library(syuzhet)
library(stm)
library(udpipe)
library(reshape2)
library(topicmodels)
library(tm)

##
TWOO <- texts(readtext("The Wizard of Oz.txt", encoding = "UTF-8"))
twoo_clean <- tokens(TWOO, remove_punct = TRUE) %>% 
  tokens_tolower()
twoo_token_count <- as.character(twoo_clean)
length (twoo_token_count)
twoo_remove <- tokens_remove(twoo_clean, stopwords("english"))

TSG <- texts(readtext("The secret garden.txt", encoding = "UTF-8"))
tsg_clean <- tokens(TSG, remove_punct = TRUE) %>% 
  tokens_tolower()
tsg_token_count <- as.character(tsg_clean)
length (tsg_token_count)
tsg_remove <- tokens_remove(tsg_clean, stopwords("english"))

TLP <- texts(readtext("The little prince.txt", encoding = "UTF-8"))
tlp_clean <- tokens(TLP, remove_punct = TRUE) %>% 
  tokens_tolower()
tlp_token_count <- as.character(tlp_clean)
length (tlp_token_count)
tlp_remove <- tokens_remove(tlp_clean, stopwords("english"))

PP <- texts(readtext("Pippi.txt", encoding = "UTF-8"))
pp_clean <- tokens(PP, remove_punct = TRUE) %>% 
  tokens_tolower()
pp_token_count <- as.character(pp_clean)
length (pp_token_count)
pp_remove <-tokens_remove(pp_clean, stopwords("english"))

PA <- texts(readtext("PETER AND WENDY.txt", encoding = "UTF-8"))
pa_clean <- tokens(PA, remove_punct = TRUE) %>% 
  tokens_tolower()
pa_token_count <- as.character(pa_clean)
length (pa_token_count)
pa_remove <- tokens_remove(pa_clean, stopwords("english"))

AN <- texts(readtext("Anne.txt", encoding = "UTF-8"))
an_clean <- tokens(AN, remove_punct = TRUE) %>% 
  tokens_tolower()
an_token_count <- as.character(an_clean)
length (an_token_count)
an_remove <- tokens_remove(an_clean, stopwords("english"))
  
twoo_chunk <- tokens_chunk(twoo_remove, size = 1000)
tsg_chunk <- tokens_chunk(tsg_remove, size = 1000)
tlp_chunk <- tokens_chunk(tlp_remove, size = 1000)
pp_chunk <- tokens_chunk(pp_remove, size = 1000)
pa_chunk <- tokens_chunk(pa_remove, size = 1000)
an_chunk <- tokens_chunk(an_remove, size = 1000)

#记得根据chunks数量在Latex画个表格
##
TWOO_syuzhet <- get_text_as_string("The Wizard of Oz.txt")
twoo_sentences <- get_sentences(TWOO_syuzhet)
twoo_sentiment <- get_sentiment(twoo_sentences, method = "syuzhet")
sy_sent_cs <- cumsum(twoo_sentiment)
plot(sy_sent_cs, type = "l",
     xlab = "Narrative time",
     ylab = "Emotional Valence", 
     main = "The Emotion Trajectory in The Wizard of Oz")

TSG_syuzhet <- get_text_as_string("The secret garden.txt")
tsg_sentences <- get_sentences(TSG_syuzhet)
tsg_sentiment <- get_sentiment(tsg_sentences, method = "syuzhet")
sy_sent_cs <- cumsum(tsg_sentiment)
plot(sy_sent_cs, type = "l",
     xlab = "Narrative time",
     ylab = "Emotional Valence", 
     main = "The Emotion Trajectory in The Secret Garden")

TLP_syuzhet <- get_text_as_string("The little prince.txt")
tlp_sentences <- get_sentences(TLP_syuzhet)
tlp_sentiment <- get_sentiment(tlp_sentences, method = "syuzhet")
plot(sy_sent_cs, type = "l",
     xlab = "Narrative time",
     ylab = "Emotional Valence", 
     main = "The Emotion Trajectory in The Little Prince")

PP_syuzhet <- get_text_as_string("Pippi.txt")
pp_sentences <- get_sentences(PP_syuzhet)
pp_sentiment <- get_sentiment(pp_sentences, method = "syuzhet")
plot(sy_sent_cs, type = "l",
     xlab = "Narrative time",
     ylab = "Emotional Valence", 
     main = "The Emotion Trajectory in Pippi Longstocking")

PA_syuzhet <- get_text_as_string("PETER AND WENDY.txt")
PA_syuzhet <- iconv(PA_syuzhet, to = "UTF-8", sub = " ")
pa_sentences <- get_sentences(PA_syuzhet)
pa_sentiment <- get_sentiment(pa_sentences, method = "syuzhet")
plot(sy_sent_cs, type = "l",
     xlab = "Narrative time",
     ylab = "Emotional Valence", 
     main = "The Emotion Trajectory in Peter Pan")

AN_syuzhet <- get_text_as_string("Anne.txt")
an_sentences <- get_sentences(AN_syuzhet)
an_sentiment <- get_sentiment(an_sentences, method = "syuzhet")
plot(sy_sent_cs, type = "l",
     xlab = "Narrative time",
     ylab = "Emotional Valence", 
     main = "The Emotion Trajectory in Anne of Green Gables")

all_books <- data.frame(
  time = c(seq_along(cumsum(twoo_sentiment)), 
           seq_along(cumsum(tsg_sentiment)),
           seq_along(cumsum(tlp_sentiment)),
           seq_along(cumsum(pp_sentiment)),
           seq_along(cumsum(pa_sentiment)),
           seq_along(cumsum(an_sentiment))),
  sentiment = c(cumsum(twoo_sentiment), 
                cumsum(tsg_sentiment),
                cumsum(tlp_sentiment),
                cumsum(pp_sentiment),
                cumsum(pa_sentiment),
                cumsum(an_sentiment)),
  book = rep(c("The Wizard of Oz", "The Secret Garden", "The Little Prince",
               "Pippi Longstocking", "Peter Pan", "Anne of Green Gables"),
             times = c(length(cumsum(twoo_sentiment)), 
                       length(cumsum(tsg_sentiment)),
                       length(cumsum(tlp_sentiment)),
                       length(cumsum(pp_sentiment)),
                       length(cumsum(pa_sentiment)),
                       length(cumsum(an_sentiment))))
)
ggplot(all_books, aes(x = time, y = sentiment, color = book)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  labs(x = "Narrative time", 
       y = "Emotional Valence", 
       title = "Emotion Trajectories in Children's Literature",
       color = "Book Title") +
  theme_minimal() +
  scale_color_manual(values = c("The Wizard of Oz" = "#FF2D2D",
                                "The Secret Garden" = "#0080FF",
                                "The Little Prince" = "#02DF82",
                                "Pippi Longstocking" = "#B15BFF",
                                "Peter Pan" = "#FF9224",
                                "Anne of Green Gables" = "#B87070")) +
  theme(legend.position = "bottom")

books_upward <- data.frame(
  time_1 = c(
    1:length(cumsum(tsg_sentiment)),
    1:length(cumsum(tlp_sentiment)),
    1:length(cumsum(pp_sentiment)),
    1:length(cumsum(an_sentiment))
  ),
  sentiment_1 = c(
    cumsum(tsg_sentiment),
    cumsum(tlp_sentiment),
    cumsum(pp_sentiment),
    cumsum(an_sentiment)
  ),
  book_1 = rep(
    c("The Secret Garden", "The Little Prince", 
      "Pippi Longstocking", "Anne of Green Gables"),
    times = c(
      length(cumsum(tsg_sentiment)),
      length(cumsum(tlp_sentiment)),
      length(cumsum(pp_sentiment)),
      length(cumsum(an_sentiment))
    )
  )
)
ggplot(books_upward, aes(x = time_1, y = sentiment_1, color = book_1)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  labs(
    x = "Narrative time", 
    y = "Emotional Valence", 
    title = "Emotion Trajectories in Children's Literature (Group 1)",
    color = "Book Title"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "The Secret Garden" = "#9F0050",
    "The Little Prince" = "#F00078",
    "Pippi Longstocking" = "#FF79BC",
    "Anne of Green Gables" = "#FF95CA"
  )) +
  theme(legend.position = "bottom")

books_upward <- data.frame(
  time_2 = c(
    seq_along(cumsum(twoo_sentiment)),
    seq_along(cumsum(pa_sentiment))
  ),
  sentiment_2 = c(
    cumsum(twoo_sentiment),
    cumsum(pa_sentiment)
  ),
  book_2 = rep(
    c("The Wizard of Oz", "Peter Pan"),
    times = c(
      length(cumsum(twoo_sentiment)),
      length(cumsum(pa_sentiment))
    )
  )
)
ggplot(books_upward, aes(x = time_2, y = sentiment_2, color = book_2)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  labs(
    x = "Narrative time", 
    y = "Emotional Valence", 
    title = "Emotion Trajectories in Children's Literature (Group 2)",
    color = "Book Title"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "The Wizard of Oz" = "#005AB5",
    "Peter Pan" = "#97CBFF"
  )) +
  theme(legend.position = "bottom")

##
twoo_dfm <- dfm(twoo_chunk)
pa_dfm <- dfm(pa_chunk)

twoo_dtm <- convert(twoo_dfm, to = "topicmodels")

set.seed(42)

find_optimal_k <- function(dtm, k_range = seq(10, 20, by = 1)) {
  logLik_scores <- numeric(length(k_range))
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("Testing k = %d\n", k))
    model <- LDA(dtm, method = "Gibbs", k = k, control = list(seed = 1))
    logLik_scores[i] <- logLik(model)
  }

  plot(k_range, logLik_scores, type = "b",
       xlab = "Number of Topics (k)",
       ylab = "Log-Likelihood",
       main = "Topic Model Log-Likelihood by k")
  
  results <- data.frame(
    k = k_range,
    logLik = logLik_scores)
  return(results)
}
results <- find_optimal_k(twoo_dtm)
print(results)

pa_dtm <- convert(pa_dfm, to = "topicmodels")

set.seed(42)

find_optimal_k <- function(dtm, k_range = seq(10, 20, by = 1)) {
  logLik_scores <- numeric(length(k_range))
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("Testing k = %d\n", k))
    model <- LDA(dtm, method = "Gibbs", k = k, control = list(seed = 1))
    logLik_scores[i] <- logLik(model)
  }
  
  plot(k_range, logLik_scores, type = "b",
       xlab = "Number of Topics (k)",
       ylab = "Log-Likelihood",
       main = "Topic Model Log-Likelihood by k")
  
  results <- data.frame(
    k = k_range,
    logLik = logLik_scores)
  return(results)
}
results <- find_optimal_k(pa_dtm)
print(results)

find_optimal_k_data <- function(dtm, k_range = seq(10, 20, by = 1)) {
  logLik_scores <- numeric(length(k_range))
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("Testing k = %d\n", k))
    model <- LDA(dtm, method = "Gibbs", k = k, control = list(seed = 1))
    logLik_scores[i] <- logLik(model)
  }
  
  data.frame(
    k = k_range,
    logLik = logLik_scores)
}

# 获取两个数据集的结果
set.seed(42)
twoo_results <- find_optimal_k_data(twoo_dtm)
pa_results <- find_optimal_k_data(pa_dtm)

# 添加数据集标识
twoo_results$dataset <- "twoo"
pa_results$dataset <- "pa"

# 合并数据
combined_results <- rbind(twoo_results, pa_results)

# 绘制合并图形
ggplot(combined_results, aes(x = k, y = logLik, color = dataset, group = dataset)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Topics (k)",
       y = "Log-Likelihood",
       title = "Topic Model Log-Likelihood Comparison",
       color = "Dataset") +
  scale_x_continuous(breaks = seq(10, 20, by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom")

twoo_lda <- LDA(twoo_dtm, k = 20)
terms(twoo_lda, 20)[,11]
pa_lda <- LDA(pa_dtm, k = 20)
terms(pa_lda, 20)[,17]

##
twoo_topic11 <- exp(twoo_lda@beta[11, ])
pa_topic17 <- exp(pa_lda@beta[17, ])     

common_terms <- intersect(colnames(twoo_lda@terms), colnames(pa_lda@terms))

twoo_topic11_common <- twoo_topic11[match(common_terms, colnames(twoo_lda@terms))]
pa_topic17_common <- pa_topic17[match(common_terms, colnames(pa_lda@terms))]

cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

similarity <- cosine_similarity(twoo_topic11_common, pa_topic17_common)
print(paste("cosine similarity:", similarity))

##
specified_conrad <- readtext("The Wizard of Oz.txt", encoding = "UTF-8")

selected_children <- list.files(pattern = "Prince|Anne|Garden|Pippi", ignore.case = TRUE) 
corpus_children <- lapply(selected_children, function(file) {
  paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
})
names(corpus_children) <- selected_children

combined_texts <- c(specified_conrad$text, unlist(corpus_children))
combined_corpus <- Corpus(VectorSource(combined_texts))

combined_dtm <- DocumentTermMatrix(combined_corpus,
                                   control = list(
                                     stopwords = TRUE,
                                     removePunctuation = TRUE,
                                     removeNumbers = TRUE,
                                     stemming = TRUE,
                                     bounds = list(global = c(2, Inf))
                                   ))

specified_dtm <- combined_dtm[1, ] 
children_dtm <- combined_dtm[-1, ] 

children_lda <- LDA(children_dtm, k = 20) 

specified_topics <- posterior(children_lda, specified_dtm) 
specified_beta <- t(specified_topics$terms)
children_beta <- exp(children_lda@beta)

cosine_similarity <- function(vec, mat) {
  vec <- as.vector(vec)
  sim <- sapply(1:nrow(mat), function(j) {
    sum(vec * mat[j, ]) / (sqrt(sum(vec^2)) * sqrt(sum(mat[j, ]^2)))
  })
  return(sim)
}

specified_vec <- as.vector(specified_beta)  
children_mat <- children_beta             

similarity_results <- cosine_similarity(specified_vec, children_mat)
print(similarity_results)

results_df <- data.frame(
  Children_Topic = 1:length(similarity_results),
  Similarity = similarity_results
)

ggplot(results_df, aes(x = Children_Topic, y = Similarity)) +
  geom_col(fill = "#84C1FF") +
  geom_hline(yintercept = mean(similarity_results), linetype = "dashed", color = "red") +
  labs(title = "A comparison of the similarity between the themes of The Wizard of Oz and children's literature", 
       x = "Children's literature theme number",
       y = "cosine similarity") +
  theme_minimal()
