############################################################
# PROJEKT ZALICZENIOWY
# System analizy opinii klientów hoteli
# Dane: tripadvisor_hotel_reviews.csv
############################################################

############################################################
# 0. WYMAGANE PAKIETY ----
############################################################

# Instalacja pakietów
# install.packages("tm")
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("textdata")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("SnowballC")
# install.packages("topicmodels")
# install.packages("factoextra")
# install.packages("cluster")
# install.packages("DT")

library(tm)           # Przetwarzanie tekstu
library(tidyverse)    # Praca z danymi i wykresy
library(tidytext)     # Tokenizacja i sentyment
library(textdata)     # Słowniki sentymentu
library(wordcloud)    # Chmury słów
library(RColorBrewer) # Palety kolorów
library(ggplot2)      # Wykresy
library(ggthemes)     # Motywy wykresów
library(SnowballC)    # Stemming
library(topicmodels)  # LDA topic modeling
library(factoextra)   # Wizualizacja klastrów
library(cluster)      # Klastrowanie
library(DT)           # Interaktywne tabele

############################################################
# 1. WCZYTANIE DANYCH ----
############################################################

file_name <- "tripadvisor_hotel_reviews.csv"

reviews_data <- read.csv(
  file_name,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# Podgląd danych
str(reviews_data)
head(reviews_data)
colnames(reviews_data)

# Dataset ma kolumny: Review oraz Rating
reviews_data <- reviews_data %>%
  rename(
    review_text = Review,
    rating = Rating
  )

# Usunięcie pustych opinii
reviews_data <- reviews_data %>%
  filter(!is.na(review_text), review_text != "") %>%
  mutate(document_id = row_number())

cat("Liczba opinii:", nrow(reviews_data), "\n")
cat("Liczba kolumn:", ncol(reviews_data), "\n")

############################################################
# 2. ANALIZA OCEN ----
############################################################

rating_table <- reviews_data %>%
  count(rating)

print(rating_table)
write.csv(rating_table, "wyniki_01_rozklad_ocen.csv", row.names = FALSE)

plot_rating <- ggplot(rating_table, aes(x = factor(rating), y = n)) +
  geom_col() +
  labs(
    title = "Rozkład ocen hoteli",
    x = "Ocena",
    y = "Liczba opinii"
  ) +
  theme_minimal()

print(plot_rating)
ggsave("wykres_01_rozklad_ocen.png", plot_rating, width = 8, height = 5)

############################################################
# 3. KORPUS I CZYSZCZENIE TEKSTU ----
############################################################

# Utworzenie korpusu tekstowego
corpus <- VCorpus(VectorSource(reviews_data$review_text))

# Zapewnienie kodowania UTF-8
corpus <- tm_map(
  corpus,
  content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte"))
)

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# Usunięcie zbędnych znaków i pozostałości URL/HTML
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")

# Standardowe czyszczenie tekstu
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Dodatkowe stopwords dopasowane do recenzji hoteli
custom_stopwords <- c(
  "hotel", "hotels", "room", "rooms", "stay", "stayed", "night", "nights",
  "trip", "advisor", "review", "reviews", "really", "just", "also", "one",
  "get", "got", "can", "will", "would", "could", "did", "dont", "didnt",
  "im", "ive", "us", "u", "amp", "much", "many", "even", "back"
)

corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stripWhitespace)

# WAŻNE: nie używamy stemCompletion, bo przy dużym pliku działa bardzo wolno.
# Dla sprawnego działania projektu używamy korpusu po czyszczeniu.
corpus_final <- corpus

# Kontrola przykładowej opinii po czyszczeniu
corpus_final[[1]][[1]]

############################################################
# 4. MACIERZE BAG OF WORDS: TDM I DTM ----
############################################################

# Term-Document Matrix
tdm <- TermDocumentMatrix(corpus_final)

# Document-Term Matrix
dtm <- DocumentTermMatrix(corpus_final)

# Usunięcie bardzo rzadkich słów, aby skrypt działał sprawnie
dtm_sparse <- removeSparseTerms(dtm, 0.99)
tdm_sparse <- removeSparseTerms(tdm, 0.99)

dtm_m <- as.matrix(dtm_sparse)
tdm_m <- as.matrix(tdm_sparse)

cat("Wymiar DTM:", dim(dtm_m), "\n")
cat("Wymiar TDM:", dim(tdm_m), "\n")

############################################################
# 5. ANALIZA CZĘSTOŚCI SŁÓW ----
############################################################

word_freq <- sort(colSums(dtm_m), decreasing = TRUE)

word_freq_df <- data.frame(
  word = names(word_freq),
  freq = as.numeric(word_freq),
  row.names = NULL
)

# Top 20 słów
top_words <- head(word_freq_df, 20)
print(top_words)
write.csv(top_words, "wyniki_02_top_20_slow.csv", row.names = FALSE)

# Wykres najczęstszych słów
plot_top_words <- ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Najczęściej występujące słowa w opiniach",
    x = "Słowo",
    y = "Liczba wystąpień"
  ) +
  theme_minimal()

print(plot_top_words)
ggsave("wykres_02_top_20_slow.png", plot_top_words, width = 8, height = 5)

############################################################
# 6. CHMURA SŁÓW ----
############################################################

png("wykres_03_chmura_slow.png", width = 1000, height = 800)
set.seed(123)
wordcloud(
  words = word_freq_df$word,
  freq = word_freq_df$freq,
  min.freq = 20,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
title("Chmura słów - opinie klientów hoteli")
dev.off()

# Wyświetlenie chmury w RStudio
set.seed(123)
wordcloud(
  words = word_freq_df$word,
  freq = word_freq_df$freq,
  min.freq = 20,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
title("Chmura słów - opinie klientów hoteli")

############################################################
# 7. TF-IDF ----
############################################################

# Macierz DTM z wagami TF-IDF
dtm_tfidf <- DocumentTermMatrix(
  corpus_final,
  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
)

dtm_tfidf_sparse <- removeSparseTerms(dtm_tfidf, 0.99)
dtm_tfidf_m <- as.matrix(dtm_tfidf_sparse)

# Średnia wartość TF-IDF dla słów
tfidf_scores <- sort(colMeans(dtm_tfidf_m), decreasing = TRUE)

tfidf_df <- data.frame(
  word = names(tfidf_scores),
  tfidf = as.numeric(tfidf_scores),
  row.names = NULL
)

top_tfidf <- head(tfidf_df, 20)
print(top_tfidf)
write.csv(top_tfidf, "wyniki_03_top_20_tfidf.csv", row.names = FALSE)

plot_tfidf <- ggplot(top_tfidf, aes(x = reorder(word, tfidf), y = tfidf)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Słowa o najwyższej średniej wartości TF-IDF",
    x = "Słowo",
    y = "Średnia wartość TF-IDF"
  ) +
  theme_minimal()

print(plot_tfidf)
ggsave("wykres_04_tfidf.png", plot_tfidf, width = 8, height = 5)

############################################################
# 8. ANALIZA SENTYMENTU - BING ----
############################################################

# Tokenizacja przy użyciu tidytext
reviews_tidy <- reviews_data %>%
  select(document_id, rating, review_text) %>%
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% custom_stopwords)

# Słownik Bing: positive / negative
bing <- get_sentiments("bing")

sentiment_bing <- reviews_tidy %>%
  inner_join(bing, by = "word", relationship = "many-to-many")

sentiment_summary <- sentiment_bing %>%
  count(sentiment)

print(sentiment_summary)
write.csv(sentiment_summary, "wyniki_04_sentyment_bing_podsumowanie.csv", row.names = FALSE)

plot_sentiment_bing <- ggplot(sentiment_summary, aes(x = sentiment, y = n)) +
  geom_col() +
  labs(
    title = "Liczba słów pozytywnych i negatywnych - Bing",
    x = "Sentyment",
    y = "Liczba słów"
  ) +
  theme_minimal()

print(plot_sentiment_bing)
ggsave("wykres_05_sentyment_bing.png", plot_sentiment_bing, width = 8, height = 5)

# Najczęstsze słowa pozytywne i negatywne
top_sentiment_words <- sentiment_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word, n, sentiment))

write.csv(top_sentiment_words, "wyniki_05_top_slowa_sentyment_bing.csv", row.names = FALSE)

plot_top_sentiment <- ggplot(top_sentiment_words, aes(x = word2, y = n)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Najczęstsze słowa pozytywne i negatywne - Bing",
    x = "Słowo",
    y = "Liczba wystąpień"
  ) +
  theme_gdocs()

print(plot_top_sentiment)
ggsave("wykres_06_top_slowa_sentyment_bing.png", plot_top_sentiment, width = 10, height = 6)

############################################################
# 9. ANALIZA EMOCJI - NRC ----
############################################################

# Słownik NRC
nrc <- get_sentiments("nrc")

sentiment_nrc <- reviews_tidy %>%
  inner_join(nrc, by = "word", relationship = "many-to-many")

nrc_summary <- sentiment_nrc %>%
  count(sentiment, sort = TRUE)

print(nrc_summary)
write.csv(nrc_summary, "wyniki_06_sentyment_nrc.csv", row.names = FALSE)

plot_nrc <- ggplot(nrc_summary, aes(x = reorder(sentiment, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Kategorie emocji w opiniach - NRC",
    x = "Emocja / sentyment",
    y = "Liczba słów"
  ) +
  theme_gdocs()

print(plot_nrc)
ggsave("wykres_07_emocje_nrc.png", plot_nrc, width = 8, height = 5)

############################################################
# 10. SENTYMENT A OCENA HOTELU ----
############################################################

sentiment_by_rating <- sentiment_bing %>%
  count(rating, sentiment) %>%
  group_by(rating) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

print(sentiment_by_rating)
write.csv(sentiment_by_rating, "wyniki_07_sentyment_wedlug_oceny.csv", row.names = FALSE)

plot_sentiment_rating <- ggplot(sentiment_by_rating, aes(x = factor(rating), y = share, fill = sentiment)) +
  geom_col(position = "dodge") +
  labs(
    title = "Udział słów pozytywnych i negatywnych według oceny hotelu",
    x = "Ocena hotelu",
    y = "Udział słów",
    fill = "Sentyment"
  ) +
  theme_minimal()

print(plot_sentiment_rating)
ggsave("wykres_08_sentyment_wedlug_oceny.png", plot_sentiment_rating, width = 8, height = 5)

############################################################
# 11. ASOCJACJE SŁÓW ----
############################################################

association_dtm <- DocumentTermMatrix(corpus_final)
association_dtm <- removeSparseTerms(association_dtm, 0.99)

# Funkcja do sprawdzania asocjacji słów
show_associations <- function(dtm_object, term, correlation_limit = 0.20) {
  if (term %in% Terms(dtm_object)) {
    cat("\nAsocjacje dla słowa:", term, "\n")
    print(findAssocs(dtm_object, term, correlation_limit))
  } else {
    cat("\nSłowo", term, "nie występuje w macierzy po czyszczeniu danych.\n")
  }
}

show_associations(association_dtm, "service", 0.20)
show_associations(association_dtm, "staff", 0.20)
show_associations(association_dtm, "clean", 0.20)
show_associations(association_dtm, "location", 0.20)
show_associations(association_dtm, "breakfast", 0.20)
show_associations(association_dtm, "dirty", 0.20)

############################################################
# 12. TOPIC MODELING LDA ----
############################################################

lda_dtm <- DocumentTermMatrix(corpus_final)
lda_dtm <- removeSparseTerms(lda_dtm, 0.99)

# Usunięcie pustych dokumentów
row_totals <- apply(as.matrix(lda_dtm), 1, sum)
lda_dtm <- lda_dtm[row_totals > 0, ]

# Liczba tematów
number_of_topics <- 4

set.seed(1234)
lda_model <- LDA(
  lda_dtm,
  k = number_of_topics,
  control = list(seed = 1234)
)

lda_topics <- tidy(lda_model, matrix = "beta")

top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)
write.csv(top_terms, "wyniki_08_lda_top_slowa.csv", row.names = FALSE)

plot_lda <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Najważniejsze słowa w tematach LDA",
    x = "Słowo",
    y = "Beta - ważność słowa w temacie"
  ) +
  theme_minimal()

print(plot_lda)
ggsave("wykres_09_lda_tematy.png", plot_lda, width = 10, height = 7)

############################################################
# 13. KLASTROWANIE OPINII METODĄ K-MEANS ----
############################################################

cluster_dtm <- DocumentTermMatrix(corpus_final)
cluster_dtm <- removeSparseTerms(cluster_dtm, 0.99)
cluster_m <- as.matrix(cluster_dtm)

# Usunięcie pustych dokumentów
row_totals_cluster <- rowSums(cluster_m)
cluster_m <- cluster_m[row_totals_cluster > 0, ]

# Ograniczenie liczby dokumentów, żeby działało szybko
set.seed(123)
if (nrow(cluster_m) > 1000) {
  sample_idx <- sample(1:nrow(cluster_m), 1000)
  cluster_m_sample <- cluster_m[sample_idx, ]
} else {
  sample_idx <- 1:nrow(cluster_m)
  cluster_m_sample <- cluster_m
}

cat("Liczba dokumentów użytych do klastrowania:", nrow(cluster_m_sample), "\n")
cat("Liczba cech/słów użytych do klastrowania:", ncol(cluster_m_sample), "\n")

# Wykonanie klastrowania k-means
set.seed(123)
k <- 4

kmeans_model <- kmeans(
  cluster_m_sample,
  centers = k,
  nstart = 25
)

# Wizualizacja klastrów
plot_cluster <- fviz_cluster(
  list(data = cluster_m_sample, cluster = kmeans_model$cluster),
  geom = "point",
  main = "Wizualizacja klastrów opinii klientów"
)

print(plot_cluster)
ggsave("wykres_10_klastry_kmeans.png", plot_cluster, width = 8, height = 6)

# Podsumowanie klastrów: liczebność i top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(kmeans_model$cluster == i)
  cluster_docs <- cluster_m_sample[cluster_docs_idx, , drop = FALSE]
  word_freq_cluster <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words_cluster <- paste(names(word_freq_cluster)[1:min(5, length(word_freq_cluster))], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentow = length(cluster_docs_idx),
    Top_5_slow = top_words_cluster,
    stringsAsFactors = FALSE
  )
})

cluster_info_df <- do.call(rbind, cluster_info)
print(cluster_info_df)
write.csv(cluster_info_df, "wyniki_09_klastry_podsumowanie.csv", row.names = FALSE)

plot_cluster_count <- ggplot(cluster_info_df, aes(x = factor(Klaster), y = Liczba_dokumentow)) +
  geom_col() +
  labs(
    title = "Liczba opinii w poszczególnych klastrach",
    x = "Klaster",
    y = "Liczba dokumentów"
  ) +
  theme_minimal()

print(plot_cluster_count)
ggsave("wykres_11_liczba_opinii_w_klastrach.png", plot_cluster_count, width = 8, height = 5)

