library(tm)
library(NLP)
library(qdap)

audio <- read.csv('P:/Toolboxes/RiskDatabase/code/Audio.csv', encoding='chars', stringsAsFactors = FALSE)

audio_text <- audio$Audio_Text

audio_ID <- audio$SubID

audio_text <- iconv(audio_text, "latin1", "ASCII", sub="")

audio_text <- removeNumbers(audio_text)

audio_text <- stripWhitespace(audio_text)

audio_text <- tolower(audio_text)

audio_text <- removePunctuation(audio_text)

audio_text <- replace_abbreviation(audio_text)

audio_text <- replace_contraction(audio_text)

audio_text <- replace_symbol(audio_text)

new_stops <- c(stopwords())
audio_text <- removeWords(audio_text, new_stops)

# do stemming
# audio_text <- stemDocument(audio_text)
# audio_text <- stemCompletion(audio_text)

audio_source <- VectorSource(audio_text)

audio_corpus <- VCorpus(audio_source)

# construct TDM matrix
audio_dtm <- DocumentTermMatrix(audio_corpus)

terms <- audio_dtm$dimnames$Terms

audio_m <- as.matrix(audio_dtm)

frequency <- colSums(audio_m)

# top 10 words
frequency_sorted <- sort(frequency, decreasing = TRUE)
freq_words <- frequency_sorted[0:9]

audio_svd <- svd(audio_m)

U <- audio_svd$u   #doc by signular vectors

D <- audio_svd$d   #signular values

V <- audio_svd$v   #loadings of words onto vectors

k <- 10

U_k <- U[, 0: (k-1)]
D_k <- D[0: (k-1)]
V_k <- V[, 0: (k-1)]

# reconstructed TDM matrix
audio_m_k <- U_k %*% diag(D_k) %*% t(V_k)

# find most important m words
m <- 10
important_words <- terms[order(abs(V[, 1]), decreasing=TRUE)[0: (m-1)]]
