library(NLP)
library(tm)
library(qdap)
library(xlsx)
library(text2vec)

rm(list=ls(all=TRUE))
DataPath = '/Users/host/Desktop/YuzheMa/Work/Research/NIHproject/Data/X_and_Y'
audio <- readRDS(file.path(DataPath, 'Audio.rds'))
audio_text <- audio$Audio_Text
audio_text <- iconv(audio_text, "latin1", "ASCII", sub="")
new_stops <- c(stopwords())
audio_text <- removeWords(audio_text, new_stops)
audio_text <- removeNumbers(audio_text)
audio_text <- removePunctuation(audio_text)
audio_text <- replace_abbreviation(audio_text)
audio_text <- replace_contraction(audio_text)
audio_text <- replace_symbol(audio_text)
audio_text <- tolower(audio_text)
audio_text <- stripWhitespace(audio_text)

# construct representation of each document based on word vectors
wordvec_len = 50
tokens <- space_tokenizer(audio_text)
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove = GlobalVectors$new(word_vectors_size = wordvec_len, vocabulary = vocab, x_max = 10)
word_vectors_main = glove$fit_transform(tcm, n_iter = 20)
word_vectors_context = glove$components
word_vectors = word_vectors_main + t(word_vectors_context)
words = vocab$term
I = 1 : length(audio_text)
X = matrix(0, nrow = length(audio_text), ncol = wordvec_len)
for (i in I) {
  doci = tokens[[i]]
  matched = match(doci, words)
  matched = na.omit(matched)
  if (length(matched) > 1){
    doc_vec = colSums(word_vectors[matched, ])/length(matched)
    X[i, ] = doc_vec
  }
  else if(length(matched) == 1){
    X[i, ] = word_vectors[matched, ]
  }
  else{
    print(paste('no representation for doc', toString(i)))
  }
}