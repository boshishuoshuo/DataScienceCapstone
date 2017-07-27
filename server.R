library(shiny)
library(tm)
library(NLP)
library(wordcloud)
library(RWeka)
library(RColorBrewer)

load("unigram_4.Rda")
load("bigram_4.Rda")
load("trigram_4.Rda")
load("fourgram_4.Rda")

unigram.df$word <- as.character(unigram.df$word)
bigram.df$word <- as.character(bigram.df$word)
trigram.df$word <- as.character(trigram.df$word)
fourgram.df$word <- as.character(fourgram.df$word)

luni <- sum(unigram.df$freq)
lbi <- sum(bigram.df$freq)
ltri <- sum(trigram.df$freq)
lfour <- sum(fourgram.df$freq)

unigram.df$prob <- unigram.df$freq / luni
bigram.df$prob <- bigram.df$freq / lbi
trigram.df$prob <- trigram.df$freq / ltri
fourgram.df$prob <- fourgram.df$freq / lfour

profanity.clean <- readRDS("profanity.rds")

clean.word <- function(words) {
        words.corpus <- VCorpus(VectorSource(words))
        
        # remove URLs, twtitter accounts and extra whitespace
        # toSpace is a function that transform a pattern to space
        toSpace <- content_transformer(function(x, pattern) gsub(pattern, 
                                                                 " ", x))
        
        # remove URLs
        words.corpus.clean <- tm_map(words.corpus, toSpace, 
                                     "http[s]?://[[:alnum:]_[:punct:]]+")
        
        # remove twitter accounts
        words.corpus.clean <- tm_map(words.corpus.clean, toSpace, 
                                     "@[[:alnum:]_]+")
        
        # remove profanity words
        words.corpus.clean <- tm_map(words.corpus.clean, removeWords, 
                                     profanity.clean)
        
        # change the word to lower case
        words.corpus.clean <- tm_map(words.corpus.clean, 
                                     content_transformer(tolower))
        
        # remove numbers
        words.corpus.clean <- tm_map(words.corpus.clean, removeNumbers)
        
        # remove punctuation
        words.corpus.clean <- tm_map(words.corpus.clean, removePunctuation)
        
        # remove white spaces
        words.corpus.clean <- tm_map(words.corpus.clean, stripWhitespace)
        
        # convert to plain text
        words.corpus.clean <- tm_map(words.corpus.clean, PlainTextDocument)
        
        return(as.character((words.corpus.clean[[1]])))
}

next.word <- function(words, n = 1) {
        if (identical(words, character(0))) {
                word.predict <- unigram.df[1:n, "word"]
                return(list(word.predict, "unigram", 
                            unigram.df[1:n, "freq"]))
        } else {
                # clean the words
                words.clean <- clean.word(words)
                
                # split into words
                words.split <- unlist(strsplit(words.clean, split = " "))
                
                # get the length of words.split
                words.l <- length(words.split)
                if (words.l >= 3) {
                        predict.4(words.clean, n)
                } else if (words.l == 2) {
                        predict.3(words.clean, n)
                } else {
                        predict.2(words.clean, n)
                }
        }
}


predict.1 <- function(words, n = 1) {
        word.predict <- unigram.df[1:n, "word"]
        return(list(word.predict, "unigram", unigram.df[1:n, "freq"]))
}

predict.2 <- function(words, n = 1) {
        
        # get the last 1 word
        words.1 <- tail(unlist(strsplit(words, split = " ")), 1)
        words.1 <- paste(words.1, collapse = " ")
        
        # check whether the last word is in unigram
        if (!(words.1 %in% unigram.df$word)) {
                predict.1(words, n)
        } else {
                # find the rows that start with words.2 in trigram
                bi.relative.rows <- grep(paste0("^", words.1, " "), 
                                         bigram.df$word)
                # get a subset of these rows of trigram
                bi.working <- bigram.df[bi.relative.rows,]
                
                # get the probability of the last 2 words in bigram
                words.1.prob <- unigram.df[unigram.df$word == words.1, "prob"]
                
                # add a column to trigram.working: probability of 2 words that starts
                # with words.1 over probability of the words.1
                bi.working$cond.prob <- bi.working$prob / words.1.prob
                
                # sort the trigram.working by cond.prob
                bi.working <- bi.working[order(bi.working$cond.prob,
                                               decreasing = T), ]
                
                # give the next n most likely words
                # if n > number of predicted words, go to unigram to get the rest
                
                N <- dim(bi.working)[1]
                
                if (N >= n) {
                        bi.working.result <- as.character(bi.working[c(1:n),
                                                                     "word"])
                        bi.working.prob <- bi.working[c(1:n), "cond.prob"]
                        bi.working.result.split <- strsplit(bi.working.result,
                                                            split = " ")
                        word.predict <- sapply(bi.working.result.split,
                                               function(x) x[2])
                        return(list(word.predict, "bigram",
                                    bi.working.prob))
                } else {
                        bi.working.result <- as.character(bi.working[, "word"])
                        bi.working.prob <- bi.working[,"cond.prob"]
                        bi.working.result.split <- strsplit(bi.working.result,
                                                            split = " ")
                        word.predict.from.2 <- sapply(bi.working.result.split,
                                                      function(x) x[2])
                        result.from.1 <- predict.1(words, n-N)
                        word.predict <- c(word.predict.from.2, 
                                          result.from.1[[1]])
                        n.1 <- length(result.from.1[[3]])
                        prob <- c(bi.working.prob, rep(tail(bi.working.prob,1),
                                                       n.1))
                        return(list(word.predict, paste("bigram and", 
                                                        result.from.1[[2]],
                                                        sep = " "), prob))
                }
        }
        
}

predict.3 <- function(words, n = 1) {
        
        # get the last 2 words
        words.2 <- tail(unlist(strsplit(words, split = " ")), 2)
        words.2 <- paste(words.2, collapse = " ")
        
        # check whether the last 2 words are in the bigram.df
        if (!(words.2 %in% bigram.df$word)) {
                predict.2(words, n)
        } else {
                # find the rows that start with words.2 in trigram
                tri.relative.rows <- grep(paste0("^", words.2, " "), 
                                          trigram.df$word)
                # get a subset of these rows of trigram
                tri.working <- trigram.df[tri.relative.rows,]
                
                # get the probability of the last 2 words in bigram
                words.2.prob <- bigram.df[bigram.df$word == words.2, "prob"]
                
                # add a column to trigram.working: probability of 3 words that starts
                # with words.2 over probability of the words.2
                tri.working$cond.prob <- tri.working$prob / words.2.prob
                
                # sort the trigram.working by cond.prob
                tri.working <- tri.working[order(tri.working$cond.prob,
                                                 decreasing = TRUE), ]
                
                # give the next n most likely words. 
                # if n > working number of words, go to bigram to get the 
                # rest words
                
                N <- dim(tri.working)[1]
                
                if (N >= n) {
                        tri.working.result <- as.character(tri.working[c(1:n),
                                                                       "word"])
                        tri.working.prob <- tri.working[c(1:n), "cond.prob"]
                        tri.working.result.split <- strsplit(tri.working.result,
                                                             split = " ")
                        word.predict <- sapply(tri.working.result.split,
                                               function(x) x[3])
                        return(list(word.predict, "trigram", 
                                    tri.working.prob))
                } else {
                        tri.working.result <- as.character(tri.working[, "word"])
                        tri.working.prob <- tri.working[, "cond.prob"]
                        tri.working.result.split <- strsplit(tri.working.result,
                                                             split = " ")
                        word.predict.from.3 <- sapply(tri.working.result.split,
                                                      function(x) x[3])
                        result.from.2 <- predict.2(words, n-N)
                        word.predict <- c(word.predict.from.3, 
                                          result.from.2[[1]])
                        prob <- c(tri.working.prob, result.from.2[[3]])
                        return(list(word.predict, paste("trigram and",
                                                        result.from.2[[2]],
                                                        sep = " "), prob))
                }
        }
}

predict.4 <- function(words, n = 1) {
        
        # get the last 3 words
        words.3 <- tail(unlist(strsplit(words, split = " ")), 3)
        words.3 <- paste(words.3, collapse = " ")
        
        # check whether the last 3 words is in trigram
        if (!(words.3 %in% trigram.df$word)) {
                predict.3(words, n)
        } else {
                # find the rows that start with words.3 in fourgram
                four.relative.rows <- grep(paste0("^", words.3, " "), 
                                           fourgram.df$word)
                # get a subset of these rows of fourgram
                four.working <- fourgram.df[four.relative.rows,]
                
                # get the probability of the last 3 words in trigram
                words.3.prob <- trigram.df[trigram.df$word == words.3, "prob"]
                
                # add a column to fourgram.working: probability of 4 words that 
                #starts with words.3 over probability of the words.3
                four.working$cond.prob <- four.working$prob / words.3.prob
                
                # sort the fourgram.working by cond.prob
                four.working <- four.working[order(four.working$cond.prob,
                                                   decreasing = TRUE), ]
                
                # give the next n most likely words, if n > number of predicted
                # words, go to the trigram to get the rest words
                
                N <- dim(four.working)[1]
                if (N >= n) {
                        four.working.result <- as.character(four.working[c(1:n),
                                                                         "word"])
                        four.working.prob <- four.working[c(1:n), "cond.prob"]
                        four.working.result.split <- strsplit(four.working.result,
                                                              split = " ")
                        word.predict <- sapply(four.working.result.split,
                                               function(x) x[4])
                        return(list(word.predict, "fourgram", 
                                    four.working.prob))
                } else {
                        four.working.result <- as.character(four.working[, "word"])
                        four.working.prob <- four.working[, "cond.prob"]
                        four.working.result.split <- strsplit(four.working.result,
                                                              split = " ")
                        word.predict.from.4 <- sapply(four.working.result.split,
                                                      function(x) x[4])
                        result.from.3 <- predict.3(words, n-N)
                        word.predict <- c(word.predict.from.4, 
                                          result.from.3[[1]])
                        prob <- c(four.working.prob, result.from.3[[3]])
                        return(list(word.predict, paste("fourgram and", 
                                                        result.from.3[[2]],
                                                        sep = " "), prob))
                }
                
                
        }
}

shinyServer(function(input, output) {
        result <- reactive({
                sentence <- input$text1
                n <- input$choice
                next.word(sentence, n)
        })
        output$predicted_word <- renderPrint(result()[[1]])
        output$Ngram <- renderText(result()[[2]])
        output$cloud <- renderPlot({
                wordcloud(words = result()[[1]], freq = result()[[3]], 
                          random.order = FALSE, random.color = TRUE,
                          color = rainbow(10), scale = c(6, 1))
        })
})
