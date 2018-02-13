---
title: "Capstone Project Natural language processing (Milestone report)"
author: "Mohammad Ullah"
date: "February 13, 2018"
output: 
    html_document:
        keep_md: true
---



## Introduction

In modern days, we are spending a lot of time on our mobile devices to email, text, social networking and so on. It would be beneficiary if we can make typing bit easier. One way is to build a predictive keyboard that can give us a suggestion about our next word. For example, when someone types: "I want to", the keyboard will give 3-4 options like "eat", "play", "dance". This is a problem in the field of Statistical Natural Language Processing. There are already apps available to do this kind of predictive text processing. In this capstone project, I will combine analysis of text data and natural language processing to build a predictive text model. 

In this introductory milestone report of the capstone project, I will do a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora. I will use figures and tables to understand variation in the frequencies of words and word pairs in the data.

## Data Soure

Content archived from heliohost.org on September 30, 2016 and retrieved via Wayback Machine on April 24, 207.

https://web.archive.org/web/20160726181056/http://www.corpora.heliohost.org:80/aboutcorpus.html

## Objectives

### Primary objectives: 

- Data cleaning
- Tokenization 
- Exploratory analysis of data from twitter, blog and news corpora.

### Questions

- Some words are more frequent than others - what are the distributions of word frequencies?
- What are the frequencies of 2-grams and 3-grams in the data set?
- How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
- How do you evaluate how many of the words come from foreign languages?
- Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

## Data Processing

### Load the required libraries


```r
library(qdap)
library(qdapDictionaries)
library(tm)
library(ngram)
library(stringi)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)

set.seed(2341)
```

### Read data files


```r
file1 <- file("en_US.twitter.txt")
twitter <- readLines(file1, skipNul = TRUE)
close(file1)

file1 <- file("en_US.blogs.txt")
blogs <- readLines(file1)
close(file1)

file1 <- file("en_US.news.txt", "rb")
news <- readLines(file1)
close(file1)

file1 <- file("badwords.txt")
badwords <- readLines(file1, encoding = "UTF-8")
close(file1)
```


### Basic summary of three files


```
##      File   Flsize  Noline    Nochar   Noword
## 1 twitter   316 MB 2360148 134371600 30373583
## 2    blog 260.6 MB  899288 171926595 37334131
## 3    news 261.8 MB 1010242 170429117 34372530
```

![](milestone_new_files/figure-html/chunk3-1.png)<!-- -->

Above table gives an approximate information about the three file sizes, word, line and character count. Figure 1, also gives the same information in log scale. It is interesting but understandable that there is more lines yet less word count in twitter compared to blog and news corpora. 

### Sampling

I have created three subsets containing 10% data from each corpus for the exploratory analysis.


```r
## Sample the data (10% of each corpus)

en_tweet <- sample(twitter, round(length(twitter)*0.1))
en_blog <- sample(blogs, round(length(blogs)*0.1))
en_news <- sample(news, round(length(news)*0.1))

## Function to convert from UTF-8 to ASCII 

en_tweet <- iconv(en_tweet, 'UTF-8', 'ASCII', sub = "")
en_blog <- iconv(en_blog, 'UTF-8', 'ASCII', sub = "")
en_news <- iconv(en_news, 'UTF-8', 'ASCII', sub = "")
```

### Cleaning

The function below cleans the data for tokenizaton. For bi- and tri-gram (2-, 3-gram) tokenization, punctuation are removed first by keeping intra word contractions (example: i'll, he's) followed by number removal, case transformation (tolower), removal of profane words,word contractions are replaced (example: from "i'll" to "i will"), removal of words contaning more than two repeated letters and white space. In case of unigram (1-gram) tokenization, two further operations are done. These are removal of common stop words (example: "the", "and") and stemming. In case of 1-gram tokenization we are interested to understand the frequency of individual words in three different corpora. Hence, removing stop words and perform stemming (to get the base word) is reasonable. On the other hand, it is important to have a continuous and meaningful text flow to predict the next word from previous words. We will miss that flow by removing words from the text. Hence, for 2- and 3-gram tokenization, stemming and stop word removal was not done.        


```r
## Function for data cleaning

clean <- function(x, badwords, n) {
  
  tp_cl <- removePunctuation(x, preserve_intra_word_contractions = TRUE)
  tp_cl <- removeNumbers(tp_cl)
  tp_cl <- tolower(tp_cl)
  tp_cl <- removeWords(tp_cl, badwords)
  
  ## For bi- and tri-gram
  
  if (n == 0) {
    
    tp_cl <- replace_contraction(tp_cl, contraction = contractions, sent.cap = FALSE)
    
    ## Remove words with more than two repeated letters (Example: helllo or youuu)
    
    tp_cl <- gsub("\\s*\\b(?=\\w*(\\w)\\1{2,})\\w+\\b", tp_cl, 
                  replacement = " ", perl = TRUE)
    tp_cl <- stripWhitespace(tp_cl)
  }
  
  ## To remove stopwords and do stemming (for unigram)
  
  if (n == 1) {
    
    tp_cl <- removeWords(tp_cl, stopwords("en"))
    tp_cl <- replace_contraction(tp_cl, contraction = contractions, sent.cap = FALSE)
    tp_cl <- stemDocument(tp_cl, language = "english")
    tp_cl <- gsub("\\s*\\b(?=\\w*(\\w)\\1{2,})\\w+\\b", tp_cl, 
                  replacement = " ", perl = TRUE)
    tp_cl <- stripWhitespace(tp_cl)
  }
  
  return(tp_cl)
}
```

Function calls for data cleaning


```r
cl_tweet1 <- clean(en_tweet, badwords, 1)  ## twitter for unigram
cl_blog1 <- clean(en_blog, badwords, 1)    ## Blog for unigram
cl_news1 <- clean(en_news, badwords, 1)    ## News for unigram

cl_tweet0 <- clean(en_tweet, badwords, 0)  ## twitter for bi and tri gram
cl_blog0 <- clean(en_blog, badwords, 0)    ## Blog for bi and tri gram
cl_news0 <- clean(en_news, badwords, 0)    ## News for bi and tri gram
```

### Tokenization

The function in next code chunk will generate 1, 2, and 3-gram tokens.   


```r
df <- function(x, ntoken) {
  
  tp_tk <- concatenate(x)
  ng <- ngram(tp_tk, n = ntoken)
  tp_df <- get.phrasetable(ng)
  tp_df <- tp_df[,-3]
}

## Single Word tokenization function call

tweet1 <- df(cl_tweet1, 1); blog1 <- df(cl_blog1, 1) 
news1 <- df(cl_news1, 1) 

## 2-gram tokenization function call

tweet2 <- df(cl_tweet0, 2); blog2 <- df(cl_blog0, 2) 
news2 <- df(cl_news0, 2)

## 3-gram tokenization function call

tweet3 <- df(cl_tweet0, 3); blog3 <- df(cl_blog0, 3)
news3 <- df(cl_news0, 3)
```

## Exploratory Analysis

Frequency of word and word pairs are shown in the following bar plots using N-gram analysis above. Plotting functions can be found in the appendix section. Figure 2 compares the top fifteen frequently found Uni-gram tokens. The highest frequently used word in news corpus is 'said'. This particular word is used 2.5 times more than the frequency of the second highest word. There are no such dominating single word in blog and twitter samples. There are total 82946, 75001 and 72328 unique words in twitter, blog and news samples.


```
##      File Tot_Unigram Tot_Bigram Tot_Trigram
## 1 twitter       82946    1027552     2158357
## 2    blog       75001    1197674     2696061
## 3    news       72328    1210281     2560883
```


![](milestone_new_files/figure-html/chunk8-1.png)<!-- -->

I did not remove stop words prior to bi- and tri-gram tokenization during cleaning phase. The consequence is evident from next two bar plots. Figure 3 and 4 show the top fifteen frequently used bi- and tri-grams of all three corpora. Word pairs are mainly consist of stop words (Example: "i am", "of the", "i do not"). The 2nd top tri-gram in twitter is "thanks for the". This tri-gram indicates that twitter is a social media network, where users thank each other. There are total 1027552, 1197674 and 1210281 unique bi-grams; and 2158357, 2696061 and 2560883 unique tri-grams in twitter, blog and news data samples, respectively.

![](milestone_new_files/figure-html/chunk9-1.png)<!-- -->

![](milestone_new_files/figure-html/chunk10-1.png)<!-- -->

Word Cloud is another fancy and eye-catching way to demonstrate the distribution of words.

![](milestone_new_files/figure-html/chunk11-1.png)<!-- -->![](milestone_new_files/figure-html/chunk11-2.png)<!-- -->





Figure 5, shows the 15 most common (based on the sum of the tokens) uni-grams between twitter, blog and news corpus. There are 22587 common words between three corpora. 

Figure 6 and 7 show the top 15 common bi- and tri-grams between three corpora. There are 151904 and 110251 common bi- and tri-grams, respectively. Although, "thanks for the" tri-gram is in the top common list (mostly in twitter corpus) but almost unseen in blog and news corpora. 


![](milestone_new_files/figure-html/chunk13-1.png)<!-- -->![](milestone_new_files/figure-html/chunk13-2.png)<!-- -->![](milestone_new_files/figure-html/chunk13-3.png)<!-- -->


### How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?


```
##      File Unigram50 Unigram90
## 1 twitter       365      6414
## 2    blog       558      6831
## 3    news       667      7572
```

This table is very interesting. The total unique word in the twitter corpus is 85297 but only 365 word covers 50% of the word instances and for 90% coverage the unique word count increases exponentially to 6414. Similar, behavior is also observed for both blog and news corpora. 

### How do you evaluate how many of the words come from foreign languages?

One way to determine the foreign languages like Spanish or German (similar/same alphabets) is to compare the low frequency words with a foreign dictionary. In case of languages like Chinese or Arabic, we can remove carefully anything other than a-z and 0-9. The three corpora used in this project are in English. Amount of foreign word is very small and not important for this exploratory analysis. 


### Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

We can use synonyms and antonym to increase the coverage. We can also increase the coverage by context clustering. Words can be clustered together based on some context. 

## Future Plan 

- Memory management and performance optimization. 
- I have used only 10% of the data for this analysis. The next step will be to use a larger data set.
- I have to develop a process to detect misspellings and words from foreign language.
- I have to develop a technique to deal with no match.
- Finally, development of prediction algorithm. 



## Appendix:

### The complete code

#### Load libraries


```r
library(qdap)
library(qdapDictionaries)
library(tm)
library(ngram)
library(stringi)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)

set.seed(2341)
```

#### Read data files


```r
file1 <- file("en_US.twitter.txt")
twitter <- readLines(file1, skipNul = TRUE)
close(file1)

file1 <- file("en_US.blogs.txt")
blogs <- readLines(file1)
close(file1)

file1 <- file("en_US.news.txt", "rb")
news <- readLines(file1)
close(file1)

file1 <- file("badwords.txt")
badwords <- readLines(file1, encoding = "UTF-8")
close(file1)
```

#### Basic summary of three files


```r
## Function to calculate size of the object

size <- function(x) {
  format(object.size(x), units = "auto", standard = "SI")
} 

## Function to calculate number of charater without empty field (Approx.)

nnchar <- function(x) {
  temp <- stri_length(x) - stri_count_fixed(x, " ")
  sum(temp)
}

## Function to calculate number of words (Approx.)

nwords <- function(x) {
  temp1 <- stri_count_fixed(x, " ") + 1
  sum(temp1)
} 

info <- data.frame(File = c("twitter", "blog", "news"), 
                   Flsize = c(size(twitter), size(blogs), size(news)),
                   Noline = c(length(twitter), length(blogs), length(news)),
                   Nochar = c(nnchar(twitter), nnchar(blogs), nnchar(news)),
                   Noword = c(nwords(twitter), nwords(blogs), nwords(news)))

info$Flsize <- as.character(info$Flsize)

info

info1 <- gather(info, key = "Features", value = "Count", Noline:Noword)

infopl <- ggplot(info1, aes(fill = Features, x = File, y = Count))
infopl <- infopl + geom_bar(position = "dodge", stat = "identity")
infopl <- infopl + annotate("text", x = info$File, y = info$Nochar + 50000000,
                            label = c(info$Flsize[1], info$Flsize[2], info$Flsize[3]))
infopl <- infopl + scale_y_log10() + labs(title = "Figure 1: Approximate 
                                          summary of three corpora")
infopl
```

#### Sampling


```r
## Sample the data (10% of each corpus)

en_tweet <- sample(twitter, round(length(twitter)*0.1))
en_blog <- sample(blogs, round(length(blogs)*0.1))
en_news <- sample(news, round(length(news)*0.1))

## Function to convert from UTF-8 to ASCII 

en_tweet <- iconv(en_tweet, 'UTF-8', 'ASCII', sub = "")
en_blog <- iconv(en_blog, 'UTF-8', 'ASCII', sub = "")
en_news <- iconv(en_news, 'UTF-8', 'ASCII', sub = "")
```

#### Cleaning


```r
## Function for data cleaning

clean <- function(x, badwords, n) {
  
  tp_cl <- removePunctuation(x, preserve_intra_word_contractions = TRUE)
  tp_cl <- removeNumbers(tp_cl)
  tp_cl <- tolower(tp_cl)
  tp_cl <- removeWords(tp_cl, badwords)
  
  ## For bi- and tri-gram
  
  if (n == 0) {
    
    tp_cl <- replace_contraction(tp_cl, contraction = contractions, sent.cap = FALSE)
    
    ## Remove words with more than two repeated letters (Example: helllo or youuu)
    
    tp_cl <- gsub("\\s*\\b(?=\\w*(\\w)\\1{2,})\\w+\\b", tp_cl, 
                  replacement = " ", perl = TRUE)
    tp_cl <- stripWhitespace(tp_cl)
  }
  
  ## To remove stopwords and do stemming (for unigram)
  
  if (n == 1) {
    
    tp_cl <- removeWords(tp_cl, stopwords("en"))
    tp_cl <- replace_contraction(tp_cl, contraction = contractions, sent.cap = FALSE)
    tp_cl <- stemDocument(tp_cl, language = "english")
    tp_cl <- gsub("\\s*\\b(?=\\w*(\\w)\\1{2,})\\w+\\b", tp_cl, 
                  replacement = " ", perl = TRUE)
    tp_cl <- stripWhitespace(tp_cl)
  }
  
  return(tp_cl)
}
```

#### Data cleaning function call


```r
cl_tweet1 <- clean(en_tweet, badwords, 1)  ## twitter for unigram
cl_blog1 <- clean(en_blog, badwords, 1)    ## Blog for unigram
cl_news1 <- clean(en_news, badwords, 1)    ## News for unigram

cl_tweet0 <- clean(en_tweet, badwords, 0)  ## twitter for bi and tri gram
cl_blog0 <- clean(en_blog, badwords, 0)    ## Blog for bi and tri gram
cl_news0 <- clean(en_news, badwords, 0)    ## News for bi and tri gram
```

#### Tokenization


```r
df <- function(x, ntoken) {
  
  tp_tk <- concatenate(x)
  ng <- ngram(tp_tk, n = ntoken)
  tp_df <- get.phrasetable(ng)
  tp_df <- tp_df[,-3]
}

## Single Word tokenization function call

tweet1 <- df(cl_tweet1, 1); blog1 <- df(cl_blog1, 1) 
news1 <- df(cl_news1, 1) 

## 2-gram tokenization function call

tweet2 <- df(cl_tweet0, 2); blog2 <- df(cl_blog0, 2) 
news2 <- df(cl_news0, 2)

## 3-gram tokenization function call

tweet3 <- df(cl_tweet0, 3); blog3 <- df(cl_blog0, 3)
news3 <- df(cl_news0, 3)
```

#### Ngram Table


```r
table0 <- data.frame(File = c("twitter", "blog", "news"),
                     Tot_Unigram = c(dim(tweet1)[1], dim(blog1)[1], dim(news1)[1]),
                     Tot_Bigram = c(dim(tweet2)[1], dim(blog2)[1], dim(news2)[1]),
                     Tot_Trigram = c(dim(tweet3)[1], dim(blog3)[1], dim(news3)[1])
                     )
print(table0)
```

#### Unigram distribution bar plot


```r
fplot <- function(xy, tname) {
  p <- ggplot(xy[1:15,],aes(x = ngrams, y = freq))
  p <- p + geom_bar(stat = "identity", colour = "blue")
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
  p <- p + labs(title = tname)
}

p1 <- fplot(tweet1, "1-gram tweets")
p2 <- fplot(blog1, "1-gram blog")
p3 <- fplot(news1, "1-gram news")

fig1 <- ggarrange(p1, p2, p3, ncol = 3)
annotate_figure(fig1, 
                top = text_grob("Top ten (10) Uni-gram toknes", 
                                color = "red", face = "bold"),
                fig.lab = "Figure 2", fig.lab.face = "bold")
```

#### Bi-gram distribution bar plot


```r
p4 <- fplot(tweet2, "2-gram tweets")
p5 <- fplot(blog2, "2-gram blog")
p6 <- fplot(news2, "2-gram news")

fig2 <- ggarrange(p4, p5, p6, ncol = 3)
annotate_figure(fig2, 
                top = text_grob("Top ten (10) Bi-gram tokens", 
                                color = "red", face = "bold"),
                fig.lab = "Figure 3", fig.lab.face = "bold")
```

#### Tri-gram distribution bar plot


```r
p7 <- fplot(tweet3, "3-gram tweets")
p8 <- fplot(blog3, "3-gram blog")
p9 <- fplot(news3, "3-gram news")

fig3 <- ggarrange(p7, p8, p9, ncol = 3)
annotate_figure(fig3, 
                top = text_grob("Top ten (10) Tri-gram toknes", 
                                color = "red", face = "bold"),
                fig.lab = "Figure 4", fig.lab.face = "bold")
```

#### Wordcloud


```r
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "TWITTER unigram  distribution")
wordcloud(tweet1$ngrams, tweet1$freq, scale=c(3,0.2), 
          max.words=100, random.order=FALSE,rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8,"Accent")
          )

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "NEWS unigram distribution")
wordcloud(news1$ngrams, news1$freq, scale=c(4,0.5), 
          max.words=100, random.order=FALSE,rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8,"Accent")
          )
```

#### Common Ngrams


```r
matchxyz <- function (listxyz) {
  
  temp1 <- Reduce(function(x,y) merge(x, y, by = "ngrams"), listxyz, accumulate = FALSE)
  colnames(temp1) <- c("ngrams", "tweet", "blog", "news")
  temp1 <- mutate(temp1, tbn = tweet + blog + news)
  temp1 <- temp1[order(-temp1$tbn),]
  temp2 <- head(temp1, 15)
  temp2 <- gather(temp2, key = "Corpus", value = "freq", tweet:news)
  
  
  return(list(temp2, dim(temp1)[1]))

}

list1 <- list(tweet1, blog1, news1)
list2 <- list(tweet2, blog2, news2)
list3 <- list(tweet3, blog3, news3)

match1 <-  matchxyz(list1); match2 <-  matchxyz(list2); match3 <-  matchxyz(list3)
```

#### Common Ngram plots


```r
fplot1 <- function(dfmat, fnum, gnum, tnum) {

  p <- ggplot(dfmat, aes(x = ngrams, y = freq))
  p <- p + geom_bar(stat = "identity", colour = "orange")
  p <- p + facet_grid(~Corpus, labeller = label_both) + coord_flip()
  p <- p + labs(title = paste0("Figure ", fnum, ": 15 most common ", gnum, "-gram word
                               (Total ", tnum, " ngrams)"))
  p <- p + theme(plot.title = element_text(size=18))
  p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
    
    
  return(p)
  
}

p10 <- fplot1(match1[[1]], "5", "1", match1[[2]])
p11 <- fplot1(match2[[1]], "6", "2", match2[[2]])
p12 <- fplot1(match3[[1]], "7", "3", match3[[2]])

p10
p11
p12
```

#### Data Coverage


```r
csum <- function(dfc, pct) {
  sum1 = 0
  
  for (i in 1:dim(dfc)[1]) {
    
    sum1 <- dfc$freq[i] + sum1
    if (sum1 >= pct*sum(dfc$freq)) {break}
  }
  return(i)
}

table1 <- data.frame(File = c("twitter", "blog", "news"),
                     Unigram50 = c(csum(tweet1, 0.5), csum(blog1, 0.5), csum(news1, 0.5)),
                     Unigram90 = c(csum(tweet1, 0.9), csum(blog1, 0.9), csum(news1, 0.9))
                     )
print(table1)
```
