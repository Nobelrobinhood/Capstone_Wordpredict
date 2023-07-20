# Woordvoorspel APP
## Coursera - Data Science Specialization Capstone


## Coursera Data Science Capstone Project
The Coursera Data Science Specialization Capstone project from Johns Hopkins University (JHU) allows 
students to create a usable public data product that can show their skills to potential 
employers. Projects are drawn from real-world problems and are conducted with industry, government, 
and academic partners. In this part of course JHU is partnering with SwiftKey 
(https://swiftkey.com/) to apply data science in the area of natural language processing.
Data Science Capstone Project final result

# Introduction:
Data science is an inter-disciplinary field that uses scientific methods, processes, algorithms and systems to extract knowledge and insights from many structural and unstructured data Data science is related to data mining and big data.(wikipedia definition). Data can be everything: stuctured, unstructured, text, numbers. The source of the data can be text, measurment of sensors, pictures values, population information (tables),....

The data science pipline consist of following steps:


+ Getting the data: download or from digital medium
+ Exploring the clean data
+ Getting to know the dara: dimension, format, how it is organised, are there errors
+ Tidy up the data to get clean data
+ Model the data
+ Machine learning to predict the future based on that data
+ Story telling to bring out the results of the analysis

## This project:
This project consists of developing a predictive model of text (Predictive Text Analytics) using a Swifkey Company Dataset. The datasources are: english written: twitter-messages, a newspaper and a blog. The data was collected by a web-crawler. So it can contains error to have a clean language example.

## Some words about Natural language Processing and text-mining:

Natural Language Processing, usually shortened as NLP, is a branch of artificial intelligence that deals with the interaction between computers and humans using the natural language.

### What is NLP used for?
Natural Language Processing is the driving force behind the following common applications:
Language translation applications such as Google Translate
Word Processors such as Microsoft Word and Grammarly that employ NLP to check grammatical accuracy of texts.
Interactive Voice Response (IVR) applications used in call centers to respond to certain users’ requests.
Personal assistant applications such as OK Google, Siri, Cortana, and Alexa.
Why is NLP difficult?
Natural Language processing is considered a difficult problem in computer science. It’s the nature of the human language that makes NLP difficult.
The rules that dictate the passing of information using natural languages are not easy for computers to understand.
Some of these rules can be high-leveled and abstract; for example, when someone uses a sarcastic remark to pass information.
On the other hand, some of these rules can be low-levelled; for example, using the character “s” to signify the plurality of items.
Comprehensively understanding the human language requires understanding both the words and how the concepts are connected to deliver the intended message.
While humans can easily master a language, the ambiguity and imprecise characteristics of the natural languages are what make NLP difficult for machines to implement.

### How does Natural Language Processing Works?
NLP entails applying algorithms to identify and extract the natural language rules such that the unstructured language data is converted into a form that computers can understand.
When the text has been provided, the computer will utilize algorithms to extract meaning associated with every sentence and collect the essential data from them.
Sometimes, the computer may fail to understand the meaning of a sentence well, leading to obscure results.
For example, a humorous incident occurred in the 1950s during the translation of some words between the English and the Russian languages.
Here is the biblical sentence that required translation:
“The spirit is willing, but the flesh is weak.”
Here is the result when the sentence was translated to Russian and back to English:
“The vodka is good, but the meat is rotten.”
### What are the techniques used in NLP?
Syntactic analysis and semantic analysis are the main techniques used to complete Natural Language Processing tasks.
Here is a description on how they can be used.

#### Syntax
Syntax refers to the arrangement of words in a sentence such that they make grammatical sense.
In NLP, syntactic analysis is used to assess how the natural language aligns with the grammatical rules.
Computer algorithms are used to apply grammatical rules to a group of words and derive meaning from them.
Here are some syntax techniques that can be used:
Lemmatization: It entails reducing the various inflected forms of a word into a single form for easy analysis.
Morphological segmentation: It involves dividing words into individual units called morphemes.
Word segmentation: It involves dividing a large piece of continuous text into distinct units.
Part-of-speech tagging: It involves identifying the part of speech for every word.
Parsing: It involves undertaking grammatical analysis for the provided sentence.
Sentence breaking: It involves placing sentence boundaries on a large piece of text.
Stemming: It involves cutting the inflected words to their root form.

#### Semantics
Semantics refers to the meaning that is conveyed by a text. Semantic analysis is one of the difficult aspects of Natural Language Processing that has not been fully resolved yet.
It involves applying computer algorithms to understand the meaning and interpretation of words and how sentences are structured.
Here are some techniques in semantic analysis:
Named entity recognition (NER): It involves determining the parts of a text that can be identified and categorized into preset groups. Examples of such groups include names of people and names of places.
Word sense disambiguation: It involves giving meaning to a word based on the context.
Natural language generation: It involves using databases to derive semantic intentions and convert them into human language.
Wrapping up
Natural Language Processing plays a critical role in supporting machine-human interactions.
As more research is being carried in this field, we expect to see more breakthroughs that will make machines smarter at recognizing and understanding the human language.

#### Storytelling- result of project
The goal is to put a part of a sentence and than the app will give you a predicted word.
The tool is a shiny-app. Shiny is an R package that makes it easy to build interactive web apps straight from R. You can host standalone apps on a webpage or embed them in R Markdown documents or build dashboards. You can also extend your Shiny apps with CSS themes, htmlwidgets, and JavaScript actions.

# The  implementation
## Loading Libraries
```{r libraries}
library(knitr)
library(RColorBrewer)
library(stringi)
library(wordcloud2)
library(ggplot2)
library(ngram)
library(NLP)
library(tm)
library(RWeka)
library(slam)
library(xtable)
library(magrittr)
library(stringi)
library(quanteda)
```
For additional prediction accuracy we also used:
- Good-Turing Smoothing
- MLE
- linear-regression
- linguistic tweaks

## 1 Downloading the data and reading it
The Swifkey Dataset has been downloaded and unzipped manually from the below link:

https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

## Dataset
The underlying dataset for this word prediction app was gathered from three sources:

* Blogs
* News
* Twitter

The original english corpus combined over 580 MB of language information. Which summed up to over half a billion characters. After processing the data our model consists out of **millions of ngram tokens**.

This project will look only at the English language files: en_US.blogs.txt, en_US.news.txt, en_US.twitter.txt

Information about the 3 file types for the US set downloaded:

```{r getting data}
fileInformation <- function(filepath) {
  size <- file.info(filepath)$size/1048576
  
  conn <- file(filepath, "r")
  fulltext <- readLines(conn)
  nlines <- length(fulltext)
  
  maxline <- 0
  for (i in 1:nlines) {
    linelength <- nchar(fulltext[i])
    if (linelength > maxline) { maxline <- linelength }
  }
  
  nwords <- sum(stri_count_words(fulltext))
  
  close(conn)
  
  list(size=size, nlines=nlines, maxline=maxline, nwords=nwords)
}

```

### Some analysis of raw data :  Word counts, line counts and basic data tables
```{r basicstats}
data_dir <- "C:/Users/Rob/Desktop/Capstone/Dataset/Coursera-SwiftKey/final/en_US/"

blogs <- fileInformation(paste0(data_dir,"en_US.blogs.txt"))
news<- fileInformation(paste0(data_dir,"en_US.news.txt"))
twitter <- fileInformation(paste0(data_dir,"en_US.twitter.txt"))
matrix(c(blogs[1],blogs[2],blogs[3],blogs[4], 
         news[1],news[2],news[3],news[4],
         twitter[1],twitter[2],twitter[3],twitter[4]), 
      nrow = 3, ncol = 4, byrow = TRUE,
      dimnames = list(c("Blogs Info:", "News Info:", "Twit Info:"),
                 c("Size (MB)", "No of Lines", "Longest Line (No characters)", "No of words")))
```
So we see as expected that twitter has more lines for even count of words, that means that twitter are short messages.

## 2 Begin of our model: Sampling the data

Only a portion of the data will be used for an initial analysis, therefore getting a sample for the 3 file types for US set: blogs, news, twitter. A Corpus (collection of documents) is also created based on the 3 samples.For this analysis, only 1000 lines of text, for each set, were sampled. 
It would be interesting to get a wider sample for the prediction algorithm (80% modelling/ 20% testing)

```{r corpus}
conb <- file("C:/Users/Rob/Desktop/Capstone/Dataset/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", "r")

conn <- file("C:/Users/Rob/Desktop/Capstone/Dataset/Coursera-SwiftKey/final/en_US/en_US.news.txt", "r")

cont <- file("C:/Users/Rob/Desktop/Capstone/Dataset/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r")

```

Sampling first 1000 lines of text: readLines(conb, 1000)

```{r sampling}
blogs <-  readLines(conb, 1000)
news <-   readLines(conn, 1000)
twitter <-   readLines(cont, 1000)

corpus <- VCorpus(VectorSource(c(blogs, news, twitter)), readerControl=list(readPlain, language="en", load=TRUE))

close(conb)
close(conn)
close(cont)
```
  

## Exploration and Data Cleaning
This section will use the text mining library ‘tm’ (loaded previously) to perform Data cleaning tasks, which are meaningful in Predictive Text Analytics. Main cleaning steps are:

Converting the document to lowercase

Removing punctuation marks
Removing numbers
Removing stopwords (i.e. “and”, “or”, “not”, “is”, etc)
Removing undesired terms
Removing extra whitespaces generated in previous 5 steps
The above can be achieve with some of the TM package functions; let’s take a look to each cleaning task, individually:

Converting the document to lowercase: having the entire text in lowercase format is beneficial as it would avoid comparisons between lowercase, uppercase or a mix of those:
```{r}
corpus_lowercase <- tm_map(corpus, content_transformer(tolower))
```

Removing punctuation marks Good to note that this could generate some issues like apostrophes one, which in English are commonly used.

```{r}
corpus_low_punct <- tm_map(corpus_lowercase, removePunctuation)
```



Removing numbers As typically predicting a number is quite a challenging task, decided to remove those from the data
```{r}
corpus_low_punct_no <- tm_map(corpus_low_punct, removeNumbers)
```

## Lemmatization
Removing stopwords (i.e. “and”, “or”, “not”, “is”, etc) Stopwords are words that appear so often in the text that they are not very useful for a prediction algorimth as they don’t add too much value. A good exercise before removing this type of words would be to check how common they are in the text and decide after if they are considered stopwords or not.
Nevertheless, the TM package already includes a collection of stopwords for several different languages.

```{r}
corpus_low_punct_no_stop <- tm_map(corpus_low_punct_no, removeWords,stopwords("english"))
```


Removing undesired terms In a first exploration of the datasets, we could see they contain a lot of “profanity” words, which potentially would need to be removed; nevertheless, they could have some weight in the prediction results so therefore we can always consider this step at a later stage, depending on needs.

Removing extra whitespaces generated in previous 5 steps
```{r}
corpus_low_punct_no_stop_white <- tm_map(corpus_low_punct_no_stop, stripWhitespace)
```


## Analysis of the cleaned data
Data is now cleaned and ready to be analysed, so for instance we could check:

Some words are more frequent than others - what are the distributions of word frequencies?
What are the frequencies of 2-grams and 3-grams in the dataset?
The above can be achieve by tokenizing the data as: Unigram (1-Gram), Bigram (2-Gram), Trigram (3-Gram)

### 1 Gram
```{r}
gram1 = as.data.frame((as.matrix(  TermDocumentMatrix(corpus_low_punct_no_stop_white) )) ) 
gram1v <- sort(rowSums(gram1),decreasing=TRUE)
gram1d <- data.frame(word = names(gram1v),freq=gram1v)
gram1d[1:10,]
```
### 2 Gram
```{r}
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
gram2= as.data.frame((as.matrix(  TermDocumentMatrix(corpus_low_punct_no_stop_white,control = list(tokenize = bigram)) )) ) 
gram2v <- sort(rowSums(gram2),decreasing=TRUE)
gram2d  <- data.frame(word = names(gram2v),freq=gram2v)
gram2d[1:10,]
```
### 3 Gram
```{r}

trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
gram3 = as.data.frame((as.matrix(  TermDocumentMatrix(corpus_low_punct_no_stop_white,control = list(tokenize = trigram)) )) ) 
gram3v <- sort(rowSums(gram3),decreasing=TRUE)
gram3d <- data.frame(word = names(gram3v),freq=gram3v)
gram3d[1:10,]
```


Histogram of the 30st Top Unigrams
```{r}
ggplot(gram1d[1:30,], aes(x=reorder(word, freq),y=freq)) + 
        geom_bar(stat="identity", width=0.5, fill="tomato2") + 
        labs(title="Unigrams")+
        xlab("Unigrams") + ylab("Frequency") + 
        theme(axis.text.x=element_text(angle=65, vjust=0.6))
```




WordCloud of the 20st Top BiGrams
https://www.r-graph-gallery.com/196-the-wordcloud2-library/

+ add simplicity and clarity. The most used keywords stand out better in a word cloud
+ are a potent communication tool. They are easy to understand, to be shared and are impactful
+ are visually engaging than a table data

```{r}
wordcloud2(gram2d[1:20,],size=1.6,shape = 'star')
```

# 3 Interesting findings in the dataset.
When loading the data, “news” has an incomplete final line; “twitter” set seems to contain some nulllines. As per error processing info displayed.

There are still some accents that weren’t completely removed in the cleaning step; it would be cleaner for the algorithm to remove those in a second cleaning round.

# 4 Feedback on plans for creating a prediction algorithm and Shiny app.
As previously said: for this analysis, only 1000 lines of text, for each set, were sampled. It would be interesting to get a wider sample for the prediction algorithm (80% modelling/ 20% testing)
When satisfied with a more representative and better-cleaned sample data, the algorithm can be implemented and refined (code optimization) as testing along


## Next word prediction app (using N-gram models)
- based on N-gram model with "Stupid Backoff" ([Brants et al 2007](http://www.cs.columbia.edu/~smaskey/CS6998-0412/supportmaterial/langmodel_mapreduce.pdf));
- checks if highest-order (in this case, n=4) n-gram has been seen. If not "degrades" to a lower-order model (n=3, 2);
- build on `cracklib-small` dictionary.





# Working of the App:

### Text Area:

Enter the sentence without the final word to be predicted. `Text Area`
allows copy and paste. Text is not case sensitive.

### Clear button:

Delete `Text Area`

### Prediction buttons:

When one of `Prediction` buttons is pressed it merges predicted word to `Text Area` sentence, to do further predictions.  

## N-grams tables:

Select from `unigrams`, `bigrams`, `trigrams` and `tetragrams` to display desired table.

All N-grams tables have four columns: `sentence`, `prediction`, `frequency` and `probability`.

## N-grams plot

Select from `unigrams`, `bigrams`, `trigrams` and `tetragrams` to display
n-grams barplot ordered by `probability`.

## About:
Shows this page.

## Links:

### Packages:

* [Getting Started with quanteda](https://cran.rstudio.com/web/packages/quanteda/vignettes/quickstart.html)
* [Quanteda docs](https://cran.r-project.org/web/packages/quanteda/quanteda.pdf)

### Dictionaries:

* [cracklib-small](https://github.com/cracklib/cracklib/blob/master/src/dicts/cracklib-small)

# Reference for part "Some words about Natural language Processing and text-mining": https://becominghuman.ai/a-simple-introduction-to-natural-language-processing-ea66a1747b32
