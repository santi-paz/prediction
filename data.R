library(tm)
library(stringi)
library(RWeka)
library(SnowballC)

ent <- readLines('data/final/en_US/en_US.twitter.txt', encoding = 'UTF-8')
enn <- readLines('data/final/en_US/en_US.news.txt', encoding = 'UTF-8')
enb <- readLines('data/final/en_US/en_US.blogs.txt', encoding = 'UTF-8')
set.seed(1)
index <- sample(1:length(ent),10000)
subenttr <- ent[index]
subentts <- ent[-index]
subentts <- subentts[index]
set.seed(1)
index <- sample(1:length(enn),20000)
subenntr <- enn[index]
subennts <- enn[-index]
set.seed(1)
subennts <- subennts[sample(1:length(subennts),10000)]
set.seed(1)
index <- sample(1:length(enb),30000)
subenbtr <- enb[index]
subenbts <- enb[-index]
set.seed(1)
subenbts <- subenbts[sample(1:length(subenbts),10000)]
suben <- c(subenttr,subenntr,subenbtr)
subentest <- c(subentts,subennts,subenbts)
rm(enb,enn,ent,subenbtr,subenntr,subenttr,subenbts,subennts,subentts)

################################
# get the ASCII charact
ascllen <- stri_enc_toascii(suben)
ascllen <- stri_replace_all_regex(ascllen,'\032','')
en <- Corpus(VectorSource(ascllen))
# change the capital characters to lower case
enall <- tm_map(en, content_transformer(tolower))
# remove the punctuation
enall <- tm_map(enall, removePunctuation)
# remove the numbers
enall <- tm_map(enall, removeNumbers)
# remove the stop words
enall <- tm_map(enall, removeWords, stopwords("english"))
# stemming the words
enall <- tm_map(enall, stemDocument,language = ("english"))
# remove the space more than one
enall <- tm_map(enall, stripWhitespace)

#################################
options(mc.cores=1)
# get the unigram corpus control
ctrl <- list(tokenize = words, bounds = list(global = c(10,Inf)))
# get the bigram corpus control
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
ctrl2 <- list(tokenize = BigramTokenizer, bounds = list(global = c(10,Inf)))
# get the trigram corpus control
TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
ctrl3 <- list(tokenize = TrigramTokenizer, bounds = list(global = c(10,Inf)))

# get the 1 to 3 gram corpus control
Tokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))}
ctrl0 <- list(tokenize = Tokenizer, bounds = list(global = c(10,Inf)))


# get the corpus
en.tdm <- TermDocumentMatrix(enall,control = ctrl)
en.bitdm <- TermDocumentMatrix(enall,control = ctrl2)
en.tritdm <- TermDocumentMatrix(enall,control = ctrl3)
# en.teratdm <- TermDocumentMatrix(enall,control = ctrl4)
en.tdm0 <- TermDocumentMatrix(enall,control = ctrl0)
# get the freqency of the corpus
library(slam)
freq <- rowapply_simple_triplet_matrix(en.tdm,sum)
freqbi <- rowapply_simple_triplet_matrix(en.bitdm,sum)
freqtri <- rowapply_simple_triplet_matrix(en.tritdm,sum)
# freqtera <- rowapply_simple_triplet_matrix(en.teratdm,sum)
freq0 <- rowapply_simple_triplet_matrix(en.tdm0,sum)

##############################################################
# get the most frequency words occured more than 1000 times in 60000 samples
ctrl <- list(tokenize = words, bounds = list(global = c(1000,Inf)))
enall.dtm <- DocumentTermMatrix(enall, control = ctrl)
classvec <- factor(c(rep('twitter',10000),rep('news',20000),rep('blogs',30000)))
datac <- data.frame(cbind(as.matrix(enall.dtm),classvec))
set.seed(1)
index <- sample(1:60000,40000)
datatrain <- datac[index,]
datatest <- datac[-index,]
# build a classification tree for pre class the data
library(party)
ct <- ctree(classvec ~ ., data=datatrain)
summary(ct)
classpre <- predict(ct,datatest)
table(classpre,datatest$classvec)

##3333333333333333333333333333333333333333333333333
# read in the data
ent <- readLines('data/final/en_US/en_US.twitter.txt', skipNul = TRUE, encoding = 'UTF-8')
enn <- readLines('data/final/en_US/en_US.news.txt', skipNul = TRUE, encoding = 'UTF-8')
enb <- readLines('data/final/en_US/en_US.blogs.txt', skipNul = TRUE, encoding = 'UTF-8')
# get 200000 from twiiter
set.seed(1)
index <- sample(1:length(ent),80000)
subenttr <- ent[index]
subentts <- ent[-index]
set.seed(1)
index <- sample(1:80000,10000)
subentts <- subentts[index]
# get 400000 from news
set.seed(1)
index <- sample(1:length(enn),70000)
subenntr <- enn[index]
subennts <- enn[-index]
set.seed(1)
index <- sample(1:70000,10000)
subennts <- subennts[index]
# get 20000 from blog
set.seed(1)
index <- sample(1:length(enb),80000)
subenbtr <- enb[index]
subenbts <- enb[-index]
set.seed(1)
index <- sample(1:80000,10000)
subenbts <- subenbts[index]
# clean the data
ascllenttr <- stri_enc_toascii(subenttr)
ascllenttr <- stri_replace_all_regex(ascllenttr,'\032','')
ascllentts <- stri_enc_toascii(subentts)
ascllentts <- stri_replace_all_regex(ascllentts,'\032','')

ascllenntr <- stri_enc_toascii(subenntr)
ascllenntr <- stri_replace_all_regex(ascllenntr,'\032','')
ascllennts <- stri_enc_toascii(subennts)
ascllennts <- stri_replace_all_regex(ascllennts,'\032','')

ascllenbtr <- stri_enc_toascii(subenbtr)
ascllenbtr <- stri_replace_all_regex(ascllenbtr,'\032','')
ascllenbts <- stri_enc_toascii(subenbts)
ascllenbts <- stri_replace_all_regex(ascllenbts,'\032','')

train <- VectorSource(c(ascllenttr,ascllenntr,ascllenbtr))
test <- VectorSource(c(ascllentts,ascllennts,ascllenbts))
en <- Corpus(train)
entest <- Corpus(test)

rm(enb,enn,ent,subenbtr,subenntr,subenttr,subenbts,subennts,subentts)

save(en,entest,file='data/en.RData')
load('data/en.RData')

enall <- tm_map(en, content_transformer(tolower))
enall <- tm_map(enall, removePunctuation)
enall <- tm_map(enall, removeNumbers)
enall <- tm_map(enall, removeWords, stopwords("english"))
# enall <- tm_map(enall, stemDocument,language = ("english"))
enall <- tm_map(enall, stripWhitespace)

# extract the n-gram for modeling

ctrl <- list(tokenize = words, bounds = list(global = c(6,Inf)))
options(mc.cores=1)
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
ctrl2 <- list(tokenize = BigramTokenizer, bounds = list(global = c(6,Inf)))
TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
ctrl3 <- list(tokenize = TrigramTokenizer, bounds = list(global = c(6,Inf)))
en.tdm <- TermDocumentMatrix(enall,control = ctrl)
en.bitdm <- TermDocumentMatrix(enall,control = ctrl2)
en.tritdm <- TermDocumentMatrix(enall,control = ctrl3)
library(slam)
freq <- rowapply_simple_triplet_matrix(en.tdm,sum)
freqbi <- rowapply_simple_triplet_matrix(en.bitdm,sum)
freqtri <- rowapply_simple_triplet_matrix(en.tritdm,sum)
firstname <- sapply(strsplit(names(freqbi), ' '), function(a) a[1])
secname <- sapply(strsplit(names(freqbi), ' '), function(a) a[2])
firsttriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[1])
sectriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[2])
tritriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[3])
# length(words1 <- unique(names(freq)))
# length(words2 <- unique(c(secname,firstname)))
# length(words3 <- unique(c(tritriname,sectriname,firsttriname)))
# length(finalwords3 <- intersect(intersect(words1,words2),words3))
# length(finalwords2 <- intersect(words1,words2))

# get the final n-gram dataframe

unigramDF <- data.frame(names(freq),freq,stringsAsFactors = F)
bigramDF <- data.frame(names(freqbi),freqbi,firstname,secname,stringsAsFactors = F)
trigramDF <- data.frame(names(freqtri),freqtri,paste(firsttriname,sectriname),tritriname,stringsAsFactors = F)
names(unigramDF) <- c('unigram','freq')
names(bigramDF) <- c('bigram','freq','unigram','name')
names(trigramDF) <- c('trigram','freq','bigram','name')
save(unigramDF,bigramDF,trigramDF,file = 'data/ngram.RData')
load('data/ngram.RData')
