##########################################################################
############## BUILD THE CORPUS USING TM PACKAGE #########################
##########################################################################

data<-read.csv(file="reutersCSV.csv",header=T,sep=",")
attach(data)

str(data)

#column number 139 is the tithle and 140 the text
require(tm)
x<-data[,139:140]
for(i in 1:21578){ #add title before the text and get only one text
  print(i)
  x[i,"doc.all"]<-paste(as.character(x$doc.title[i]),as.character(x$doc.text[i]), collapse = " ")
}
x<-x[-(1:2)]
colnames(x) #only "doc.all"
x<-as.vector(x[,1])#transform the column 'doc.all' as a vector

text<-as.character(x)
text[28] #the first text
reuters <- Corpus(VectorSource(text))
reuters
inspect(reuters[28])

#################################################################
############### DATA PRE-PROCESSING ON CORPUS ###################
#################################################################

reuters <- tm_map(reuters, tolower)
reuters <- tm_map(reuters, removePunctuation)
reuters <- tm_map(reuters, removeNumbers)
Stopwords<-c(stopwords("english"), "will", "said", "say", "reuter")
reuters <- tm_map(reuters, removeWords, Stopwords)
reuters<-tm_map(reuters, stemDocument)
reuters <- tm_map(reuters, removeWords, Stopwords)
reuters <- tm_map(reuters, stripWhitespace)
reuters <- tm_map(reuters, PlainTextDocument) 
# reuters <- tm_map(reuters, stemCompletion, reuters)#to long! run further

inspect(reuters[28])# check the first one cleaned text

##########################################################################
############## BUILD THE DOCUMENT TERM MATRIx #############################
##########################################################################

dtm <- DocumentTermMatrix(reuters)
tdm <- TermDocumentMatrix(reuters)
dtm

sparsity<-1:20
numberTerms<-1:20
for(i in 1:20){
  print(i)
  sparsity[i]<-0.78+i*0.01
  newdtm<-removeSparseTerms(dtm, sparsity[i])
  numberTerms[i]<-length(newdtm$dimnames$Terms)
}

plot(y=sparsity, x=numberTerms, ylab="Sparsity", xlab="Number of words in DTM",
     main="Realtionship between sparsity and number of words in DTM", type='o')


dtm # 40720 terms and 100% sparsity. it is too much! Try to keep 2000 or 200 terms only
dtm.2000 <- removeSparseTerms(dtm, 0.997)#keep 2081 terms
tdm.2000 <- removeSparseTerms(tdm, 0.997)#keep 2081 terms
dtm.2000
colnames(dtm.2000)
dtm.200 <- removeSparseTerms(dtm, 0.93)#keep 103 terms
tdm.200 <- removeSparseTerms(tdm, 0.93)#keep 103 terms
colnames(dtm.200)
dtm.250 <- removeSparseTerms(dtm, 0.96)#keep 103 terms
tdm.250 <- removeSparseTerms(tdm, 0.96)#keep 103 terms
colnames(dtm.200)
dtm.54 <- removeSparseTerms(dtm, 0.9)#keep 54 terms
tdm.54 <- removeSparseTerms(tdm, 0.9)#keep 54 terms
colnames(dtm.54)

## Convert tdm to a list of text
dtm2list <- apply(dtm.54, 1, function(x) {
  paste(rep(names(x), x), collapse=" ")
})
dtm2list[28]


#replace terms by other terms
month <- c("april","may","march")
dlr<-c("dlrs")
number<-c("one","two","three")

for(i in 1:length(dtm2list)){
  print(i)
  test <- dtm2list[i]
  for (j in 1:length(month)){
    test <-gsub(month[[j]], "month", test)
    test <-gsub(number[[j]], "number", test)
  }
  dtm2list[i] <-gsub("dlrs", "dlr", test)
}

## convert to a Corpus
myCorp <- VCorpus(VectorSource(dtm2list))

mydtm <- DocumentTermMatrix(myCorp)
colnames(mydtm)

freq <- sort(colSums(as.matrix(mydtm)), decreasing=TRUE)
freq

wf <- data.frame(word=names(freq), freq=freq)

#15 most populars world
library(ggplot2)   
p <- ggplot(subset(wf, freq>7400), aes(word, freq), main="15 most popular words")    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

#data visualisation
library(wordcloud)
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)



myCorp[[29]]

#create a data frame from the DTM using frequency

#see how many document don't have a vector representation:
dim(mydtm)
inspect(mydtm)[1:2,101:102]
dtf54 <- as.data.frame(inspect( mydtm ))
ncol(dtf54)
dtf54[1,]
rownames(dtf54)<- 1:nrow(mydtm)
#how many null vector
nbRowNULL<-NA
for(i in 1:21578){
  print(i)
  if(sum(dtf54[i,])==0){
    nbRowNULL<-c(nbRowNULL,i)
  }
}
nbRowNULL<-nbRowNULL[-1]
length(nbRowNULL)#389 to remove on the data set!!!!!!
dtf54<-dtf54[-nbRowNULL,]



############################################################
# compute the normalised term frequency: BAG OF WORDS ######
############################################################


bow.norm <- as.data.frame(inspect( weightTfIdf(mydtm, normalize = TRUE) ))
bow.norm <-bow.norm [-nbRowNULL,]
bow.norm[29,]
ncol(bow.norm)

######################

#ADD DOCUMENT FREQUENCY, INVERSE and TF*IDF: take from the bac of the code

#########################
#document frequency######
#########################
frequency.terms <- data.frame(matrix(0, ncol = 48, nrow = 1))
colnames(frequency.terms) <- colnames(mydtm)



for(i in 1:48){
  frequency.terms[1,i]<-sum(dtf54[,i]!=0)
  
}
frequency.terms



#######################################
#inverse document frequency############
#######################################
inverse.frequency.terms <- data.frame(matrix(0, ncol = 48, nrow = 1))
colnames(inverse.frequency.terms) <- colnames(mydtm)

for(i in 1:48){
  inverse.frequency.terms[1,i]<-log((21758-431)/frequency.terms[1,i])
  
}
inverse.frequency.terms



#data visualisation
library(wordcloud)
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(inverse.frequency.terms), sort(inverse.frequency.terms, decreasing=TRUE), min.freq = 2.10, colors=brewer.pal(6, "Dark2"))


sort(inverse.frequency.terms)


sort(bow.norm[29,])

#export csv file
write.csv(bow.norm, file = "bow.norm.csv") #here is the bag of words data frame


require(topicmodels)

#################################################################
############### CREATE 10 LDA TOPICS USING THE 54 WORDS #########
#################################################################

#48 words
dtm.new.54   <- mydtm[-nbRowNULL, ]
dtm.new.54[1,]
k<-10
lda.54 <- LDA(dtm.new.54, 10)
lda.proba.54<-lda.54@gamma
terms(lda.54)
terms(lda.54, 3)
nrow(lda.proba.54) #21189 rows
lda.proba.54[29,]

########## EXPORT IT
write.csv(lda.proba.250, file = "lda.proba.54.csv") #here is the LDA topic models data frame