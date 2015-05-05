#Neural Network. With Text
setwd("G:/vimal/data science/Analytical Edge/kaggle")
set.seed(144)

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTrain$Text = paste(NewsTrain$Headline, NewsTrain$Snippet, sep = ". ")
NewsTest$Text = paste(NewsTest$Headline, NewsTest$Snippet, sep = ". ")


library(tm)

# Then create a corpus from the headline variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Text, NewsTest$Text)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusHeadline)
dtm
inspect(dtm[1000:1005,505:515])
sparse = removeSparseTerms(dtm, 0.99)
sparse
TotWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(TotWords) = make.names(colnames(TotWords))

#Cluster

distances = dist(TotWords, method = "euclidean")

# Hierarchical clustering
clusterkos = hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterkos)

#6 or 9 clusters will be best
clusterGroups = cutree(clusterkos, k = 9)
table(clusterGroups)

#-------------------------------------------------Normal now--------------------------
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusHeadline)
dtm
inspect(dtm[1000:1005,505:515])
sparse = removeSparseTerms(dtm, 0.99)
sparse
HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
HeadlineWords$cluster = clusterGroups


# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of HeadlineWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(NewsTrain) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTrain"

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

# The tail function takes the last "n" rows of HeadlineWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(NewsTest) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTest"

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

# Note that this split of HeadlineWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!

#1 = yes and 0 = no
HeadlineWordsTrain$Popular = ifelse((NewsTrain$Popular == 0), 'No', 'Yes')
HeadlineWordsTrain$Popular = as.factor(HeadlineWordsTrain$Popular)

HeadlineWordsTrain$WordCount = ifelse((log(NewsTrain$WordCount) < 0), 0, log(NewsTrain$WordCount))
HeadlineWordsTest$WordCount = ifelse((log(NewsTest$WordCount) < 0), 0, log(NewsTest$WordCount))
summary(HeadlineWordsTrain)

library(qdap)
pol<- polarity(NewsTrain$Text)
HeadlineWordsTrain$polarity = pol[[1]]$polarity

pol<- polarity(NewsTest$Text)
HeadlineWordsTest$polarity = pol[[1]]$polarity

#Lets get some date and time :)
HeadlineWordsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
HeadlineWordsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

HeadlineWordsTrain$Weekday = HeadlineWordsTrain$PubDate$wday
HeadlineWordsTest$Weekday = HeadlineWordsTest$PubDate$wday

HeadlineWordsTrain$month = HeadlineWordsTrain$PubDate$mon
HeadlineWordsTest$month = HeadlineWordsTest$PubDate$mon

HeadlineWordsTrain$mday = HeadlineWordsTrain$PubDate$mday
HeadlineWordsTest$mday = HeadlineWordsTest$PubDate$mday

HeadlineWordsTrain$hour = HeadlineWordsTrain$PubDate$hour
HeadlineWordsTest$hour = HeadlineWordsTest$PubDate$hour

HeadlineWordsTrain$WType = ifelse((HeadlineWordsTrain$Weekday == 0) | (HeadlineWordsTrain$Weekday == 6), 1, 0)
HeadlineWordsTest$WType = ifelse((HeadlineWordsTest$Weekday == 0) | (HeadlineWordsTest$Weekday == 6), 1, 0)


#Other Vars
HeadlineWordsTrain$NewsDesk = ifelse((NewsTrain$NewsDesk != ''), NewsTrain$NewsDesk, "Other")
HeadlineWordsTest$NewsDesk = ifelse((NewsTest$NewsDesk != ''), NewsTest$NewsDesk, "Other")
HeadlineWordsTrain$NewsDesk = as.factor(HeadlineWordsTrain$NewsDesk)
HeadlineWordsTest$NewsDesk <- factor(HeadlineWordsTest$NewsDesk, levels = levels(HeadlineWordsTrain$NewsDesk))

HeadlineWordsTrain$SectionName = ifelse((NewsTrain$SectionName != ''), NewsTrain$SectionName, "Other")
HeadlineWordsTest$SectionName = ifelse((NewsTest$SectionName != ''), NewsTest$SectionName, "Other")
HeadlineWordsTrain$SectionName = as.factor(HeadlineWordsTrain$SectionName)
HeadlineWordsTest$SectionName <- factor(HeadlineWordsTest$SectionName, levels = levels(HeadlineWordsTrain$SectionName))

HeadlineWordsTrain$SubsectionName = ifelse((NewsTrain$SubsectionName != ''), NewsTrain$SubsectionName, "Other")
HeadlineWordsTest$SubsectionName = ifelse((NewsTest$SubsectionName != ''), NewsTest$SubsectionName, "Other")
HeadlineWordsTrain$SubsectionName = as.factor(HeadlineWordsTrain$SubsectionName)
HeadlineWordsTest$SubsectionName <- factor(HeadlineWordsTest$SubsectionName, levels = levels(HeadlineWordsTrain$SubsectionName))

#Random Forest
library(gmodels)
library(ROCR)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)

set.seed(21)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .mtry = seq(5,6,1))
train(Popular ~ WordCount  + polarity + NewsDesk + SectionName + SubsectionName  +
        month + hour + Weekday + WType, data = HeadlineWordsTrain, method = "rf", trControl = numFolds, tuneGrid = cpGrid )

HLWTrian = subset(HeadlineWordsTrain, select = c(NewsDesk,
                                                 WType,
                                                 hour,
                                                 Weekday,
                                                 polarity,
                                                 WordCount,
                                                 cluster,
                                                 word,
                                                 today,
                                                 read,
                                                 rais,
                                                 new,
                                                 morn,
                                                 get,
                                                 can,
                                                 busi,
                                                 Popular))
mod1 = glm(Popular~., data = HLWTrian, family=binomial)
summary(mod1)

logi_step <- step(mod1)
summary(logi_step)



set.seed(21)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .mtry = seq(6,8,1))
#train(Popular ~ NewsDesk + SectionName + SubsectionName + WType + hour + polarity + WordCount + cluster + 
#        word + today + read + rais + new + morn + get + can + busi, data = HeadlineWordsTrain, method = "rf", trControl = numFolds, tuneGrid = cpGrid )

fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

tr = train(Popular ~ NewsDesk + SectionName + SubsectionName + WType + hour + polarity + WordCount + cluster + 
             word + today + read + rais + new + morn + get + can + busi, data = HeadlineWordsTrain, 
             method="rf", nodesize=5, ntree=500, metric="ROC", trControl=fitControl)

summary(tr)

PredTest = predict(tr, newdata = HeadlineWordsTest, type='prob')
#PredictROC = predict(SimpleMod, newdata = NewsTest, type='prob')
OutRF = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest[,2])
write.csv(OutRF, "OutRF.csv", row.names=FALSE)

set.seed(111)

numFolds = trainControl( method = "cv", number = 10 )
nnetGrid = expand.grid( .size=c(6,8), .decay=c(0.008, 0.009,0.01,0.011))
#nnetGrid = expand.grid( .size= 20, .decay=0.006)
nnet = train(Popular ~ NewsDesk + SectionName + SubsectionName + WType + hour + polarity + WordCount + cluster + 
               word + today + read + rais + new + morn + get + can + busi,
               data = HeadlineWordsTrain, method = "nnet", trControl = numFolds, tuneGrid =  nnetGrid)
summary(nnet)
PredTest = predict(nnet, newdata = HeadlineWordsTest, type = 'prob')
OutNnet = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest[,2])
write.csv(OutNnet, "OutNnet.csv", row.names=FALSE)

finalsub = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = (OutRF$Probability1 +2*OutNnet$Probability1)/3)
write.csv(finalsub, "finalsub2.csv", row.names=FALSE)
