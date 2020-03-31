#load some packages
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)

library(tidytext)
library(tibble)
library(tidy)
library(tm)
library(wordcloud)

setwd("~/Desktop/R")

pruebas <- read.csv("Pruebas.csv")
medical <- read.csv("medical.csv")
pruebaspercent <- read.csv("PruebasPercent.csv")
qualitative <- read.csv("QualitativeFall2018.csv")


#clean up the data

pruebas2  <-  melt(pruebaspercent, id.vars=c("Student"))

x <- "pruebas$Blog.Entry"
Encoding(x) <- "UTF-8"


# Some basic graphing -----------------------------------------------------


pruebas2$variable <- factor(pruebas2$variable, levels=c("Pretest", "Midterm", "Final"))


#everything together
#pretest only
ggplot(pruebas2[pruebas2$variable=="Pretest",], aes(Student,value)) +theme_bw()+ ylab("Percentage Known on Pretest")+ geom_bar(stat="identity", position="dodge")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#all together in one graph
ggplot(pruebas2, aes(Student, fill=variable, value)) +theme_bw()+ ylab("Percentage Known on Pretest")+ geom_bar(stat="identity", position="dodge", na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#facet grid

ggplot(pruebas2, aes(Student, value, fill=variable)) + geom_bar(stat="identity", position="dodge")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~variable)+ labs(x="Test", y = "Percentage Known") + scale_fill_brewer(palette="Set2")


# loading text data and trying a word cloud -----------------------------------------------------
###https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8####

corpus <- SimpleCorpus(VectorSource(medical$Blog.Entry))

dfCorpus <- tm_map(corpus, stripWhitespace)
dfCorpus <- tm_map(dfCorpus, content_transformer(tolower))
dfCorpus <- tm_map(dfCorpus, removeNumbers)
dfCorpus <- tm_map(dfCorpus, removePunctuation)
dfCorpus <- tm_map(dfCorpus,removeWords,stopwords("spanish"))

#see stop words and which words are removed
stopwords("spanish")
corpus[[1]]$content
dfcorpus[[1]]$content

DTM <- DocumentTermMatrix(dfCorpus)
inspect(DTM)

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums)[colnames(sums)=="colSums(as.matrix(DTM))"] <- "count"
colnames(sums)[colnames(sums)=="rowname"] <- "word"
sums <- arrange(sums, desc(count))
head <- sums[1:75,]

wordcloud(words=head$word, freq=head$count, min.freq = 15,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


###graphing qualitative data#####

qualitative$MainCode <- factor(qualitative$MainCode, levels=c("Acquisition", "Autonomous Learning", "Affective Response", "Interactions"))

qualitative$SubCode <- factor(qualitative$SubCode, levels = qualitative[order(qualitative$Number), "SubCode"])

###qualitative$SubCode <- factor(qualitative$SubCode, levels=c("Self-study strategies", "Personal emotions", "Identifying gaps in knowledge", "Navigating interactions", "Using resources in the clinics", "Role of the interpreter", "Positive affect for patients", "Awareness of sociolinguistic variability", "Ascertaining knowledge", "Expressing awareness of learning", "Intercultural awareness", "Hopes"))


#by each category 
ggplot(qualitative[qualitative$MainCode=="Acquisition",], aes(SubCode,Number)) + ylab("Number of Occurrences")+ geom_bar(stat="identity", position="dodge")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(qualitative[qualitative$MainCode=="Autonomous Learning",], aes(SubCode,Number)) + ylab("Number of Occurrences")+ geom_bar(stat="identity", position="dodge")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(qualitative[qualitative$MainCode=="Affective Response",], aes(SubCode,Number)) + ylab("Number of Occurrences")+ geom_bar(stat="identity", position="dodge")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(qualitative[qualitative$MainCode=="Interactions",], aes(SubCode,Number)) + ylab("Number of Occurrences")+ geom_bar(stat="identity", position="dodge")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#all together in one graph
ggplot(qualitative, aes(MainCode, fill=SubCode, Number)) + ylab("Number of Occurrences") + geom_bar(stat="identity", position="dodge", na.rm=TRUE) +  scale_fill_brewer(palette="Paired") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ xlab("Main Code")

ggplot(qualitative, aes(MainCode, fill=SubCode, Number)) + ylab("Number of Occurrences") + geom_bar(stat="identity", position="dodge", na.rm=TRUE) +  scale_fill_brewer(palette="Set3") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ xlab("Main Code")

ggplot(qualitative, aes(MainCode, fill=SubCode, Number)) + ylab("Number of Occurrences")+ geom_bar(stat="identity", position="dodge", na.rm=TRUE)+theme(axis.text.x = element_text(angle = 45, hjust=1))+ geom_col(colour="black")+ xlab("Main Code")



ggplot(qualitative, aes(SubCode, Number)) + ylab("Number of Occurrences") + geom_bar(stat="identity", position="dodge", na.rm=TRUE) +  scale_fill_brewer(palette="Paired") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ xlab("Sub Codes")

#ordering the columns

ggplot(qualitative, aes(SubCode, Number)) +coord_flip() +  scale_fill_brewer(palette="Paired") + ylab("Number of Occurrences") + geom_bar(stat="identity", position="dodge", na.rm=TRUE, fill="orange")  +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ xlab("Sub Codes")

ggplot(qualitative, aes(SubCode, Number)) +coord_flip() +  scale_fill_brewer(palette="Paired") + ylab("Number of Occurrences") + geom_bar(stat="identity", position="dodge", na.rm=TRUE)+ xlab("Sub Codes")

ggplot(qualitative, aes(SubCode, Number)) +coord_flip()  + ylab("Number of Occurrences") + geom_bar(stat="identity", position="dodge", na.rm=TRUE)+ xlab("Sub Codes")


