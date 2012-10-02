install.packages("RJSONIO","foreach","plyr")


#Load JSON package.
require(RJSONIO)
require(foreach)
require(plyr)
require(tm)
#Define facets to search.
facets=list("Arts","Business","Obituaries","Sports","World")
queries <- list()



#Define parameters.
api <- "edf9f44cfdde2bc23a53be2830873ce7:11:62471970" #Eurry's API: 687d601e0e5d64f3ea200c0257002ebf:1:44679323
                                                      #Jed's API: edf9f44cfdde2bc23a53be2830873ce7:11:62471970
facet <- vector("list",length(facets))
records <- 2000
os <- 0:(records/10-1)
fields <- list("url%2Ctitle%2Cbody")
rank <- "newest"

#Set up query output table.
articles <- list()

#Check for missing bodys in articles and hold a fixed, complete row. 
addBody <- c("body",NA)
fixRow <- function(x) {
    inAdeq <- ldply(x[[rowNum]])
    rePlace <- t(rbind(addBody, inAdeq))
    toList <- rePlace[-1,1:3]
    names(toList) <- c("body","title","url")
    return(toList)
}

#Loop to insert facets.
query <- foreach(i= 1:length(facets))  %do%
  sprintf("nytd_section_facet%%3A%%5B%s%%5D", facets[[i]])
Queries <- ldply(query) #hold query sections for use in URI construction.

# Gather All articles and transform them into dataframes.
for(j in 1:5){
  for (i in 1:length(os)) {
    uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", Queries[j,],
                  "&offset=", os[i], 
                  "&fields=", fields,
                  "&rank=", rank,
                  "&api-key=", api,
                  sep="")
    data <- readLines(uri, warn="F") # get them
    trnslt  <- fromJSON(data) # tokenize
    rowNum <- which(sapply(trnslt$results,length)<3, arr.ind=TRUE) #returns row w/ missing body.
        #Run fixRow function to insert NA in missing body column.
        if(sum(rowNum, na.rm=TRUE)>0) {
            z<-fixRow(trnslt$results) #R didn't want me to do this directly, so I assigned it to my BFF, z.
            trnslt$results[[rowNum]] <- z}
    articles <- append(articles, unlist(trnslt$results))
    Sys.sleep(.1)
  }
  assign(paste("articles_",facets[[j]],sep=""),data.frame(matrix(unlist(articles), nrow=2000,byrow=T))) 
  rm(articles)
  articles <- list()
}

#Add descriptive row to each table. Not necessary but
#makes this way way easier later.
articles_Arts$newcol <- apply(articles_Arts,1,function(row) "Arts")
articles_Business$newcol <- apply(articles_Business,1,function(row) "Business")
articles_Obituaries$newcol <- apply(articles_Obituaries,1,function(row) "Obituaries")
articles_Sports$newcol <- apply(articles_Sports,1,function(row) "Sports")
articles_World$newcol <- apply(articles_World,1,function(row) "World")

#Combines the articles into one table.
#We need to do this because we are training the 
#machine to compare articles to eachother.
articles_All <- rbind(articles_Arts,articles_Business,
                      articles_Obituaries,articles_Sports,articles_World)

names(articles_All) <- c("Body","Title","URL","Type")

##########################################################################
#Remove punctuation, stopwords, and numbers from articles_All
##########################################################################
#Read in Stopwords

stopw <- read.table("http://jakehofman.com/ddm/wp-content/uploads/2012/03/stopwords.txt",header=F)
stopw <- t(stopw)

#Subset, Text cleaning Body

articles_All$Body <- tolower(articles_All$Body)
articles_All$Body <- gsub("\\d", "", articles_All$Body)
articles_All$Body <- gsub("ldquo", "", articles_All$Body)
articles_All$Body <- gsub("quo ", " ", articles_All$Body)
articles_All$Body <- gsub("quos ", " ", articles_All$Body)
for(i in 1:length(stopw)){
  articles_All$Body <- gsub(paste(" ",stopw[i]," ", sep="")," ",articles_All$Body)
}
articles_All$Body <- gsub("[[:punct:]]", "", articles_All$Body)
articles_All$Body <- stemDocument(articles_All$Body)
articles_All$Body <- removeWords(articles_All$Body,stopwords("english"))

#Subset, Text cleaning Titles

articles_All$Title <- tolower(articles_All$Title)
articles_All$Title <- gsub("\\d", "", articles_All$Title)
articles_All$Title <- gsub("ldquo", "", articles_All$Title)
articles_All$Title <- gsub("quo ", " ", articles_All$Title)
articles_All$Title <- gsub("quos ", " ", articles_All$Title)
for(i in 1:length(stopw)){
  articles_All$Title <- gsub(paste(" ",stopw[i]," ", sep="")," ",articles_All$Title)
}
articles_All$Title <- gsub("[[:punct:]]", "", articles_All$Title)
articles_All$Title <- stemDocument(articles_All$Title)
articles_All$Title <- removeWords(articles_All$Title,stopwords("english"))

#Creates a sparse matrix of the word counts in each article

all_Corpus <- Corpus(VectorSource(paste(articles_All$Body,articles_All$Title,sep=" ")))
all_dtm <- DocumentTermMatrix(all_Corpus)
all_dtm <- weightBin(all_dtm)
all_nosparse <- removeSparseTerms(all_dtm, 0.998)

#Turns it into a regular matrix
all_matrix <- as.matrix(all_nosparse)
#converts it to a data frame
all_df <- as.data.frame(all_matrix)
all_df$newcolumn <- articles_All$Type
#Splits the data.frame into random train and test variables

s <- sample(10000, 5000)
train <- all_df[s,] 
test <- all_df[-s,]

train$newcolumn <- as.factor(train$newcolumn)
test_correct <- as.factor(test$newcolumn)
test <- test[,!(names(test) %in% "newcolumn")]
names(test)

##################
#Train Naive Bayes Classifier
##################
naive.worker <- function(data, class)
{
  holder <- data[which(data$newcolumn==class),1:(ncol(data)-1)]
}

naive.slave <- function(data, class)
{
  holder <- data[which(data$newcolumn!=class),1:(ncol(data)-1)]
}

train_art <- naive.worker(train,"Arts")
prior_art <- nrow(train_art)/nrow(train)
counts_art <- colSums(train_art)

art_slave <- naive.slave(train,"Arts")
counts_art_slave <- colSums(art_slave)

train_business <- naive.worker(train,"Business")
prior_business <- nrow(train_business)/nrow(train)
counts_business <- colSums(train_business)

bus_slave <- naive.slave(train,"Business")
counts_bus_slave <- colSums(bus_slave)

train_obit <- naive.worker(train,"Obituaries")
prior_obit<- nrow(train_obit)/nrow(train)
counts_obit <- colSums(train_obit)

obit_slave <- naive.slave(train,"Obituaries")
counts_obit_slave <- colSums(obit_slave)

train_sports <- naive.worker(train,"Sports")
prior_sports <- nrow(train_sports)/nrow(train)
counts_sports <- colSums(train_sports)

sports_slave <- naive.slave(train,"Sports")
counts_sports_slave <- colSums(sports_slave)

train_world <- naive.worker(train,"World")
prior_world <- nrow(train_world)/nrow(train)
counts_world <- colSums(train_world)

world_slave <- naive.slave(train,"World")
counts_world_slave <- colSums(world_slave)

priors <- c(prior_art,prior_business,prior_obit,prior_sports,prior_world)
sums <- c(nrow(train_art),nrow(train_business),nrow(train_obit),nrow(train_sports),nrow(train_world))
counts <- data.frame(counts_art,counts_business,counts_obit,counts_sports,counts_world)
probs <- (counts+1)/(sums+2)#Add alpha and beta here in the form (counts + Alpha-1)/(sums + alpha + beta-2)

slave_sums <- c(nrow(art_slave),nrow(bus_slave),nrow(obit_slave),nrow(sports_slave),nrow(world_slave))
slave_counts <- data.frame(counts_art_slave,counts_bus_slave,counts_obit_slave,counts_sports_slave,counts_world_slave)
slave_probs <- (slave_counts+1)/(slave_sums+2)#Add alpha and beta here in the form (counts + Alpha-1)/(sums + alpha + beta-2)

##############
#Test Bayes
##############
log_arts <- rowSums(test*(log((probs$counts_art*(1-slave_probs$counts_art_slave))/(slave_probs$counts_art_slave*(1-probs$counts_art)))))
log_else <- log((1-probs$counts_art)/(1-slave_probs$counts_art_slave)) +log(priors[1]/(1-priors[1]))
log_odds_arts <- log_arts + log_else
#####################
#No longer necessary
###########################

#Save text files of Section pulls.
write.table(articles_Arts, file="articles_Arts.txt", sep="\t")
write.table(articles_Business, file="articles_Business.txt", sep="\t")
write.table(articles_Obituaries, file="articles_Obituaries.txt", sep="\t")
write.table(articles_Sports, file="articles_Sports.txt", sep="\t")
write.table(articles_World, file="articles_World.txt", sep="\t")

#Create samples for training.
articles_Arts_train <- articles_Arts[sample(1:nrow(articles_Arts),1000),]
articles_Business_train <- articles_Business[sample(1:nrow(articles_Business),1000),]
articles_Obituaries_train <- articles_Obituaries[sample(1:nrow(articles_Obituaries),1000),]
articles_Sports_train <- articles_Sports[sample(1:nrow(articles_Sports),1000),]
articles_World_train <- articles_World[sample(1:nrow(articles_World),1000),]

#Save text files of training samples.
write.table(articles_Arts_train, file="articles_Arts_train.txt", sep="\t")
write.table(articles_Business_train, file="articles_Business_train.txt", sep="\t")
write.table(articles_Obituaries_train, file="articles_Obituaries_train.txt", sep="\t")
write.table(articles_Sports_train, file="articles_Sports_train.txt", sep="\t")
write.table(articles_World_train, file="articles_World_train.txt", sep="\t")

#Create samples for testing.
articles_Arts_test <- articles_Arts[sample(1:nrow(articles_Arts),1000),]
articles_Business_test <- articles_Business[sample(1:nrow(articles_Business),1000),]
articles_Obituaries_test <- articles_Obituaries[sample(1:nrow(articles_Obituaries),1000),]
articles_Sports_test <- articles_Sports[sample(1:nrow(articles_Sports),1000),]
articles_World_test <- articles_World[sample(1:nrow(articles_World),1000),]

#Save text files of training samples.
write.table(articles_Arts_test, file="articles_Arts_test.txt", sep="\t")
write.table(articles_Business_test, file="articles_Business_test.txt", sep="\t")
write.table(articles_Obituaries_test, file="articles_Obituaries_test.txt", sep="\t")
write.table(articles_Sports_test, file="articles_Sports_test.txt", sep="\t")
write.table(articles_World_test, file="articles_World_test.txt", sep="\t")
