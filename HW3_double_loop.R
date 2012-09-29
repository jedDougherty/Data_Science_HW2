install.packages("RJSONIO","foreach","plyr")


#Load JSON package.
require(RJSONIO)
require(foreach)
require(plyr)
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
