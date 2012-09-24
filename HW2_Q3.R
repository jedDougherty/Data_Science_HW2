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
    articles <- append(articles, unlist(trnslt$results))
    Sys.sleep(.1)
}
assign(paste("articles_",Queries[j,],sep=""),data.frame(matrix(unlist(articles), nrow=2000,byrow=T))) 
rm(articles)
articles <- list()
}
#Transform list into Arts dataframe.
articles_arts <- data.frame(matrix(unlist(articles), nrow=2000,byrow=T))
colnames(articles_arts) <- c("body","title","url")

# Gather Business articles.
for (i in 1:length(os)) {
    uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", Queries[2,],
                  "&offset=", os[i], 
                  "&fields=", fields,
                  "&rank=", rank,
                  "&api-key=", api,
                  sep="")
    data <- readLines(uri, warn="F") # get them
    trnslt  <- fromJSON(data) # tokenize
    articles <- append(articles, unlist(trnslt$results))
}
#Transform list into Business dataframe.
articles_bus <- data.frame(matrix(unlist(articles), nrow=2000,byrow=T))
colnames(articles_bus) <- c("body","title","url")

# Gather Obituraries articles.
for (i in 1:length(os)) {
    uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", Queries[3,],
                  "&offset=", os[i], 
                  "&fields=", fields,
                  "&rank=", rank,
                  "&api-key=", api,
                  sep="")
    data <- readLines(uri, warn="F") # get them
    trnslt  <- fromJSON(data) # tokenize
    articles <- append(articles, unlist(trnslt$results))
}
#Transform list into Obituaries dataframe.
articles_obit <- data.frame(matrix(unlist(articles), nrow=2000,byrow=T))
colnames(articles_obit) <- c("body","title","url")

# Gather Sports articles.
for (i in 1:length(os)) {
    uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", Queries[4,],
                  "&offset=", os[i], 
                  "&fields=", fields,
                  "&rank=", rank,
                  "&api-key=", api,
                  sep="")
    data <- readLines(uri, warn="F") # get them
    trnslt  <- fromJSON(data) # tokenize
    articles <- append(articles, unlist(trnslt$results))
}
#Transform list into Sports dataframe.
articles_sprts <- data.frame(matrix(unlist(articles), nrow=2000,byrow=T))
colnames(articles_sprts) <- c("body","title","url")

# Gather World articles.
for (i in 1:length(os)) {
    uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", Queries[5,],
                  "&offset=", os[i], 
                  "&fields=", fields,
                  "&rank=", rank,
                  "&api-key=", api,
                  sep="")
    data <- readLines(uri, warn="F") # get them
    trnslt  <- fromJSON(data) # tokenize
    articles <- append(articles, unlist(trnslt$results))
}
#Transform list into World dataframe.
articles_wrld <- data.frame(matrix(unlist(articles), nrow=2000,byrow=T))
colnames(articles_wrld) <- c("body","title","url")
rm(articles)
#Create training sets of sections.
articles_art_test <- articles_arts[sample(1:nrow(articles_arts),1000),]
write.table(articles_art_test, "tab_del_arts_articles.txt", sep="\t")

articles_bus_test <- articles_bus[sample(1:nrow(articles_bus),1000),]
articles_obit_test <- articles_obit[sample(1:nrow(articles_obit),1000),]
articles_sprts_test <- articles_sprts[sample(1:nrow(articles_sprts),1000),]
articles_wrld_test <- articles_wrld[sample(1:nrow(articles_wrld),1000),]
