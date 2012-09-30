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
  assign(paste("articles_",facets[[j]],sep=""),data.frame(matrix(unlist(articles), nrow=2000,byrow=T))) 
  
  rm(articles)
  articles <- list()
}
#Add descriptive row to articles
articles_Arts$newcol <- apply(articles_Arts,1,function(row) "Arts")
articles_Business$newcol <- apply(articles_Business,1,function(row) "Business")
articles_Obituaries$newcol <- apply(articles_Obituaries,1,function(row) "Obituaries")
articles_Sports$newcol <- apply(articles_Sports,1,function(row) "Sports")
articles_World$newcol <- apply(articles_World,1,function(row) "World")
#Create training sets of sections.
articles_art_test <- articles_arts[sample(1:nrow(articles_arts),1000),]
write.table(articles_art_test, "tab_del_arts_articles.txt", sep="\t")

articles_bus_test <- articles_bus[sample(1:nrow(articles_bus),1000),]
articles_obit_test <- articles_obit[sample(1:nrow(articles_obit),1000),]
articles_sprts_test <- articles_sprts[sample(1:nrow(articles_sprts),1000),]
articles_wrld_test <- articles_wrld[sample(1:nrow(articles_wrld),1000),]
