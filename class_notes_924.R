require(rjson)
require(plyr)
library(RTextTools)

res1 <- fromJSON(file=theCall)
res1$results
str(res1$results)
class(res1$results)
res1$results[[1]]
res1 <- from JSON(file)
res1 <- ldply(res1$results, as.data.frame)


results <- vector("list", 3)
theCall <- "http://api.nytimes.com/svc/search/v1/article?format=json&query="
resultsArts <- vector("list",3)
for(i in 0:2)
{
  tempCall <- sprintf(theCall, "Arts", i)
  tempJson <- fromFSON(file=tempCall)
  resultsArts[[i+1]] <- ldply(tempJson$results, as.data.frame)
}
myFunc <-function(x)
{
  
}
resultDF <- ldply(results, myfunc)
resultDFArts <- "Arts"
view(resultDF)

doc_matrix <- create_matrix(articles_Arts$body, language="english", removeNumbers=TRUE, stemWords=TRUE, 
                            removeStopwords=TRUE, removeSparseTerms=.998)


doc_matrix <- create_matrix(articles_Arts[1:2000,1], language="english", removeNumbers=TRUE, stemWords=TRUE, removeStopwords=TRUE, removeSparseTerms=.998)

#multinomial regression
#stats dept chair prefers five seperate logistic regressions
