library('RTextTools')
library('e1071');
library('SparseM');
library('tm');
library('ggplot2');

#Combines the articles into one table.
#We need to do this because we are training the 
#machine to compare articles to eachother.
articles_All <- rbind(articles_Arts,articles_Business,
                      articles_Obituaries,articles_Sports,articles_World)


#Next we do this cute trick to sample two random and independent sections of the data.
#I also took sample t as a subset of the test data so I wouldn't have to wait so long
#for it to run.
s <- sample(10000, 5000)
t <- sample(5000, 100)
train <- articles_All[s,] 
test <- articles_All[-s,] 
littletest <- test[t,]

#Creates the training matrix with the listed specifications

#trainvector <- cbind(train["X1"],train["X2"])

#littletrainvector <- cbind(littletest["X1"],littletest["X2"])

#corpuslittle <- Corpus(VectorSource(littletrainvector))
#corpuslittle <- tm_map(corpuslittle,removePunctuation)
#corpuslittle <- tm_map(corpuslittle,s)
                                    
trainmatrix <- create_matrix(cbind(train["X1"],train["X2"]), language="english",
                             removeNumbers=TRUE, removePunctuation=TRUE, 
                             stemWords=TRUE, removeSparseTerms = .998,removeStopwords=TRUE)

#Creates the test matrix with the listed specifications

testmatrix <- create_matrix(cbind(test["X1"],test["X2"]), language="english",
                             removeNumbers=TRUE, removePunctuation=TRUE, removeStopwords=TRUE,
                             stemWords=TRUE, removeSparseTerms = .998)

#Creates a little test matrix

little_test_matrix <- create_matrix(cbind(littletest["X1"],littletest["X2"]), language="english",
                                    removeNumbers=TRUE, removePunctuation=TRUE,ngramLength=1,
                                    stemWords=TRUE, removeSparseTerms = .998, removeStopwords = TRUE)


#This trains the model using Naive Bayes and 5000 test rows.
model <- naiveBayes(as.matrix(trainmatrix),as.factor(train$newcol),laplace=3);


#This provides the probabilities for each section using test data.
#To run for full results instead of the small set, rename 
#little_test_matrix to testmatrix
#WARNING THIS TAKES A LONG TIME AND MAKES YOUR COMPUTER GET REALLY HOT

detailed_results <- predict(model,as.matrix(little_test_matrix),type="raw",threshold = .001);

#This provides only the most likely section for each row of your data
results <- predict(model,as.matrix(little_test_matrix));

#This creates a confusion table that looks at the actual data and compares
#it to the predicted class
comparison_table <- table(results,littletest$newcol)
View(comparison_table)

#Turns the table to various data frames
#this one is the raw table numbers
confusion <-as.data.frame(table(results,littletest$newcol))
#this one is normalized based on the number of projections in each category
confusion_normalized_projected <- as.data.frame(prop.table(table(results,littletest$newcol),1))
#this one is normalized based on the actual number in each category
confusion_normalized_actual <- as.data.frame(prop.table(table(results,littletest$newcol),2))
#plots the data frame, in this case for confusion_normalized_projected
#This plots the confusion table in a heatmap using ggplots.
#Names the rows and columns
colnames(input.matrix.normalized) = c("Arts", "Business", "Obituaries", "Sports", "World News")
rownames(input.matrix.normalized) = colnames(input.matrix.normalized)
plot <- ggplot(confusion)
plot + geom_tile(aes(x=Var2, y=results, fill=Freq)) + scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") +
  scale_fill_gradient(breaks=seq(from=-0, to=20, by=2)) + 
  labs(fill="Normalized\nFrequency")

#Determining the most important words in each type of article

article_word_use <- t(as.data.frame(model$tables))
article_word_use <- as.data.frame(article_word_use[odds
                                                   
art_sort <- article_word_use[order(-article_word_use$Arts),]
bus_sort <- article_word_use[order(-article_word_use$Business),]
obi_sort <- article_word_use[order(-article_word_use$Obituaries),]
spo_sort <- article_word_use[order(-article_word_use$Sports),]
wor_sort <- article_word_use[order(-article_word_use$World),]

#We can use this matrix to do other predictions with RTextTools. 
#If we want to do predictions withs stuff other than Naive Bayes.
#We'll have to do a little more coding on it so I left it commented for now.

#allmatrix <- create_matrix(cbind(articles_All["X1"],articles_All["X2"]), language="english",
#                            removeNumbers=TRUE, removePunctuation=TRUE, 
#                             stemWords=TRUE, removeSparseTerms = .998)
#trainmatrix <- create_matrix(cbind(articles_All["X1"],articles_All["X2"]), language="english",
#                             removeNumbers=TRUE, removePunctuation=TRUE, 
#                             stemWords=TRUE, removeSparseTerms = .998)

container <- create_container(little_test_matrix,
                              littletest$newcol, trainSize=1:100,
                              virgin=FALSE)