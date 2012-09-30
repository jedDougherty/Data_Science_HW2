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

trainmatrix <- create_matrix(cbind(train["X1"],train["X2"]), language="english",
                             removeNumbers=TRUE, removePunctuation=TRUE, 
                             stemWords=TRUE, removeSparseTerms = .998)

#Creates the test matrix with the listed specifications

testmatrix <- create_matrix(cbind(test["X1"],test["X2"]), language="english",
                             removeNumbers=TRUE, removePunctuation=TRUE, 
                             stemWords=TRUE, removeSparseTerms = .998)

#Creates a little test matrix

little_test_matrix <- create_matrix(cbind(littletest["X1"],littletest["X2"]), language="english",
                                    removeNumbers=TRUE, removePunctuation=TRUE, 
                                    stemWords=TRUE, removeSparseTerms = .998)

#This trains the model using Naive Bayes and 5000 test rows.
model <- naiveBayes(as.matrix(trainmatrix),as.factor(train$newcol));


#This provides the probabilities for each section using test data.
#To run for full results instead of the small set, rename 
#little_test_matrix to testmatrix
#WARNING THIS TAKES A LONG TIME AND MAKES YOUR COMPUTER GET REALLY HOT

detailed_results <- predict(model,as.matrix(little_test_matrix),type="raw");

#This provides only the most likely section for each row of your data
results <- predict(model,as.matrix(little_test_matrix),type="raw");

#This creates a confusion table that looks at the actual data and compares
#it to the predicted class
comparison_table <- table(results,littletest$newcol)
View(comparison_table)


#This plots the confusion table in a heatmap using ggplots.
#Names the rows and columns
colnames(input.matrix.normalized) = c("Arts", "Business", "Obituaries", "Sports", "World News")
rownames(input.matrix.normalized) = colnames(input.matrix.normalized)

#Turns the table to a data frame
confusion <- as.data.frame(prop.table(table(results,littletest$newcol),1))

#plots the data frame
plot <- ggplot(confusion)
plot + geom_tile(aes(x=Var2, y=results, fill=Freq)) + scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") +
  scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + 
  labs(fill="Normalized\nFrequency")


#We can use this matrix to do other predictions with RTextTools. 
#If we want to do predictions withs stuff other than Naive Bayes.
#We'll have to do a little more coding on it so I left it commented for now.

#allmatrix <- create_matrix(cbind(articles_All["X1"],articles_All["X2"]), language="english",
#                            removeNumbers=TRUE, removePunctuation=TRUE, 
#                             stemWords=TRUE, removeSparseTerms = .998)
#trainmatrix <- create_matrix(cbind(articles_All["X1"],articles_All["X2"]), language="english",
#                             removeNumbers=TRUE, removePunctuation=TRUE, 
#                             stemWords=TRUE, removeSparseTerms = .998)

#container <- create_container(doc_matrix,
#                              USCongress$major, trainSize=s,
#                              testSize=-s, virgin=FALSE)