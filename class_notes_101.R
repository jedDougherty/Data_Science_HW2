do.double <- function(x, y=3)
{
    return(x*y)
}

mult.args <- function(x, y)
{
  do.double(x,y,4)
}

system.time({result <- do.double(4,6)})
mult.args(3,5)

require(ggplot2)
data(diamonds)
head(diamonds)

mod1 <- lm(price ~ carat + cut, data=diamonds)



#Learning about factors
class(diamonds$cut)
myChar <- c("a","b","c","d","e","a","a")
model.matrix(~myChar)
myDF <- data.frame(A=1:7, letter=myChar)
myDF
model.matrix(~ A +letter-1, data=myDF)

#on to regression
mod1 <- lm(price ~ carat + cut, data=diamonds)
summary(mod1)
mod1$coefficients
theCoef <- coef(mod1)
class(theCoef)
coefDF
coefDF <- data.frame(variable=names(theCoef), value=theCoef)

ggplot(coefDF, aes(x=value, y=variable)) +geom_point()


#model to plot functions
dotplot <- function(model, linecolor = "black", dotcolor = "purple",linetype=2)
{
  #grab the coefficients
  theCoef <- coef(model)
  #turn itno a data.frame
  coefDF <- data.frame(variable=names(theCoef), value=theCoef)
  #plot
  ggplot(coefDF, aes(x=value, y=variable)) +geom_point(color=dotcolor) + 
    geom_vline(xintercept=0, color=linecolor, linetype=linetype)
}

g <- dotplot(mod1, linecolor = "red", dotcolor = "purple")
class(g)
g
str(g)

ggplot(diamonds, aes(x=carat,y=price)) +geom_point()
head(mod1$fitted.values)
diamonds$fitted <- mod1$fitted.values
ggplot(diamonds, aes(x=carat,y=price)) +geom_point()+geom_line(aes(y=fitted),color="blue")


#scope! whats with that?
#The issue is if n
x<-4
newFunc <- function(arg)
{
  arg*x
}
newFunc(7, 9)
newFunc(7)
rm(x)


#wipes environment
rm(list=ls())
#clear plot window
dev.off()

naive <- function(data, alpha=1, beta=1, class="Section")
{
  result <- ddply(data, .variables=class, .fun=naive.worker, alpha=alpha, beta=beta)
}

naive.worker <- function(data, alpha=1, beta=1, class)
{
  data <- data[,which(names(data) == class)]
  njc <-  colSums(data)
  nc <- nrow(data)
}