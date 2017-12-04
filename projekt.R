library(dplyr)
library(readr)
library(GA)
library(rpart)

# Pretty decision tree output
library(rattle)
library(rpart.plot)
library(RColorBrewer)

evaluate.precision <- function(fit, data)
{
  pred <- as.data.frame(table(predict(fit, data, type = "class")))
  real <- as.data.frame(table(data$quality))
  fails = 0
  if (length(pred[,1]) > length(real[,1]))
  {
    j = 1
    for (i in 1:length(pred[,1]))
    {
      if (array(pred[i,1]) < array(real[j,1]))
      {
        fails = fails + array(pred[i,2])
        next
      }
      else if (array(pred[i,1]) == array(real[j,1]))
      {
        fails = fails + abs(array(pred[i,2]) - array(real[j,2]))
        if (j < length(real[,1]))
        {
          j = j + 1
        }
      }
      else if (array(pred[i,1]) > array(real[j,1]))
      {
        fails = fails + array(real[j,1])
        while (array(pred[i,1]) <= array(real[j,1]))
        {
          if (j < length(real[,1]))
          {
            j = j + 1
          }
        }
      }
    }
  }
  else 
  {
    j = 1
    for (i in 1:length(real[,1]))
    {
      if (array(real[i,1]) < array(pred[j,1]))
      {
        fails = fails + array(real[i,2])
        next
      }
      else if (array(real[i,1]) == array(pred[j,1]))
      {
        fails = fails + abs(array(real[i,2]) - array(pred[j,2]))
        if (j < length(pred[,1]))
        {
          j = j + 1
        }
      }
      else if (array(real[i,1]) > array(pred[j,1]))
      {
        fails = fails + array(pred[j,1])
        while (array(real[i,1]) <= array(pred[j,1]))
        {
          if (j < length(pred[,1]))
          {
            j = j + 1
          }
          else
          {
            break
          }
        }
      }
    }
  }
  return(fails/2)
}

redWine <- read_csv("winequality/winequality-red.csv")
whiteWine <- read_csv("winequality/winequality-white.csv")
#spliting redWine into training and verifing data sets
v <- split(redWine, 1:2)
redWineT <- v$`1`
redWineV <- v$`2`
#spliting whiteWine into training and verifing data sets
v <- split(whiteWine, 1:2)
whiteWineT <- v$`1`
whiteWineV <- v$`2`
#generating trees with rpart
redFit <- rpart(quality ~ chlorides+density+pH+sulphates+alcohol+
                 fixedAcidity+volatileAcidity+citricAcid+
                 residualSugar+freeSulfurDioxide,
             data = redWineT,
             method = "class")
whiteFit <- rpart(quality ~ chlorides+density+pH+sulphates+alcohol+
                  fixedAcidity+volatileAcidity+citricAcid+
                  residualSugar+freeSulfurDioxide,
                data = whiteWineT,
                method = "class")
printcp(redFit) # display the results
plotcp(redFit) # visualize cross-validation results
summary(redFit) # detailed summary of splits
# plot tree
plot(redFit, uniform=TRUE,
     main="Classification Tree for red wine quality")
text(redFit, use.n=TRUE, all=TRUE, cex=.8)

printcp(whiteFit) # display the results
plotcp(whiteFit) # visualize cross-validation results
summary(whiteFit) # detailed summary of splits
# plot tree
plot(whiteFit, uniform=TRUE,
     main="Classification Tree for white wine quality")
text(whiteFit, use.n=TRUE, all=TRUE, cex=.8)
#predicting for verifing data sets
print(table(predict(redFit, redWineV, type = "class")))
print(table(redWineV$quality))
print(table(predict(whiteFit, whiteWineV, type = "class")))
print(table(whiteWineV$quality))
#evaluating
print(evaluate.precision(redFit, redWineV))
print(evaluate.precision(whiteFit, whiteWineV))

# rattle package decision tree output
fancyRpartPlot(redFit)
fancyRpartPlot(whiteFit)


