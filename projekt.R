library(dplyr)
library(readr)
library(GA)
library(rpart)

load.data <- function(file_name) {
  data <- read_csv(file_name, col_names = TRUE,
                    col_types = list( col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_number(),
                                      col_integer()))
  return(data)
}
redWine <- load.data("winequality/winequality-red.csv")
whiteWine <- load.data("winequality/winequality-white.csv")
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
print(table(predict(whiteFit, whiteWineV, type = "class")))