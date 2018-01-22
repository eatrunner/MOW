# testy do algorytmu ewolucyjnego według dokumentacji końcowej
# Autorzy: Barłomiej Kunikowki, Karol Niedzielewski
library(readr)
library(rpart)
library(data.table)
library(projektMOW)

# Pretty decision tree output
library(rattle)
library(rpart.plot)
library(RColorBrewer)

redWine <- read_csv("../data/winequality-red.csv")
whiteWine <- read_csv("../data/winequality-white.csv")
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
print(evaluate.accuracy(redFit, redWineV))
print(evaluate.accuracy(whiteFit, whiteWineV))

# rattle package decision tree output
fancyRpartPlot(redFit)
fancyRpartPlot(whiteFit)
#another way of ploting
rpart.plot(redFit)
rpart.plot(whiteFit)

# generating tree with evolution algorithm
tred <- generate.with.evolution.algorithm(redWineT, 100)
print(tred, "var", "val", "cl")
plot(tred)
twhite <- generate.with.evolution.algorithm(whiteWineT, 100)
print(twhite, "var", "val", "cl")
plot(twhite)

#evaluating trees generated with evolution algorithm with verification data
print(leafs.prediction(tred, redWineV)$accuracy)
print(leafs.prediction(twhite, whiteWineV)$accuracy)