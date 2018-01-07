library(dplyr)
library(readr)
library(GA)
library(rpart)
library(data.table)
library(data.tree)
library(foreach)
library(doParallel)

# Pretty decision tree output
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# evaluate accuracy of decision tree for rpart library
evaluate.accuracy <- function(fit, data)
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
  return(length(data$quality) - fails/2)
}

# checks classification of decision tree and puts
# class labels to leaves
leafs.prediction <- function(tree, data)
{
  # reseting every leaf
  for (leaf in tree$leaves)
  {
    leaf$cl <- c(0,0,0,0,0,0,0,0,0,0)
  }
  
  for (i in 1:length(data$quality))
  {
    el <- data[i,]
    node <- tree$root
    # every element from data set is classified by tree
    while (1)
    {
      if (el[node$var] < node$val)
      {
        if (is.null(node$children[["lNode"]]))
        {
          if (is.null(node$children[["lLeaf"]])) # if lleaf does not exist
          {
            node$AddChild("lLeaf", cl = c(0,0,0,0,0,0,0,0,0,0)) # create new leaf
          }
          node$children[["lLeaf"]]$cl[[el$quality]] <- node$children[["lLeaf"]]$cl[[el$quality]] + 1 # increment class counter
          break
        }
        else
        {
          node <- node$children[["lNode"]] # go to next node
        }
      }
      else
      {
        if (is.null(node$children[["rNode"]]))
        {
          if (is.null(node$children[["rLeaf"]])) # if rleaf does not exist
          {
            node$AddChild("rLeaf", cl = c(0,0,0,0,0,0,0,0,0,0)) # create new leaf
          }
          node$children[["rLeaf"]]$cl[[el$quality]] <- node$children[["rLeaf"]]$cl[[el$quality]] + 1
          break
        }
        else
        {
          node <- node$children[["rNode"]] # go to next node
        }
      }
    }
  }
  # attach classes to leaves on the basis of the biggest count of each class
  # calculates accuracy
  acc <- 0
  for (leaf in tree$leaves)
  {
    maxCount <- 0
    maxCl <- 0
    for (i in 1:10)
    {
      if (maxCount < leaf$cl[[i]])
      {
        maxCount = leaf$cl[[i]]
        maxCl <- i
        next
      }
    }
    leaf$cl <- maxCl
    acc <- acc + maxCl
  }
  return(c("tree" = tree, "accuracy" = acc))
}

# randomly modifies tree
modify.tree <- function(tree, data)
{
  Pcut <- 0.1 # probability of removing node
  Pmod <- 0.1 # probability of modifying node
  Pcre <- 1 - Pcut - Pmod # probability of creating node
  Pl <- 0.5 #probabilty of creating left node
  # find node to modify
  node <- tree$leaves[[as.integer(runif(1,1,length(tree$leaves) + 1))]]$parent
  p <- runif(1)
  if (p < Pcut)
  {
    # remove node
    if (!is.null(node$children[["lLeaf"]]))
    {
      node$RemoveChild("lLeaf")
    }
    if (!is.null(node$children[["rLeaf"]]))
    {
      node$RemoveChild("rLeaf")
    }
    if (node$isRoot)
    {
      var <- c(attributes(data)$names[as.integer(runif(1, min = 1, max = length(data)))])
      val <- c(runif(1, min = min(data[as.character(var)], na.rm=T), max = max(data[as.character(var)], na.rm=T)))
      node <- Node$new("Node", var = var, val = val)
    }
    else
    {
      node$parent$RemoveChild(node$name)
    }
  }
  else if (p < Pcut + Pmod)# beacuse Pcut< p < Pmod
  {
    # modify node
    var <- c(attributes(data)$names[as.integer(runif(1, min = 1, max = length(data)))])
    val <- c(runif(1, min = min(data[as.character(var)], na.rm=T), max = max(data[as.character(var)], na.rm=T)))
    node <- Node$new("lNode", var = var, val = val)
  }
  else
  {
    # create new node
    if (runif(1) < Pl)
    {
      # create left node
      if (!is.null(node$children[["lLeaf"]]))
      {
        node$RemoveChild("lLeaf")
      }
      var <- c(attributes(data)$names[as.integer(runif(1, min = 1, max = length(data)))])
      val <- c(runif(1, min = min(data[as.character(var)], na.rm=T), max = max(data[as.character(var)], na.rm=T)))
      node$AddChildNode(Node$new("lNode", var = var, val = val))
    }
    else
    {
      # create right node
      if (!is.null(node$children[["rLeaf"]]))
      {
        node$RemoveChild("rLeaf")
      }
      var <- c(attributes(data)$names[as.integer(runif(1, min = 1, max = length(data)))])
      val <- c(runif(1, min = min(data[as.character(var)], na.rm=T), max = max(data[as.character(var)], na.rm=T)))
      node$AddChildNode(Node$new("rNode", var = var, val = val))
    }
  }
  return(tree)
}

selector <- function(population, newpopulation, data)
{
  pop <- c(population, newpopulation)
  acc <- vector(mode = "integer", length = length(pop))
  ind <- 1:length(pop) # indexes trees in population
  df <- data.frame(ind, acc)
  # evaluates accuracy
  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)
  r <- foreach (tree = pop,.combine = c, .export = "leafs.prediction") %dopar%  
    leafs.prediction(tree, data)
  for (i in 1:length(pop))
  {
    df$acc[[i]] <- r[[2*i]] / length(data$quality)
    pop [[i]] <- r[[2*i-1]]
  }
  stopCluster(cl)
  # sort
  dfo <- df[order(-df$acc),]
  #create final population to return
  retPop <- population
  for (i in 1:length(population))
  {
    retPop[[i]] <- Clone(pop[[dfo$ind[[i]]]])
  }
  return(c("population" = retPop, "bestAcc" = dfo$acc[[1]]))
}

# generate decision tree using evolution algorithm
generate.with.evolution.algorithm <- function(data, desiredAccuracy)
{
  start.time <- Sys.time()
  
  N <- 5 # size of population
  M <- 10 # maximum number of iterations
  #generate random population
  Population <- c()
  for (i in 1:N)
  {
    var <- c(attributes(data)$names[as.integer(runif(1, min = 1, max = length(data)))])
    val <- c(runif(1, min = min(data[as.character(var)], na.rm=T), max = max(data[as.character(var)], na.rm=T)))
    Population <-c(Population, Node$new("Node", var = var, val = val))
    Population[[i]]  <- leafs.prediction(Population[[i]], data)$tree
  }
  # evolution algorithm loop
  tmp <- 0
  best <- 0
  bestAcc <- 100
  mist <- 0
  NewPopulation <- c()
  for (j in 1:M)
  {
    for (i in 1:N)
    {
      NewPopulation[[i]] <- Clone(Population[[i]])
      modify.tree(NewPopulation[[i]], data)
    }
    r <- selector(Population, NewPopulation, data)
    for (i in 1:N)
    {
      Population[[i]] <- Clone(r[[i]])
    }
    mist[j] <- r$bestAcc
    best <- 1
    plot(mist, xlab="Iteracja",ylab="Accuracy najlepszego osobnika",col="royalblue1",pch=16)
    if (desiredAccuracy < r$bestAcc)
    {
      print(j)
      break
    }
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  return(Population[[best]])
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
twhite <- generate.with.evolution.algorithm(whiteWineT, 100)
print(tred, "var", "val", "cl")
plot(tred)
print(twhite, "var", "val", "cl")
plot(twhite)

#evaluating trees generated with evolution algorithm with verification data
print(leafs.prediction(tred, redWineV)$accuracy / length(redWine$quality))
print(leafs.prediction(twhite, whiteWineV)$accuracy / length(redWine$quality))