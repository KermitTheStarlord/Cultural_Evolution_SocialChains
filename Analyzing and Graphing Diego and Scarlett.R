## Graphing data from the Lexical Bruno-Scarlett Dataset

################################################################################
# 0. Library and constants #####################################################
################################################################################

#We're using the tidyverse package & ggplot
library(tidyverse)
library(ggplot2)
library(igraph)

# Setting constants
dataName <- "Data_DiegoScarlett.csv"

################################################################################
# 1. Getting and cleaning the data #############################################
################################################################################

# Importing the CSV file

# General getCurrentFileLocation function, by Juan Bernade on StackOverflow
# https://stackoverflow.com/questions/47044068/get-the-path-of-current-script

getCurrentFileLocation <-  function(){
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

# Reading the CSV File using the above function
DataPath <- paste(getCurrentFileLocation(),"/",dataName,sep="")
rawData <- read.csv(DataPath,header=TRUE,sep=",")

# Changing null to NA
rawData[rawData==""] <- NA

# Removing lines where no answer were given
cleanData <- rawData[complete.cases(rawData[,4:5]),]

################################################################################
# 2. Preliminary Analysis ######################################################
################################################################################

# Let's look at the occurences of unique spelling of Diego Maradona and
# Scarlett Johansson

getOccurencesTable <- function(dataFrame,columnToCheck){
  return(as.data.frame(
    sort(table(dataFrame[,columnToCheck]),decreasing=TRUE),
    stringsAsFactors=FALSE))
}

#diegoOccurenceTable <- getOccurencesTable(cleanData,"Diego")
#scarlettOccurencesTable <- getOccurencesTable(cleanData,"Scarlett")

# We then track every changes within sub-groups

listChanges <- function(dataFrame,columnToCheck,columnsToKeep=NA){
  # This function returns a dataframe that tracks whether the content of a
  # chosen column changes from one row to another. It also copies the columns
  # specified in the optional argument.
  # dataFrame is a dataFrame object.
  # columnToCheck is a string
  # columnToKepp is a list of string
  
  if (sum(is.na(columnsToKeep))){
    outputDf <- as.data.frame(dataFrame[,c(columnToCheck)])
    colnames(outputDf) <- columnToCheck
  } else {
  outputDf <- dataFrame[,c(columnToCheck,columnsToKeep)]
  }
  outputDf['Change'] <- rep(NA,length(dataFrame[,1]))
  
  previousAnswer = ""
  forLoopIndex = 0
  for (nextAnswer in outputDf[,columnToCheck]){
    forLoopIndex <- forLoopIndex + 1
    if (nextAnswer == previousAnswer){
      outputDf[forLoopIndex,"Change"] <- 0
    } else {
      outputDf[forLoopIndex,"Change"] <- 1
    }
    previousAnswer <- nextAnswer
  }
  
  return(outputDf)
} 

#diegoChanges <- listChanges(cleanData,"Diego",
#                        c("Group","Subgroup","Item","Gender","KnowDiego")
#                            )

#scarlettChanges <-listChanges(cleanData,"Scarlett",
#                        c("Group","Subgroup","Item","Gender","KnowScarlett")
#                            )

################################################################################
# 3. In-Depth Analysis #########################################################
################################################################################

# We create a function to get the stateMatrix of the different possible
# spelling

getStateMatrix <- function(dataFrame,columnToCheck,groupUnique=FALSE){
  # This function outputs a matrix
  # dataFrame is a dataFrame object, that does not contain the string "Other"
  # in the columnToCheck column.
  # columnToCheck is a string
  # groupUnique is a Boolean
  
  # !! IF REUSING, MODIFY THE LAST FOR LOOP ACCORDINGLY !! as an explicit
  # column name is used.
  
  # If groupUnique is True, then all of the unique occurences will be grouped
  # as "Other"
  if (groupUnique==TRUE){
    occurenceTable <- getOccurencesTable(dataFrame,columnToCheck)
    listOfIsolatedDf <- subset(occurenceTable,Freq==1)
    listOfIsolated <- listOfIsolatedDf[,1]
  } else {
    listOfIsolated = c("")
  }
  
  # We extract all of the possible states
  matrixName <- c()
  for(i in unique(dataFrame[,columnToCheck])){
    if (!(i %in% listOfIsolated)){
      matrixName <- append(matrixName,i)
    } else {
      matrixName <- append(matrixName,"Other")
    }
  }
  matrixName <- unique(matrixName)
  matrixSize = length(matrixName)
  
  # We build the Matrix
  stateMatrix <- matrix(data = 0, nrow=matrixSize, ncol=matrixSize,)
  colnames(stateMatrix) = rownames(stateMatrix) = matrixName
  
  for (index in 2:length(dataFrame[,columnToCheck])){
    if (dataFrame[index,"Item"] == 0){next}
    else {
      previousAnswer = dataFrame[index-1,columnToCheck]
      if (previousAnswer %in% listOfIsolated){previousAnswer <- "Other"}
      answer = dataFrame[index,columnToCheck]
      if (answer %in% listOfIsolated){answer <- "Other"}
      
      stateMatrix[previousAnswer,answer] <- stateMatrix[previousAnswer,answer] + 1
    }
  }
  
  return(stateMatrix)
}

# We use the igraph library to plot simple state-change between possible
# spellings.

stateMatrix <- getStateMatrix(cleanData,"Diego",groupUnique=TRUE)

plotNetwork <- function(stateMatrix){
  # This function inputs a stateMatrix and displays a graph using a network
  # object from the igraph library.
  numberNodes <- dim(stateMatrix)[1]
  networkMatrix <- matrix(
    rep.int(1,numberNodes*numberNodes),
    nrow=numberNodes,
    ncol=numberNodes)
  network <- graph_from_adjacency_matrix(networkMatrix)
  
  totalWeight <- sum(stateMatrix)
  widthVector <- c()
  for (i in stateMatrix){
    widthVector = append(widthVector,(i/totalWeight))
  }
  
  plot(network,
       vertex.color = "white",
       vertex.shape = "rectangle",
       vertex.size = 150,
       vertex.size2 = 30,
       
       vertex.label = colnames(stateMatrix),
       vertex.label.color = "black",
       vertex.label.font = 2, #Bold
       
       edge.color = "pink",
       edge.width = 30*widthVector,
       edge.arrow.size = 1,
       edge.arrow.width = 2,
       edge.curved = 0.5,
       edge.label = round(widthVector, digits = 3),
       
       layout = layout.circle,
       main = "circle"
       )
}

plotNetwork(stateMatrix)
################################################################################
# 4. Graphing Results ##########################################################
################################################################################

