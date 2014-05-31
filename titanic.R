# titanic case

# 891 people
# survival (0=dead, 549 passengers dead, 61.6%)


runTestSet <- function()
{
        titanic_test <- loadData('/home/wijnand/R_workspace_titanic/test.csv')
        titanic_test$Survived <- NA
        
        titanic_train <- loadData()
        
        titanic <- rbind(titanic_train, titanic_test)
        titanic$Survived <- as.factor(titanic$Survived)
        
        #View(titanic)
        print(str(titanic))
        trainedModel <- decisionTree(titanic)
        
        titanic_pred <- predict(trainedModel, titanic_test, type="class", trials=10, na.action=na.exclude)
        
        tmp <- NULL
        tmp$PassengerId <- titanic_test$PassengerId
        tmp$Survived <- titanic_pred
        
        write.csv(tmp, '/home/wijnand/R_workspace_titanic/result_test.csv', row.names=FALSE, quote=FALSE)
}


runTrainData <- function(percentageTrain=0.7)
{
        # load the data into a data table
        titanic <- loadData()
        titanic$Survived <- as.factor(titanic$Survived)
        
        # order randomly
        set.seed(12345)
        titanic_rand <- titanic[order(runif(nrow(titanic)))]
        
        # row number of x% of the data for training set
        percentageTrain <- round(nrow(titanic) * percentageTrain, digits=0) - 1
        titanic_train <- titanic_rand[1:percentageTrain,]
        titanic_test <- titanic_rand[percentageTrain+1:nrow(titanic_rand),]
        
        # train a model
        trainedModel <- decisionTree(titanic_train)
        print(summary(trainedModel))
        
        # predict
        titanic_pred <- predict(trainedModel, titanic_test, type="class", trials=10, na.action=na.exclude)

        # show results
        printPredictionResults(titanic_pred, titanic_test)
}

printPredictionResults <- function(prediction, titanic_test)
{
        require(gmodels)
        table <- CrossTable(titanic_test$Survived, prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
                            prop.t = TRUE, dnn=(c('actual result', 'predicted result')))
        
        precision <- (table$prop.tbl[1,1] + table$prop.tbl[2,2]) * 100
        
        print(paste0("Precision as percentage: ", round(precision, digits=3)))
        
        
        
}

decisionTree <- function(trainData)
{
        require(C50)
        trainData <- C5.0(trainData[,!"Survived",with=FALSE], trainData$Survived, trials=10, na.action=na.exclude)
}


loadData <- function(location='/home/wijnand/R_workspace_titanic/train.csv')
{
        require(data.table)
        data <- read.csv(location)
        data <- as.data.table(data)
        
        levels(data$Cabin)[1] = "missing"
        levels(data$Embarked)[1] = "missing"

        data <- data[,Name:=NULL]
        data <- data[,Ticket:=NULL]
        #data <- data[,PassengerId:=NULL]
}