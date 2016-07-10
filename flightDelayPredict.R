# Predicting whether flights will arrive >15 minutes after scheduled arrival time 

# Implementation is broken into 3 main sections:
# 1. Cleaning up data
# 2. Training algo
# 3. Improving performance

# Load data into dataframe
origData <- read.csv2('/Users/justindsouza/desktop/ML/flight_data.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
nrow(origData) # ~470,000 rows of data

airports <-c('ATL','LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')
origData <- subset(origData, DEST %in% airports & ORIGIN %in% airports)

nrow(origData) # now ~33,000 rows of data

#	----- 1. Cleaning up data ------
head(origData,5)
tail(origData,5) # column X has no value so we remove it
origData$X <- NULL # removes X column

head(origData,10)

# Possible correlations between ORIGIN_AIRPORT_SEQ_ID + ORIGIN_AIRPORT_ID, so use cor function to check
# " " " " " " " " " " " " " " " DEST_AIRPORT_SEQ_ID + DEST_AIRPORT_ID, " " " " " " " " " " " " " " " "
cor(origData[c("ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_AIRPORT_ID")]) # correlection = 1
cor(origData[c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")]) # correlation = 1

origData$ORIGIN_AIRPORT_SEQ_ID <- NULL # dropping column
origData$DEST_AIRPORT_SEQ_ID <- NULL # dropping column

mismatched <- origData[origData$CARRIER != origData$UNIQUE_CARRIER,] # check mismatches between CARRIER + UNIQUE_CARRIER
nrow(mismatched) # 0 mismatched, so remove UNIQUE_CARRIER
origData$UNIQUE_CARRIER <- NULL

onTimeData <- origData[!is.na(origData$ARR_DEL15) & origData$ARR_DEL15!="" & !is.na(origData$DEP_DEL15) & origData$DEP_DEL15!="",]
nrow(origData)
nrow(onTimeData)

onTimeData$DISTANCE <- as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED <- as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED <- as.integer(onTimeData$DIVERTED)

onTimeData$ARR_DEL15 <- as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15 <-as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID <- as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID <- as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK <- as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST <- as.factor(onTimeData$DEST)
onTimeData$ORIGIN <- as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK <- as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$CARRIER <- as.factor(onTimeData$CARRIER)

# Use tapply to check # arrival delayed vs. non-delayed flights + # departure delayed. vs non-delayed flights
tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15, length)
tapply(onTimeData$DEP_DEL15, onTimeData$DEP_DEL15, length)

(6460 / (25664 + 6460)) # ~20% flight delays  

#	----- 2. Training algo -----
install.packages('caret')
library(caret) # load caret

set.seed(122515) # set random # seed

# set columns to be used for algo training
featureCols <- c("ARR_DEL15", "DAY_OF_WEEK", "CARRIER", "DEST","ORIGIN","DEP_TIME_BLK")

onTimeDataFiltered <- onTimeData[,featureCols] # create filtered version of onTimeData dataframe
# create vector contain row indicies to put into the training data frames
inTrainRows <- createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.70, list=FALSE)

head(inTrainRows,10) # check row IDs

trainDataFiltered <- onTimeDataFiltered[inTrainRows,] # create training dataframe
testDataFiltered <- onTimeDataFiltered[-inTrainRows,] # create testing dataframe. 

# Check split 
nrow(trainDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered)) # 70%
nrow(testDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered)) # 30%

# Logistic regression (trained prediction model)
logisticRegModel <- train(ARR_DEL15 ~ ., data=trainDataFiltered, method="glm", family="binomial",
                          trControl=trainControl(method="cv", number=10, repeats=10))

# Predict using trained model against test data w/ Logistic Regression
logRegPrediction <- predict(logisticRegModel, testDataFiltered)

# Confusion matrix to get stats of prediction vs. actual
logRegConfMat <- confusionMatrix(logRegPrediction, testDataFiltered[,"ARR_DEL15"])
logRegConfMat

#	------ 3. Improving performance ------
# Utilize random forest algo (uses decision trees + bagging)
install.packages('randomForest')
library(randomForest) # load library into current session
rfModel <- randomForest(trainDataFiltered[-1], trainDataFiltered$ARR_DEL15, proximity = TRUE, importance = TRUE)
rfModel
rfValidation <- predict(rfModel, testDataFiltered)

# Confusion matrix to get stats of prediction vs. actual 
rfConfMat <- confusionMatrix(rfValidation, testDataFiltered[,"ARR_DEL15"])
rfConfMat




