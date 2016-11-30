

TrainData <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/X_train.txt")
TrainDataLabels <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/y_train.txt")
TrainDataSubjects <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/subject_train.txt")
TrainDataBodyAccX <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/body_acc_x_train.txt")
TrainDataBodyAccY <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/body_acc_y_train.txt")
TrainDataBodyAccZ <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/body_acc_z_train.txt")
TrainDataGyroX <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/body_gyro_x_train.txt")
TrainDataGyroY  <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/body_gyro_y_train.txt")
TrainDataGyroZ <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/body_gyro_z_train.txt")
TrainDataTotalAccX <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/total_acc_x_train.txt")
TrainDataTotalAccY <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/total_acc_y_train.txt")
TrainDataTotalAccZ <- read.csv("C:/Users/vivek/Desktop/DataScience/GettindandCleaningData/UCIHARDataset/train/InertialSignals/total_acc_z_train.txt")

TrainDataSet <- as.data.frame(c("DataSubjects","DataActivityLabels","Data","DataBodyAccX","DataBodyAccY","DataBodyAccZ","DataGyroX","DataGyroY","DataGyroZ","DataTotalAccX","DataTotalAccY","DataTotalAccZ"))

TrainDataSet <- cbind(TrainDataSubjects,TrainDataLabels,TrainData,TrainDataBodyAccX,TrainDataBodyAccY,TrainDataBodyAccZ,TrainDataGyroX,TrainDataGyroY,TrainDataGyroZ,TrainDataTotalAccX,TrainDataTotalAccY,TrainDataTotalAccZ);
names(TrainDataSet) <- c("DataSubjects","DataActivityLabels","Data","DataBodyAccX","DataBodyAccY","DataBodyAccZ","DataGyroX","DataGyroY","DataGyroZ","DataTotalAccX","DataTotalAccY","DataTotalAccZ")



TestData <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/X_test.txt")
TestDataLabels <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/y_test.txt")
TestDataSubjects <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/subject_test.txt")
TestDataBodyAccX <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/body_acc_x_test.txt")
TestDataBodyAccY <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/body_acc_y_test.txt")
TestDataBodyAccZ <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/body_acc_z_test.txt")
TestDataGyroX <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/body_gyro_x_test.txt")
TestDataGyroY  <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/body_gyro_y_test.txt")
TestDataGyroZ <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/body_gyro_z_test.txt")
TestDataTotalAccX <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/total_acc_x_test.txt")
TestDataTotalAccY <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/total_acc_y_test.txt")
TestDataTotalAccZ <- read.csv("C:/Users/vivek/Desktop/Datascience/GettindandCleaningData/UCIHARDataset/test/InertialSignals/total_acc_z_test.txt")

TestDataSet <- as.data.frame(c("DataSubjects","DataActivityLabels","Data","DataBodyAccX","DataBodyAccY","DataBodyAccZ","DataGyroX","DataGyroY","DataGyroZ","DataTotalAccX","DataTotalAccY","DataTotalAccZ"))

TestDataSet <- cbind(TestDataSubjects,TestDataLabels,TestData,TestDataBodyAccX,TestDataBodyAccY,TestDataBodyAccZ,TestDataGyroX,TestDataGyroY,TestDataGyroZ,TestDataTotalAccX,TestDataTotalAccY,TestDataTotalAccZ)

names(TestDataSet) <- c("DataSubjects","DataActivityLabels","Data","DataBodyAccX","DataBodyAccY","DataBodyAccZ","DataGyroX","DataGyroY","DataGyroZ","DataTotalAccX","DataTotalAccY","DataTotalAccZ")

MergedDataSet <- rbind(TrainDataSet,TestDataSet)
######################################################################################################################################



DataU <- lapply(MergedDataSet[,3],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MeanDataULSSR <- sapply(DataULSSNR,mean)
MergedDataSet$sddata <- StandardDeviationDataULSSR
MergedDataSet$mdata <- MeanDataULSSR


DataU <- lapply(MergedDataSet[,4],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdbodyaccxdata <- StandardDeviationDataULSSR
MergedDataSet$mdbodyaccxdata <- MeanDataULSSR


DataU <- lapply(MergedDataSet[,5],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdbodyaccydata <- StandardDeviationDataULSSR
MergedDataSet$mdbodyaccydata <- MeanDataULSSR



DataU <- lapply(MergedDataSet[,6],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdbodyacczdata <- StandardDeviationDataULSSR
MergedDataSet$mdbodyacczdata <- MeanDataULSSR


DataU <- lapply(MergedDataSet[,7],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdgyroxdata <- StandardDeviationDataULSSR
MergedDataSet$mdgyroxdata <- MeanDataULSSR


DataU <- lapply(MergedDataSet[,8],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdgyroydata <- StandardDeviationDataULSSR
MergedDataSet$mdgyroydata <- MeanDataULSSR


DataU <- lapply(MergedDataSet[,9],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdgyrozdata <- StandardDeviationDataULSSR
MergedDataSet$mdgyrozdata <- MeanDataULSSR



DataU <- lapply(MergedDataSet[,10],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdtotalaccxdata <- StandardDeviationDataULSSR
MergedDataSet$mdtotalaccxdata <- MeanDataULSSR


DataU <- lapply(MergedDataSet[,11],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdtotalaccydata <- StandardDeviationDataULSSR
MergedDataSet$mdtotalaccydata <- MeanDataULSSR



DataU <- lapply(MergedDataSet[,12],as.character)
DataULSS <- list()
for (i in 1:10297)
{
  DataULSS[i] <- strsplit(as.character(DataU[i])," ")
}
DataULSSN <- list()
for (i in 1:10297)
{
  DataULSSN[[i]] <- as.numeric(unlist(DataULSS[[i]]))
}
DataULSSNR <- lapply(DataULSSN, function(x) x[!is.na(x)])
MeanDataULSSR <- sapply(DataULSSNR,mean)
StandardDeviationDataULSSR <- sapply(DataULSSNR,sd)
MergedDataSet$sdtotalacczdata <- StandardDeviationDataULSSR
MergedDataSet$mdtotalacczdata <- MeanDataULSSR


colnames(MergedDataSet) <- c("DataSubjects","DataActivityLabels","Data",
                        "DataBodyAccX","DataBodyAccY","DataBodyAccZ",
                        "DataGyroX","DataGyroY","DataGyroZ","DataTotalAccX",
                        "DataTotalAccY","DataTotalAccZ","meandata","standarddeviationdata",
                        "meanbodyaccx","standarddeviationbodyaccx","meanbodyaccy","standarddeviationbodyaccy",
                        "meanbodyaccz","standarddeviationbodyaccz","meangyrox","standarddeviationgyrox",
                        "meangyroy","standarddeviationgyroy","meangyroz","standarddeviationgyroz",
                        "meantotalaccx","standarddeviationtotalaccx","meantotalaccy","standarddeviationtotalaccy",
                        "meantotalaccz","standarddeviationtotalaccz")

MergedDataSet$DataActivityLabels <- as.character(MergedDataSet$DataActivityLabels)
MergedDataSet$DataActivityLabels[MergedDataSet$DataActivityLabels == 1] <- "Walking"
MergedDataSet$DataActivityLabels[MergedDataSet$DataActivityLabels == 2] <- "Walking Upstairs"
MergedDataSet$DataActivityLabels[MergedDataSet$DataActivityLabels == 3] <- "Walking Downstairs"
MergedDataSet$DataActivityLabels[MergedDataSet$DataActivityLabels == 4] <- "Sitting"
MergedDataSet$DataActivityLabels[MergedDataSet$DataActivityLabels == 5] <- "Standing"
MergedDataSet$DataActivityLabels[MergedDataSet$DataActivityLabels == 6] <- "Laying"

MergedDataSet[,1] <- as.factor(MergedDataSet[,1])
MergedDataSet[,2] <- as.factor(MergedDataSet[,2])

MergedDataSetMelted <- melt(MergedDataSet, id = c("DataActivityLabels","DataSubjects"), measure.vars = c("meandata","standarddeviationdata",
                                                                                                         "meanbodyaccx","standarddeviationbodyaccx","meanbodyaccy","standarddeviationbodyaccy",
                                                                                                         "meanbodyaccz","standarddeviationbodyaccz","meangyrox","standarddeviationgyrox",
                                                                                                         "meangyroy","standarddeviationgyroy","meangyroz","standarddeviationgyroz",
                                                                                                         "meantotalaccx","standarddeviationtotalaccx","meantotalaccy","standarddeviationtotalaccy",
                                                                                                         "meantotalaccz","standarddeviationtotalaccz"))

MergedDataSetCasted <- dcast(MergedDataSetMelted,DataActivityLabels + DataSubjects ~ variable, mean)

write.table(MergedDataSetCasted, file = "TidyData.txt", row.names = FALSE)









#######################################################################################################################################


