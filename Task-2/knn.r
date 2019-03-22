library("readxl")
Dataset <- read_excel('Potential datasets for recruitment.xlsx',sheet = 2,na="?")

#See the summary and structure of dataset 
str(Dataset)
summary(Dataset)

#Excluding the Sample code number feature 
Dataset<-Dataset[-1]

#See the number of datapoints of each class 
table(Dataset$`Class: (2 for benign,  4 for malignant)`)

#Converting the datatype of BareNuclei to numeric 
Dataset$`Bare Nuclei` = as.numeric(Dataset$`Bare Nuclei`)

#Replacing the missing values with median 
median_BareNuclei <- median(Dataset$`Bare Nuclei`,na.rm = TRUE)
Dataset[is.na(Dataset$`Bare Nuclei`),'Bare Nuclei'] <- median_BareNuclei

#Encoding the Class feature as factor
Dataset$`Class: (2 for benign,  4 for malignant)` <- factor(Dataset$`Class: (2 for benign,  4 for malignant)`,
                                                            levels = c(2,4),labels = c("Benign","Malignant"))
# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(Dataset$`Class: (2 for benign,  4 for malignant)`, SplitRatio = 0.8)
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)

#Normalize function to normalize the dataset
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
training_set[-10] <- as.data.frame(lapply(training_set[-10],normalize))
test_set[-10] <- as.data.frame(lapply(test_set[-10], normalize))


#Building the classifier and making predictions
library(class)
Dataset_test_pred <- knn(train = training_set[,-10],
                         test = test_set[,-10],
                         cl = training_set[,10,drop=TRUE] , 
                         k =21)
cm = table(unlist(test_set[,10]),Dataset_test_pred)

#------------------To reduce the false negative and false positive values-----------------------
#------------Applying z scaling and making predictions
training_set_z <- as.data.frame(scale(training_set[-10]))
test_set_z <- as.data.frame(scale(test_set[-10]))

Dataset_test_pred <- knn(train = training_set_z,
                         test = test_set_z,
                         cl = training_set[,10,drop=TRUE] , 
                         k =21)
cm_z = table(unlist(test_set[,10]),Dataset_test_pred)

#-------------------Visualising results-----------------------------------
k = as.vector(cm)
label <- c("True Positive","False Positive","False Negative","True Negative")
piepercent<- round(100*k/sum(k), 1)

pie(k, piepercent, main = "Confusion matrix pie chart", col = rainbow(length(k)),radius = 0.8)
legend("topleft",c("True Positive(%)","False Positive(%)","False Negative(%)","True Negative(%)"),fill = rainbow(length(k)),cex=0.6)


#---Pie chart of z-scaling results
k_z = as.vector(cm_z)
label <- c("True Positive","False Positive","False Negative","True Negative")
piepercent<- round(100*k_z/sum(k_z), 1)
pie(k_z, piepercent, main = "Confusion matrix pie chart after z-scaling", col = rainbow(length(k_z)),radius = 0.8)
legend("topleft",c("True Positive(%)","False Positive(%)","False Negative(%)","True Negative(%)"),fill = rainbow(length(k_z)),cex=0.6)

