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
                                                            levels = c(2,4),labels = c(1,2))
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

#changing the column name of target variable
colnames(training_set)[10]<-'class'
colnames(test_set)[10]<-'class'

# Applying LDA
library(MASS)
lda = lda(formula = class~ ., data = training_set)
training_set = as.data.frame(predict(lda, training_set))
training_set = training_set[c(4, 1)]
test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(4, 1)]

#Building the classifier and making predictions
library(class)
Dataset_test_pred <- knn(train = as.data.frame(training_set[,-2]),
                         test = as.data.frame(test_set[,-2]),
                         cl = training_set[,2,drop=TRUE], 
                         k =21)
cm = table(unlist(test_set[,2]),Dataset_test_pred)

#------------------To reduce the false negative and false positive values-----------------------
#------------Applying z scaling and making predictions
training_set_z <- as.data.frame(scale(training_set[-2]))
test_set_z <- as.data.frame(scale(test_set[-2]))

#Appending the class feature to training and test set
training_set_z$class = training_set$class
test_set_z$class = test_set$class

# Applying LDA
library(MASS)
lda = lda(formula = class~ ., data = training_set_z)
training_set_z = as.data.frame(predict(lda, training_set_z))
training_set_z = training_set_z[c(4, 1)]
test_set_z = as.data.frame(predict(lda, test_set_z))
test_set_z = test_set_z[c(4, 1)]

#Building the classifier and making predictions
library(class)
Dataset_test_pred <- knn(train = as.data.frame(training_set_z[,-2]),
                         test = as.data.frame(test_set_z[,-2]),
                         cl = training_set_z[,2,drop=TRUE], 
                         k =21)
cm_z = table(unlist(test_set_z[,2]),Dataset_test_pred)

#-------------------Visualising results-----------------------------------
k = as.vector(cm)
label <- c("True Positive","False Positive","False Negative","True Negative")
piepercent<- round(100*k/sum(k), 1)

pie(k, piepercent, main = "Confusion matrix pie chart after LDA", col = rainbow(length(k)),radius = 0.8)
legend("topleft",c("True Positive(%)","False Positive(%)","False Negative(%)","True Negative(%)"),fill = rainbow(length(k)),cex=0.6)


#---Pie chart of z-scaling results
k_z = as.vector(cm_z)
label <- c("True Positive","False Positive","False Negative","True Negative")
piepercent<- round(100*k_z/sum(k_z), 1)
pie(k_z, piepercent, main = "Confusion matrix pie chart after z-scaling along with LDA", col = rainbow(length(k_z)),radius = 0.8)
legend("topleft",c("True Positive(%)","False Positive(%)","False Negative(%)","True Negative(%)"),fill = rainbow(length(k_z)),cex=0.6)

