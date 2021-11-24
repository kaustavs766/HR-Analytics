#Importing the dataset
emp_data = read.csv("D:/Edureka Assignments/338_cert_proj_datasets_v3.0.csv")
head(emp_data)

#Exploratory Data Analysis

#Splitting the data-set into Independent and Dependent Variables
x<-emp_data[, -which(names(emp_data) == "left")] 
y<-emp_data$left

#Creating the correlation matrix of attributes
result = cor(x[,sapply(x,is.numeric)],use="complete.obs",method="pearson")
round(result,2)

#Plotting the Correlation matrix with a heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = result, col = col, symm = TRUE)

#We can also create a correlogram
#Positive correlations are displayed in blue and negative 
#correlations in red color. Color intensity and the size of the 
#circle are proportional to the correlation coefficients. In the 
#right side of the correlogram, the legend color shows the 
#correlation coefficients and the corresponding colors
install.packages("corrplot")
library(corrplot)
corrplot(result, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Visualizing the characteristics of whole data and only people who left,using plots and histograms
install.packages("ggpplot2")
library(ggplot2)

#subsetting the data with only those who left the company
emp_data_left<-emp_data[emp_data$left=='1',]

#Plotting the barplot for number of projects  vs only people who left.
tbl <- with(emp_data_left, table(number_project,left))
barplot(tbl,ylab="Number of people who left",
        legend=TRUE)
#We find the maximum number of people who left the company hve worked in only 2 projects.

#Plotting the barplot for time spend in company  vs only people who left.
tbl <- with(emp_data_left, table(time_spend_company,left))
barplot(tbl,ylab="Number of people who left",
        legend=TRUE)

#We find the maximum number of people who left the company have 3 years of experience

#Plotting the barplot for work accident vs only people who left.
tbl <- with(emp_data_left, table(Work_accident,left))
barplot(tbl,ylab="Number of people who left",
        legend=TRUE)
#We find the maximum number of people who left the company do not have work related accidents.

#Plotting the barplot for promotion given in last 5 years  vs only people who left.
tbl <- with(emp_data_left, table(promotion_last_5years,left))
barplot(tbl,ylab="Number of people who left",
        legend=TRUE)
#We find all of the peope who left the company were denied promotions for the last 5 years.


#Plotting the barplot for department vs only people who left.
tbl <- with(emp_data_left, table(department,left))
barplot(tbl,ylab="Number of people who left",
        legend=TRUE)
#Maximum people are leaving the company from Sales department

#Plotting the barplot for salary vs only people who left.
tbl <- with(emp_data_left, table(salary,left))
barplot(tbl,ylab="Number of people who left",
        legend=TRUE)
#We find maximum  peope who left the company had salary in the low bracket.

#Plotting the boxplot for average monthly hours vs people who left or stayed in company
boxplot(emp_data$average_montly_hours~emp_data$left, data = emp_data, xlab = "People who stayed in or left the company",
        ylab = "Average Monthly Hours",main = "Average Monthly Hours Vs People who left or stayed in Company")

#We find the median value of average monthly hours is greater for people who left the company

#Plotting the boxplot for satisfaction level vs people who left or stayed in company
boxplot(emp_data$satisfaction_level~emp_data$left, data = emp_data, xlab = "People who stayed in or left the company",
        ylab = "Satisfaction Level",main = "Satisfaction Level Vs People who left or stayed in Company")

#We find the median value of satisfaction is lower for people who left the company

#Plotting the boxplot for last evaluation vs people who left or stayed in company
boxplot(emp_data$last_evaluation~emp_data$left, data = emp_data, xlab = "People who stayed in or left the company",
        ylab = "Last evaluation",main = "Last Evaluation Vs People who left or stayed in Company")

#We find the median value of last evalution score is higher for people who left the company.

#Evaluate the values of each attributes for both left and non-left employees
#Plotting the barplot for number of projects  vs only people who left and non-left.
tbl <- with(emp_data, table(number_project,left))
barplot(tbl,ylab="Number of people",
        legend=TRUE)
#We find that there is not much of an effect of the number of projects on people leaving or staying in company

#Plotting the barplot for time spend in company  vs only people who left.
tbl <- with(emp_data, table(time_spend_company,left))
barplot(tbl,ylab="Number of people",
        legend=TRUE)

#We find that there is not much of an effect of time spent in company on people leaving or staying in company


#Plotting the barplot for work accident vs only people who left.
tbl <- with(emp_data, table(Work_accident,left))
barplot(tbl,ylab="Number of people ",
        legend=TRUE)
#We find that work related accidents have a major role in deciding whether people leave or stay in company.
#People who are leaving the company have no work related accidents.

#Plotting the barplot for promotion given in last 5 years  vs only peole who left.
tbl <- with(emp_data, table(promotion_last_5years,left))
barplot(tbl,ylab="Number of people",
        legend=TRUE)
#We find that  promotion given in last 5 years have a major role in deciding whether people leave or stay in company.
#People who are leaving the company did not have promotion for the last 5 years.


#Plotting the barplot for department  vs only people who left.
tbl <- with(emp_data, table(department,left))
barplot(tbl,ylab="Number of people",
        legend=TRUE)
#No major effect.

#Plotting the barplot for salary vs only people who left.
tbl <- with(emp_data, table(salary,left))
barplot(tbl,ylab="Number of people",
        legend=TRUE)
#We find salary does have a major efffect in deciding whether people will leave the company or stay
#Maximum of the people who are leave the company are in low salary bracket.



#Final Conclusion
#We find the mamimum number of people who left the company have worked in only 2 projects.
#We find the maximum number of people who left the company have 3 years of experience.
#We find the maximum number of people who left the company do not have work related accidents.
#We find all of the peope who left the company were denied promotions for the last 5 years.
#Maximum people are leaving the company from Sales department.
#We find maximum  peope who left the company had salary in the low bracket.
#We find the median value of average monthly hours is greater for people who left the company
#We find the median value of satisfaction is lower for people who left the company.
#We find the median value of last evalution score is higher for people who left the company.
#We find that there is not much of an effect of the number of projects on people leaving or staying in company
#We find that there is not much of an effect of time spent in company on people leaving or staying in company.
#We find that work related accidents have a major role in deciding whether people leave or stay in company.
#People who are leaving the company have no work related accidents.
#We find that  promotion given in last 5 years have a major role in deciding whether people leave or stay in company.
#People who are leaving the company did not have promotion for the last 5 years.
#No major effect for department.
#We find salary does have a major efffect in deciding whether people will leave the company or stay
#Maximum of the people who are leave the company are in low salary bracket.


#Analyse the department wise turnouts and find out the percentage of employees leaving from each department.
#First we find the count of employees leaving the company from different departments and store this in freq
freq<-table(emp_data_left$department)
freq
#Then we find the percentage of employees leaving from each department
percentage_freq<-freq*100/sum(freq)
percentage_freq

#We find the maximum percentage is from the Sales department which shows out of the people who are leaving
#the company, maximum is from Sales department.



#Build models using Decision Tree, Random Forest, Naive Bayes and SVM techniques and find out the most accurate one.

#Data Preprocessing




# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(emp_data$left, SplitRatio = 0.8)
training_set = subset(emp_data, split == TRUE)
test_set = subset(emp_data, split == FALSE)

#First we relocate the dependent/target varibale to the last column for ease of calculation
library(dplyr)
left=training_set$left
newdata <- training_set %>% 
  select(left=left)
training_set<-training_set %>% relocate(left, .after = last_col())

#We do the same thing for test data
library(dplyr)
left=test_set$left
newdata <- test_set %>% 
  select(left=left)
test_set<-test_set %>% relocate(left, .after = last_col())

# Encoding the target feature as factor
training_set$left = factor(training_set$left, levels = c(0, 1))

#Encoding the remaining independent categorical variables
training_set$department <- factor(training_set$department, levels = c('accounting','hr','IT','management','marketing','product_mng','RandD','sales','support','technical'), labels = c(0,1,2,3,4,5,6,7,8,9))
training_set$department <- as.numeric(training_set$department)


training_set$salary <- factor(training_set$salary, levels = c('high','low','medium'), labels = c(0,1,2))
training_set$salary <- as.numeric(training_set$salary)


training_set$Work_accident <- factor(training_set$Work_accident,levels = c(0,1))
training_set$Work_accident <- as.numeric(training_set$Work_accident)


training_set$promotion_last_5years <- factor(training_set$promotion_last_5years,levels = c(0,1))
training_set$promotion_last_5years <- as.numeric(training_set$promotion_last_5years)


training_set$number_project <- factor(training_set$number_project,levels = c('2','3','4','5','6','7'), labels = c(0,1,2,3,4,5))
training_set$number_project <- as.numeric(training_set$number_project)


training_set$time_spend_company <- factor(training_set$time_spend_company,levels = c('2','3','4','5','6','7','8','10'), labels = c(0,1,2,3,4,5,6,7))
training_set$time_spend_company <- as.numeric(training_set$time_spend_company)

summary(training_set)



#Feature Scaling
#We leave out the target variable
training_set[-10]=scale(training_set[-10])


#Same preprocessing is done for test set
# Encoding the target feature as factor
test_set$left = factor(test_set$left, levels = c(0, 1))

#Encoding the remaining independent categorical variables
test_set$department <- factor(test_set$department, levels = c('accounting','hr','IT','management','marketing','product_mng','RandD','sales','support','technical'), labels = c(0,1,2,3,4,5,6,7,8,9))
test_set$department <- as.numeric(test_set$department)


test_set$salary <- factor(test_set$salary, levels = c('high','low','medium'), labels = c(0,1,2))
test_set$salary <- as.numeric(test_set$salary)


test_set$Work_accident <- factor(test_set$Work_accident,levels = c(0,1))
test_set$Work_accident <- as.numeric(test_set$Work_accident)


test_set$promotion_last_5years <- factor(test_set$promotion_last_5years,levels = c(0,1))
test_set$promotion_last_5years <- as.numeric(test_set$promotion_last_5years)


test_set$number_project <- factor(test_set$number_project,levels = c('2','3','4','5','6','7'), labels = c(0,1,2,3,4,5))
test_set$number_project <- as.numeric(test_set$number_project)


test_set$time_spend_company <- factor(test_set$time_spend_company,levels = c('2','3','4','5','6','7','8','10'), labels = c(0,1,2,3,4,5,6,7))
test_set$time_spend_company <- as.numeric(test_set$time_spend_company)
summary(test_set)



#Feature Scaling
#We leave out the target variable
test_set[-10]=scale(test_set[-10])

#First we apply SVM with linear kernel
install.packages("e1071")
library(e1071)
classifier = svm(formula = left ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-10])

# Making the Confusion Matrix
cm = table(test_set[, 10],y_pred)
cm

#Calculating the Accuracy
n = sum(cm)
n #total Records 
nc = nrow(cm)
nc #Total classes 
diag = diag(cm) #Correctly classified points 
rowsums = apply(cm,1,sum)
rowsums

colsums = apply(cm,2,sum)
colsums

p = rowsums/n

q = colsums/n

accuracy = sum(diag)/n
accuracy


precision = diag/colsums
precision
recall = diag/rowsums
recall
f1 = 2*precision*recall/(precision+recall)
f1
data.frame(precision,recall,f1)

#We apply SVM with radial kernel
classifier = svm(formula = left ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-10])

# Making the Confusion Matrix
cm = table(test_set[, 10], y_pred)
cm

#Calculating the Accuracy
n = sum(cm)
n #total Records 
nc = nrow(cm)
nc #Total classes 
diag = diag(cm) #Correctly classified points 
rowsums = apply(cm,1,sum)
rowsums

colsums = apply(cm,2,sum)
colsums

p = rowsums/n

q = colsums/n

accuracy = sum(diag)/n
accuracy


precision = diag/colsums
precision
recall = diag/rowsums
recall
f1 = 2*precision*recall/(precision+recall)
f1
data.frame(precision,recall,f1)
#We find Accuracy,precision,recall and f1 are all improved by using Kernel SVM 


#Now we use naive Bayes

classifier = naiveBayes(x = training_set[-10],
                        y = training_set$left)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-10])

# Making the Confusion Matrix
cm = table(test_set[, 10], y_pred)
cm

#Calculating the Accuracy
n = sum(cm)
n #total Records 
nc = nrow(cm)
nc #Total classes 
diag = diag(cm) #Correctly classified points 
rowsums = apply(cm,1,sum)
rowsums

colsums = apply(cm,2,sum)
colsums

p = rowsums/n

q = colsums/n

accuracy = sum(diag)/n
accuracy


precision = diag/colsums
precision
recall = diag/rowsums
recall
f1 = 2*precision*recall/(precision+recall)
f1
data.frame(precision,recall,f1)


#Random Forest Classifier
install.packages("randomForest")
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-10],
                          y = training_set$left,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-10])

# Making the Confusion Matrix
cm = table(test_set[, 10], y_pred)
cm

#Calculating the Accuracy
n = sum(cm)
n #total Records 
nc = nrow(cm)
nc #Total classes 
diag = diag(cm) #Correctly classified points 
rowsums = apply(cm,1,sum)
rowsums

colsums = apply(cm,2,sum)
colsums

p = rowsums/n

q = colsums/n

accuracy = sum(diag)/n
accuracy


precision = diag/colsums
precision
recall = diag/rowsums
recall
f1 = 2*precision*recall/(precision+recall)
f1
data.frame(precision,recall,f1)

#We have improved Accuracy, precision,recall and f1 score with Random Forest


#Decision Tree Classification
library(rpart)
classifier = rpart(formula = left ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-10], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 10], y_pred)
cm

# Plotting the tree
install.packages("rpart.plot")
library(rpart.plot)
par(mar = c(2, 2, 2, 2))
rpart.plot(classifier, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#Calculating the Accuracy
n = sum(cm)
n #total Records 
nc = nrow(cm)
nc #Total classes 
diag = diag(cm) #Correctly classified points 
rowsums = apply(cm,1,sum)
rowsums

colsums = apply(cm,2,sum)
colsums

p = rowsums/n

q = colsums/n

accuracy = sum(diag)/n
accuracy

precision = diag/colsums
precision
recall = diag/rowsums
recall
f1 = 2*precision*recall/(precision+recall)
f1
data.frame(precision,recall,f1)