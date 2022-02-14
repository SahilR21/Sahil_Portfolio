#Logistic Regression
#Project no - 04 - whether patients have a diabetes or not
#load the liabraries
library(caTools)

#ingest the data
diabetes_data = read.csv("diabetes.csv")
print(head(diabetes_data))

#split the data into training and test dataset
split = sample.split(diabetes_data,SplitRatio = 0.8)
train_set = subset(diabetes_data, split == "TRUE")
test_set = subset(diabetes_data, split == "FALSE")

#mugg the data
# outcome is whole catogorical valeu so let's convert them into the factor
diabetes_data$Outcome = as.factor(diabetes_data$Outcome)

# Create model using training dataset
model = glm(Outcome ~ Pregnancies + Glucose + BloodPressure-SkinThickness+Insulin + BMI+ DiabetesPedigreeFunction +Age,train_set,family = "binomial" )
print(summary(model))

#Test the model usign testing data - all the probabilities that have diabities
#res = predict(model,test_set, type = "response")
#print(res)
res = predict(model,train_set,type = "response") 
print(res)

#confusion matrix - checking accuracy  
confmatrix_x = table(Actualvalue = train_set$Outcome,Predictedvalue = res>0.5)
print(confmatrix_x)
#accuracy
num = (333+126)/(333+46+126+92)
print(num)

#to find thershold value - using ROCR
#we can change the value of threshold to get accuracy so how we can choose threshold
library(ROCR)
ROCRPred = prediction(res,train_set$Outcome) 
ROCRPref = performance(ROCRPred,"tpr","fpr") #how is the performace of ROCRPred by plotting tpr, fpr
#save the image
png("Diabetes.png")
#plot 
plot(ROCRPref,colorize = TRUE,print.cutoffs.at = seq(0.1,by=0.1))
#Save it
dev.off()
#Now choose the threshold value from graph which is max tpr and min fpr
# the value is 0.3 = res > 0.3

