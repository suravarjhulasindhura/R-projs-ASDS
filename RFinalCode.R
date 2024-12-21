warning = FALSE
#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(cowplot)
library(pROC)
library(caTools)
#install.packages("vtree")
library(vtree)
#install.packages("corrgram")
library(corrgram)
library(caret)

library(dplyr)
setwd("C:\\Users\\sindh\\Downloads")
# Load the dataset
heart_disease <- read.csv("framingham.csv")
str(heart_disease)
#Data Cleaning and Exploratory Data Analysis
summary(heart_disease)

#First, we can remove Duplicate Observations and Clean Null Observations.
heart_disease <- heart_disease %>% distinct()
colSums(is.na(heart_disease))
# Impute missing values
heart_disease <- heart_disease %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#heart_disease <- na.omit(heart_disease)
head(heart_disease)

#Convert binary variables to numeric for better visualization
heart_disease$currentSmoker <- as.numeric(as.character(heart_disease$currentSmoker))
heart_disease$prevalentHyp <- as.numeric(as.character(heart_disease$prevalentHyp))
heart_disease$diabetes <- as.numeric(as.character(heart_disease$diabetes))
heart_disease$TenYearCHD <- as.numeric(as.character(heart_disease$TenYearCHD ))

# Data Visualization
# In this plot, there are 3656 patients and 557 of them had heart disease in ten years. Among who had heart disease, 307 (55%) of them were male and 250 (45%) were female. More males than females have CHD
vtree(heart_disease, c("TenYearCHD", "Sex"), 
      fillcolor = c(TenYearCHD = "#e7d4e8", Sex = "#99d8c9"),
      horiz = TRUE)
# In this plot, among 557 patients had heart disease, just 35 (6%) patients have diabetes. More non-diabetics have CHD compared to diabetics.
vtree(heart_disease, c("TenYearCHD", "diabetes"), 
      fillcolor = c(TenYearCHD = "#e7d4e8", Sex = "#99d8c9"),
      horiz = TRUE)
vtree(heart_disease, c("TenYearCHD", "BPMeds"), 
      fillcolor = c(TenYearCHD = "#e7d4e8", Sex = "#99d8c9"),
      horiz = TRUE)
vtree(heart_disease, c("TenYearCHD", "currentSmoker"), 
      fillcolor = c(TenYearCHD = "#e7d4e8", Sex = "#99d8c9"),
      horiz = TRUE)
vtree(heart_disease, c("TenYearCHD", "prevalentStroke"), 
      fillcolor = c(TenYearCHD = "#e7d4e8", Sex = "#99d8c9"),
      horiz = TRUE)
vtree(heart_disease, c("TenYearCHD", "prevalentHyp"), 
      fillcolor = c(TenYearCHD = "#e7d4e8", Sex = "#99d8c9"),
      horiz = TRUE)

vtree(heart_disease, c("TenYearCHD", "Education"), 
      fillcolor = c(TenYearCHD = "#e7d4e8", Sex = "#99d8c9"),
      horiz = TRUE)
# Now, we can look into our variables distributions and effect of heart disease.
plot_1<- ggplot(heart_disease, aes(age)) + geom_density(fill = "blue") + labs(x="",title = "Age Distrubition") + theme_minimal()

plot_12 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = age, color = TenYearCHD, fill = TenYearCHD)) +
  geom_boxplot() + labs(x="",title = "Heart Diseas ~ Age")
plot_2 <- ggplot(heart_disease, aes(totChol)) + geom_density(fill = "blue") + labs(x="",title = "Cholesterol Distrubition") + theme_minimal()

plot_22 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = totChol, color = TenYearCHD)) +
  geom_boxplot() + labs(x="",title = "Heart Diseas ~ Total Chol") 

plot_grid(plot_1, plot_12, plot_2, plot_22)

# In this plot, many patients are 35-55 years old. Among them, who are above 55 years old 
#had more heart failure. Many patients have average 250 cholesterol. Total Cholesterol 
#has effect on heart disease.
plot_3 <- ggplot(heart_disease, aes(sysBP)) + geom_density(fill = "blue") + labs(x="",title = "sysBP Distrubition") + theme_minimal()
plot_33 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = sysBP, fill = TenYearCHD)) +
  geom_boxplot() + labs(x="",title = "Heart Disease ~ sysBP")
plot_4 <- ggplot(heart_disease, aes(diaBP)) + geom_density(fill = "blue") + labs(x="", title = "diaBP Distrubition") + theme_minimal()
plot_44 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = diaBP, color = TenYearCHD)) +
  geom_boxplot() + labs(x="", title = "Heart Disease ~ diaBP") 
plot_grid(plot_3, plot_33, plot_4, plot_44)

# In this plot, many patients have average 140 systolic blood pressure. People 
#with CHD have higher mean systolic blood pressures. People with CHD have higher 
#mean diastolic blood pressures.

plot_5 <- ggplot(heart_disease, aes(diaBP)) + geom_density(fill = "blue") + labs(x="", title = "BMI Distrubition") + theme_minimal()
plot_55 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = BMI, fill = TenYearCHD)) +
  geom_boxplot() +labs(x="", title = "Heart Disease ~ BMI")
plot_6 <- ggplot(heart_disease, aes(heartRate)) + geom_density(fill = "blue") + labs(x="", title = "Heart Rate Distrubition") + theme_minimal()
plot_66 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = heartRate, fill = TenYearCHD)) +
  geom_boxplot() +labs(x="", title = "Heart Disease ~  HeartRate")
plot_grid(plot_5, plot_55, plot_6, plot_66)

#People with CHD have a higher mean BMI. People with CHD have very similar mean 
#heart rates as people without CHD.
plot_7 <- ggplot(heart_disease, aes(glucose)) + geom_density(fill = "blue") + labs(x="", title = "Glucose Distrubition") + theme_minimal()
plot_77 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = glucose, fill = TenYearCHD)) +
  geom_boxplot() +labs(x="", title = "Heart Disease ~ Glucose")
plot_8 <- ggplot(heart_disease, aes(cigsPerDay)) + geom_density(fill = "blue") + labs(x="", title = "Cigarete PerDay Distrubition") + theme_minimal()
plot_88 <- ggplot(data = heart_disease, mapping = aes(x = as.factor(TenYearCHD), y = cigsPerDay, fill = TenYearCHD)) +
  geom_boxplot() +labs(x="", title = "Heart Disease ~ Cigarete PerDay") 
plot_grid(plot_7, plot_77, plot_8, plot_88)


# Check class imbalance
table(heart_disease$TenYearCHD)

# Heatmap for correlations
#install.packages("ggcorrplot")
library(ggcorrplot)
numeric_vars <- heart_disease %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars, use = "complete.obs")
ggcorrplot(cor_matrix, lab = TRUE)

# Load necessary library
library(dplyr)

# Handling outliers for sysBP
Q1_sysBP <- quantile(heart_disease$sysBP, 0.25, na.rm = TRUE)
Q3_sysBP <- quantile(heart_disease$sysBP, 0.75, na.rm = TRUE)
IQR_sysBP <- Q3_sysBP - Q1_sysBP
lower_sysBP <- Q1_sysBP - 1.5 * IQR_sysBP
upper_sysBP <- Q3_sysBP + 1.5 * IQR_sysBP
heart_disease <- heart_disease %>% filter(sysBP >= lower_sysBP & sysBP <= upper_sysBP)

# Handling outliers for cigsPerDay
Q1_cigs <- quantile(heart_disease$cigsPerDay, 0.25, na.rm = TRUE)
Q3_cigs <- quantile(heart_disease$cigsPerDay, 0.75, na.rm = TRUE)
IQR_cigs <- Q3_cigs - Q1_cigs
lower_cigs <- Q1_cigs - 1.5 * IQR_cigs
upper_cigs <- Q3_cigs + 1.5 * IQR_cigs
heart_disease <- heart_disease %>% filter(cigsPerDay >= lower_cigs & cigsPerDay <= upper_cigs)

# Handling outliers for glucose
Q1_glucose <- quantile(heart_disease$glucose, 0.25, na.rm = TRUE)
Q3_glucose <- quantile(heart_disease$glucose, 0.75, na.rm = TRUE)
IQR_glucose <- Q3_glucose - Q1_glucose
lower_glucose <- Q1_glucose - 1.5 * IQR_glucose
upper_glucose <- Q3_glucose + 1.5 * IQR_glucose
heart_disease <- heart_disease %>% filter(glucose >= lower_glucose & glucose <= upper_glucose)

# Handling outliers for BMI
Q1_BMI <- quantile(heart_disease$BMI, 0.25, na.rm = TRUE)
Q3_BMI <- quantile(heart_disease$BMI, 0.75, na.rm = TRUE)
IQR_BMI <- Q3_BMI - Q1_BMI
lower_BMI <- Q1_BMI - 1.5 * IQR_BMI
upper_BMI <- Q3_BMI + 1.5 * IQR_BMI
heart_disease <- heart_disease %>% filter(BMI >= lower_BMI & BMI <= upper_BMI)

# Handling outliers for heartRate
Q1_heartRate <- quantile(heart_disease$heartRate, 0.25, na.rm = TRUE)
Q3_heartRate <- quantile(heart_disease$heartRate, 0.75, na.rm = TRUE)
IQR_heartRate <- Q3_heartRate - Q1_heartRate
lower_heartRate <- Q1_heartRate - 1.5 * IQR_heartRate
upper_heartRate <- Q3_heartRate + 1.5 * IQR_heartRate
heart_disease <- heart_disease %>% filter(heartRate >= lower_heartRate & heartRate <= upper_heartRate)

# Handling outliers for totChol
Q1_totChol <- quantile(heart_disease$totChol, 0.25, na.rm = TRUE)
Q3_totChol <- quantile(heart_disease$totChol, 0.75, na.rm = TRUE)
IQR_totChol <- Q3_totChol - Q1_totChol
lower_totChol <- Q1_totChol - 1.5 * IQR_totChol
upper_totChol <- Q3_totChol + 1.5 * IQR_totChol
heart_disease <- heart_disease %>% filter(totChol >= lower_totChol & totChol <= upper_totChol)

# Handling outliers for age
Q1_age <- quantile(heart_disease$age, 0.25, na.rm = TRUE)
Q3_age <- quantile(heart_disease$age, 0.75, na.rm = TRUE)
IQR_age <- Q3_age - Q1_age
lower_age <- Q1_age - 1.5 * IQR_age
upper_age <- Q3_age + 1.5 * IQR_age
heart_disease <- heart_disease %>% filter(age >= lower_age & age <= upper_age)

# Visualize the results with boxplots
par(mfrow = c(3, 3)) # Set up plotting grid
boxplot(heart_disease$sysBP, main = "Boxplot of sysBP", col = "lightblue")
boxplot(heart_disease$cigsPerDay, main = "Boxplot of cigsPerDay", col = "lightblue")
boxplot(heart_disease$glucose, main = "Boxplot of glucose", col = "lightblue")
boxplot(heart_disease$BMI, main = "Boxplot of BMI", col = "lightblue")
boxplot(heart_disease$heartRate, main = "Boxplot of heartRate", col = "lightblue")
boxplot(heart_disease$totChol, main = "Boxplot of totChol", col = "lightblue")
boxplot(heart_disease$age, main = "Boxplot of age", col = "lightblue")

# Summary of the updated dataset
summary(heart_disease)

# Normalize numeric variables (optional for certain models)
heart_disease <- heart_disease %>%
  mutate(across(where(is.numeric), scale))
heart_disease


# Model Evaluation
library(caret)
predictTest <- ifelse(glm_probs > 0.5, 1, 0)
confusionMatrix(as.factor(predictTest), as.factor(test$TenYearCHD))

#Convert binary variables to numeric for better visualization train and test datas
train$currentSmoker <- as.numeric(as.character(train$currentSmoker))
train$prevalentHyp <- as.numeric(as.character(train$prevalentHyp))
train$diabetes <- as.numeric(as.character(train$diabetes))
train$TenYearCHD <- as.numeric(as.character(train$TenYearCHD))
test$currentSmoker <- as.numeric(as.character(test$currentSmoker))
test$prevalentHyp <- as.numeric(as.character(test$prevalentHyp))
test$diabetes <- as.numeric(as.character(test$diabetes))
test$TenYearCHD <- as.numeric(as.character(test$TenYearCHD))

# Generalized Linear Regression
# step -> Randomly split patients into training and testing sets
# step -> Logistic regression on training set to predict whether or not a patient 
#experienced CHD within 10 years of first examination
# step -> Evaluate predictive power on test set
# Randomly split the data into training and testing sets. We may put 70% of the data 
#in the training set. When you have more data like we do here, you can afford to put 
#less data in the training set and more in the testing set. This will increase our 
#confidence in the ability of the model to extend to new data since we have a larger 
#test set, and still give us enough data in the training set to create our model.

set.seed(1000)
split = sample.split(heart_disease$TenYearCHD, SplitRatio = 0.70)
train = subset(heart_disease, split==TRUE)
test = subset(heart_disease, split==FALSE)


# Stratified sampling
library(caret)
set.seed(1000)
train_index <- createDataPartition(heart_disease$TenYearCHD, p = 0.7, list = FALSE)
train <- heart_disease[train_index, ]
test <- heart_disease[-train_index, ]

# ROC and AUC for Logistic Regression
library(pROC)
glm_probs <- predict(new_glm_model, type = "response", newdata = test)
roc_curve <- roc(test$TenYearCHD, glm_probs)
plot(roc_curve, main = "ROC Curve - Logistic Regression")
auc(roc_curve)

#Now, we can make new generalized model as follows;
glm_model <- glm(TenYearCHD ~ ., data=train, family=binomial, na.action=na.omit)
round(summary(glm_model)$coefficients, 3)

# It looks like Sex, age, cigsPerDay, total cholesterol, systolic blood pressure, 
#and glucose are all significant in our model. The diaBP is almost significant .
# All of the significant variables have positive coefficients, meaning that higher 
#values in these variables contribute to a higher probability of 10-year coronary heart disease.
# Remove the insignificant variables and retrain the model.

new_glm_model <- glm(TenYearCHD ~ Sex + age + totChol + cigsPerDay + sysBP + glucose, data=train, family=binomial)

#Statistical Inference
summary(new_glm_model)

# Our model formula can be generated as follow;
# Heart Disease = -8.703 + 0.531 * male1 + 0.067 * age + 0.0029 * totChol + 0.019 * cigsPerDay + 0.017 * sysBP + 0.007 * glucose
# Confidence Intervals and P-values
# We can check our model`s confidence interval.

confint(new_glm_model)

#We can do ANOVA also
anova(new_glm_model, test="Chisq")

# Looking at the ANOVA, age, male, restecg, totChol, cigsPerDay, sysBP, and glucose 
#are significant factor for predicting heart disease. P- values are extremely close to zero.
# Prediction
# We'll call our predictions predictTest and use the predict function, which takes as 
#arguments the name of our model, new_glm_model, then type = "response", which gives 
#us probabilities, and lastly newdata = test, the name of our testing set.
# We'll use the table function and give as the first argument, the actual values, 
#test$TenYearCHD, and then as the second argument our predictions, predictTest > 0.5.
# A confusion matrix contains information about actual and predicted classifications 
#done by a classification system. Performance of such a system is commonly evaluated 
#using the data in the matrix. Predictions on the test set and Confusion matrix with threshold of 0.5

predictTest = predict(new_glm_model, type="response", newdata=test)
table(test$TenYearCHD, predictTest > 0.5)

# Install required library
library(car)

# Check VIF for the logistic regression model
vif(glm_model)

# Rebuild the model after removing variables with high VIF (e.g., >5)
reduced_glm_model <- glm(TenYearCHD ~ Sex + age + sysBP + glucose, data = train, family = binomial)

# Summary of the reduced model
summary(reduced_glm_model)

# Recompute VIF for the reduced model
vif(reduced_glm_model)

library(MASS)
library(caret)
library(pROC)

# Step 1: Identify multicollinearity using Variance Inflation Factor (VIF)
# Fit a logistic regression model to calculate VIF
vif_model <- glm(TenYearCHD ~ ., data = train, family = binomial)
library(car)
vif_values <- vif(vif_model)
print(vif_values)

# Step 2: Remove predictors with high VIF (e.g., VIF > 5)
# Retain only predictors with acceptable multicollinearity
reduced_train <- train[, !(names(train) %in% names(vif_values[vif_values > 5]))]
reduced_test <- test[, !(names(test) %in% names(vif_values[vif_values > 5]))]
# Step 3: Fit the LDA model on the reduced dataset
lda_model <- lda(TenYearCHD ~ ., data = reduced_train)

# Step 4: Summary of the LDA model
print(lda_model)

# Step 5: Plot LDA - if there are two discriminant axes
plot(lda_model)

# Step 6: Predict on the test set
lda_predictions <- predict(lda_model, newdata = reduced_test)

# Extract predicted class and posterior probabilities
lda_pred_classes <- lda_predictions$class
lda_pred_probs <- lda_predictions$posterior

# Step 7: Evaluate the model using a confusion matrix
lda_confusion <- confusionMatrix(lda_pred_classes, test$TenYearCHD)
print(lda_confusion)

# Step 8: Plot the ROC Curve and calculate AUC
lda_roc <- roc(test$TenYearCHD, lda_pred_probs[, 2])  # Assuming "1" is the positive class
plot(lda_roc, main = "ROC Curve - LDA")
auc_lda <- auc(lda_roc)
print(paste("AUC:", auc_lda))


# Results and Discussion¶
# The study cohort accumulated 3,656 patients-years of observation with 10 years. 
#The statistically independent predictive risk factors in our model are age, male, 
#restecg, totChol, cigsPerDay, sysBP, and glucose. With every extra cigarette one 
#smokes there is a 2% increase in the odds of CDH. Smoking and aging are more effective 
#on heart disease. For Total cholesterol level and glucose level there is no significant 
#change. There is a 1.7% increase in odds for every unit increase in systolic Blood Pressure.

# Conclusions
# We have an accuracy about 83.22% on our test set, which means that the model can 
#differentiate between low risk patients and high risk patients pretty well. Men seem 
#to be more susceptible to heart disease than women. Increase in age, number of cigarettes 
#smoked per day and systolic Blood Pressure also show increasing odds of having heart disease. 
#Total cholesterol shows no significant change in the odds of CHD. This could be due to the 
#presence of 'good cholesterol(HDL) in the total cholesterol reading. Glucose too causes a very 
#negligible change in odds (0.2%).
