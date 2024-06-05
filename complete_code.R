setwd("H:/CS699-Data Mining/Final Project")
getwd()

library(dplyr)
library(caret)
library(modeest)

# Complete dataframe
df.complete <- read.csv("project_dataset_5K.csv")
dim(df.complete)

#Pre-processing the data--------------------------------------------------------

# Dataframe without ids and target column
df <- df.complete[10:(ncol(df.complete)-1)]

# Differentiating categorical and numeric columns-------------------------------
is_categorical <- function(column, threshold_unique_value = 10) {
  unique_values <- length(unique(column))
  
  return(unique_values < threshold_unique_value)
}

is_cat <- apply(df, 2, is_categorical)

# Differentiating categorical and numeric columns
df.cat <- df[is_cat == T] # 213 categorical
df.num <- df[is_cat == F] #  53 numeric

## Analyzing Categorical columns------------------------------------------------

# Removing columns in categorical dataframe with 80:5 unbalanced
df.cat1 <- df.cat[, -nearZeroVar(df.cat, freqCut = 80/5)]
#-------------------------------------------------------------------------------

# Selecting columns with less than or equal to 20% missing values
msvals <- (colSums(is.na(df.cat1))/nrow(df.cat1))*100
df.cat2 <- df.cat1[,msvals<=20]
#-------------------------------------------------------------------------------

# Filling the remaining missing columns with mode
fill_na_with_mode <- function(df) {
  for (col in names(df)) {
    mode_value <- as.numeric(mfv(df[[col]]))
    df[[col]][is.na(df[[col]])] <- mode_value
  }
  return(df)
}

df.cat3 <- fill_na_with_mode(df.cat2)
df.cat3$FLUSHOT7[is.na(df.cat3$FLUSHOT7)] <- 1 # mfv in FLUSHOT7 were NA
df.cat3$X_BMI5CAT[is.na(df.cat3$X_BMI5CAT)] <- 4 # mfv in X_BMI5CAT were NA

ncol(df.cat3) # 69 remaining columns
sum(is.na(df.cat3))

#-------------------------------------------------------------------------------
# Calculate the correlation matrix
cor_matrix <- cor(df.cat3)

# Set correlation threshold
threshold <- 0.7  # Adjust as needed

# Find columns with correlation above the threshold
correlated_cols <- findCorrelation(cor_matrix, cutoff = threshold)

# Remove correlated columns
df.cat.f <- df.cat3[, -correlated_cols]

# Print the resulting dataframe
ncol(df.cat.f) # 50 remaining columns

## Analyzing Numeric columns----------------------------------------------------

colnames(df.num)

# Removing columns in categorical dataframe with 80:5 unbalanced
df.num11 <- df.num[, -nearZeroVar(df.num, freqCut = 4000/400)]

# Selecting columns with less than or equal to 20% missing values
msvals.num <- (colSums(is.na(df.num11))/nrow(df.num11))*100
df.num1<-df.num11[,msvals.num<=20]

colnames(df.num1)
colSums(is.na(df.num1))

# Pre-processing SLEPTIM1 according to Codebook
x <- df.num1$SLEPTIM1[df.num1$SLEPTIM1 %in% 0:24]
hist(x)

df.num1$SLEPTIM1[df.num1$SLEPTIM1 %in% c(77,99)] <- mean(x)

# Pre-processing EMPLOY1 according to Codebook
x <- df.num1$EMPLOY1[df.num1$EMPLOY1 %in% 1:8]

df.num1$EMPLOY1[is.na(df.num1$EMPLOY1) | df.num1$EMPLOY1 == 9] <- mfv(x) 

# Pre-processing CHILDREN according to Codebook
df.num1$CHILDREN[df.num1$CHILDREN == 88] <- 0

x <- df.num1$CHILDREN[df.num1$CHILDREN %in% 0:87]
hist(x)
df.num1$CHILDREN[is.na(df.num1$CHILDREN)] <- mfv(x) 


# Pre-processing INCOME2 according to Codebook
x <- df.num1$INCOME2[df.num1$INCOME2 %in% 1:8]
hist(x)
df.num1$INCOME2[df.num1$INCOME2 %in% c(7,9)] <- mfv(x)
df.num1$INCOME2[is.na(df.num1$INCOME2)] <- mfv(x) 

# Pre-processing WEIGHT2 according to Codebook
ind <- (!is.na(df.num1$WEIGHT2) & df.num1$WEIGHT2 > 9000 & df.num1$WEIGHT2 < 9999)

df.num1$WEIGHT2[ind] <- 2.204*(df.num1$WEIGHT2[ind] - 9000)

df.num1$WEIGHT2[df.num1$WEIGHT2 %in% c(7777,9999)] <- median(df.num1$WEIGHT2[!df.num1$WEIGHT2 %in% c(7777,9999)], na.rm = T)

hist(df.num1$WEIGHT2)

median_W <- median(df.num1$WEIGHT2, na.rm = TRUE) # Calculate median of column 'A' excluding NAs
df.num1$WEIGHT2[is.na(df.num1$WEIGHT2)] <- median_W  # Replace NA values with median

# Pre-processing HEIGHT3 according to Codebook
ind.cm <- (!is.na(df.num1$HEIGHT3) & df.num1$HEIGHT3 > 9000 & df.num1$HEIGHT3 < 9999)

ind.inch <- (!is.na(df.num1$HEIGHT3) & df.num1$HEIGHT3 < 7000)

num_to_inch <- function(y){
  i <- y%%100
  f <- floor(y/100)
  return(12*f + i)
}

df.num1$HEIGHT3[ind.inch] <- num_to_inch(df.num1$HEIGHT3[ind.inch])

df.num1$HEIGHT3[ind.cm] <- (df.num1$HEIGHT3[ind.cm] - 9000)

df.num1$HEIGHT3[ind.cm] <- 0.393701*df.num1$HEIGHT3[ind.cm]

df.num1$HEIGHT3[df.num1$HEIGHT3 %in% c(7777,9999)] <- mean(df.num1$HEIGHT3[!df.num1$HEIGHT3 %in% c(7777,9999)], na.rm = T)

hist(df.num1$HEIGHT3)

mean_H <- mean(df.num1$HEIGHT3, na.rm = TRUE) # Calculate median of column 'A' excluding NAs
df.num1$HEIGHT3[is.na(df.num1$HEIGHT3)] <- mean_H  # Replace NA values with median

hist(df.num1$HEIGHT3)

# ALCDAY5 removed by dependency on X_DRNKWK1
# X_STRWT X_RAWRAKE X_WT2RAKE X_LLCPWT2  X_LLCPWT X_AGEG5YR,  removed as data not displayed and it is a id/date column
# HTIN4      HTM4     WTKG3 removed by dependency
# DROCDY3_ removed by dependency on X_DRNKWK1

hist(df.num$X_AGE80)

# Pre-processing X_DRNKWK1 according to Codebook (0: No drinks, 1 -98999,9990:NA)
# Strategy Fill the NA values with the median value as the distribution is skewed
hist(df.num1[['X_DRNKWK1']][df.num1[['X_DRNKWK1']]<99900],breaks=length(names(table(df.num1[['X_DRNKWK1']][df.num1[['X_DRNKWK1']]<99900]))))
# Calculate the median excluding NA and 99900 values

median_value <- median(df.num1$X_DRNKWK1[df.num1$X_DRNKWK1 <= 99900], na.rm = TRUE)

# Replace NA and 99900 values with the median
df.num1$X_DRNKWK1 <- ifelse(is.na(df.num1$X_DRNKWK1) | df.num1$X_DRNKWK1 == 99900, median_value, df.num1$X_DRNKWK1)

hist(df.num1$X_DRNKWK1)

# Pre-processing X_BMI5 according to Codebook (1-greater:BMI Values,999: Blank/NA)
# Strategy Fill the NA values with the median value as the distribution is skewed
# Calculate the median excluding NA 
hist(df.num1[['X_BMI5']])
median_value <- median(df.num1$X_BMI5, na.rm = TRUE)
# Replace NA values with the median
df.num1$X_BMI5 <- ifelse(is.na(df.num1$X_BMI5), median_value, df.num1$X_BMI5)

df.num2 <- df.num1[, names(df.num1) %in% c("SLEPTIM1", "EMPLOY1", "CHILDREN", "INCOME2", "WEIGHT2", "HEIGHT3", "X_AGE80", "X_BMI5", "X_DRNKWK1")]
#-------------------------------------------------------------------------------

# Calculate the correlation matrix
cor_matrix <- cor(df.num2)

# Set correlation threshold
threshold <- 0.7  # Adjust as needed

# Find columns with correlation above the threshold
correlated_cols <- findCorrelation(cor_matrix, cutoff = threshold)

# Remove correlated columns
df.num.f <- df.num2[, -correlated_cols]

# Print the resulting dataframe
ncol(df.num.f)

colnames(df.num.f)

#-------------------------------------------------------------------------------
# Combine dataframes for all features
combined_df <- cbind(df.num.f, df.cat.f)

ncol(combined_df)

# Calculate the correlation matrix
cor_matrix <- cor(combined_df)

# Set correlation threshold
threshold <- 0.7  # Adjust as needed

# Find columns with correlation above the threshold
correlated_cols <- findCorrelation(cor_matrix, cutoff = threshold)

# Remove correlated columns
combined_df.f <- combined_df[, -correlated_cols]

# Print the resulting dataframe
ncol(combined_df.f)

sum(is.na(df.complete$Class))

#Analyzing Combined dataframe---------------------------------------------------
# Outlier test

# Display columns with missing values
columns_with_missing <- colnames(combined_df.f)[colSums(is.na(combined_df.f)) > 0]

# View the columns with missing values
print(columns_with_missing)

df <- cbind(combined_df.f, df.complete$Class)

names(df)[names(df) == "df.complete$Class"] <- "Class"
df$Class <- ifelse(df$Class == 'Y', 1, 0)

# Function to detect and remove outliers using IQR method
remove_outliers <- function(df, cols, threshold = 1.5) {
  df_out <- df
  for (col in cols) {
    q1 <- quantile(df[[col]], 0.25)
    q3 <- quantile(df[[col]], 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - threshold * iqr
    upper_bound <- q3 + threshold * iqr
    df_out <- df_out[df_out[[col]] >= lower_bound & df_out[[col]] <= upper_bound, ]
  }
  return(df_out)
}

# Define columns for outlier detection
cols <- c("SLEPTIM1", "CHILDREN", "HEIGHT3", "X_AGE80", "X_BMI5", "X_DRNKWK1")

# Remove outliers using the IQR method
df <- remove_outliers(df, cols)

# Visualising final columns:
#c = 'col_name'
#hist(df[[c]], main = paste("Histogram of" , c), xlab = c)
#------------------------------------------------------------------------
# Scaling

# Function to apply min-max scaling
apply_min_max_scaling <- function(df, cols) {
  scaled_df <- df
  for (col in cols) {
    min_val <- min(df[[col]])
    max_val <- max(df[[col]])
    scaled_df[[col]] <- (df[[col]] - min_val) / (max_val - min_val)
  }
  return(scaled_df)
}

# Define columns for min-max scaling
cols <- c("SLEPTIM1", "CHILDREN", "HEIGHT3", "X_AGE80", "X_BMI5", "X_DRNKWK1")

# Apply min-max scaling
df <- apply_min_max_scaling(df, cols)

# Visualising final columns:
#c = 'col_name'
#hist(df[[c]], main = paste("Histogram of" , c), xlab = c)
#-------------------------------------------------------------------------------
# Final preprocessed dataframe:

# Print the cleaned dataframe's dimensions
dim(df)

# Select columns with less than 10 unique values and convert to factor
df <- df %>%
  mutate_if(~n_distinct(.) < 10, as.factor)

head(df)
# Printing dimensions of the preprocessed dataframe
print(dim(df))

write.csv(df, "preprocessed_data.csv", row.names=FALSE)


#Training different algorithms--------------------------------------------------
library(RWeka) 
library(e1071)
library(caret) 
library(rsample)
library(C50)
library(rpart)
library(rpart.plot)
library(pROC)
library(MASS)
library(modeest)

# Complete dataframe
df <- read.csv("preprocessed_data.csv")
#head(df)

# Printing dimensions of the preprocessed dataframe
#print(dim(df))

# Converting Class column to a factor column
df$Class<-as.factor(df$Class)

# Display class distribution
table(df$Class)

## Balancing dataset------------------------------------------------------------
balancing_data <- function(train, method){
  if (method == 'oversampling') {
    
    subset_data <- subset(train, Class==1)

    x <- c(1:nrow(subset_data))
    srswr <- sample(x, 2000, replace=T)
    sampled_dat <- subset_data[srswr,]
    
    oversampled_data <- rbind(subset(train, Class==0), sampled_dat)
    
    train <- oversampled_data
  } 
  
  else if (method == 'undersampling') {
    
    subset_data <- subset(train, Class==0)

    x <- c(1:nrow(subset_data))
    srswr <- sample(x, 450, replace=F)
    sampled_dat <- subset_data[srswr,]
    
    undersampled_data <- rbind(subset(train, Class==1), sampled_dat)
    
    train <- undersampled_data
  } 
  
  else if (method == 'mixsampling') {
    
    subset_data <- subset(train, Class==0)

    x <- c(1:nrow(subset_data))
    srswr <- sample(x, 600, replace=F)
    undersampled_data <- subset_data[srswr,]
    
    subset_data <- subset(train, Class==1)

    x <- c(1:nrow(subset_data))
    srswr <- sample(x, 150, replace=F) 
    oversampled_data <- subset_data[srswr,]
    
    mix_data <- rbind(undersampled_data,subset_data, oversampled_data)

    train <- mix_data
  } 
  
  return(train)
}

## Feature selection------------------------------------------------------------
library(FSelector)
library(Boruta)

feature_selection <- function(df, method){
  if (method == 'cfs') {
    subset <- cfs(Class ~., df)
    imp.columns <- subset
  } 
  
  else if (method == 'Boruta') {
    bone.marrow.boruta <- Boruta(Class~.,data=df)
    imp.columns <- names(bone.marrow.boruta$finalDecision[bone.marrow.boruta$finalDecision == 'Confirmed'])
  } 
  
  else if (method == 'IG') {
    info.gain <- information.gain(Class ~ ., df)
    info.gain <- cbind(rownames(info.gain), data.frame(info.gain, row.names=NULL))
    names(info.gain) <- c("Attribute", "Info Gain")
    sorted.info.gain <- info.gain[order(-info.gain$`Info Gain`), ]
    sorted.info.gain
    # top 10
    imp.columns <- sorted.info.gain[1:40, ]$Attribute
  } 
  
  return(imp.columns)
}

## Models-----------------------------------------------------------------------
library(rpart)
library(pROC)

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

train_control.simp <- trainControl(method = "repeatedcv", number = 5, 
                              summaryFunction = defaultSummary)

model_training <- function(train, key){
  # 1. CART: rpart:
  if (key == 'rpart') {
    set.seed(31)
    model <- train(Class ~ ., data = train, method = "rpart", trControl = train_control,
                   tuneLength = 20)
  } 
  
  # 2. Flexible Discriminant Analysis: 
  else if (key == 'fda') {
    set.seed(30)
    model <- train(Class ~ ., data = train, method = "fda", trControl = train_control)
  } 
  
  # 3. Bagged Flexible Discriminant Analysis:
  else if (key == 'bagFDA') {
    set.seed(30)
    model <- train(Class ~ ., data = train, method = "bagFDA", trControl = train_control.simp)
  } 
  
  # 4. Random Forest:
  else if (key == 'rf') {
    set.seed(31)
    rfGrid <-  expand.grid(mtry = seq(from = 1, by = 2, length.out = 20))
    
    
    model <- train(Class ~., data = train, method = "rf", trControl=train_control.simp,
                   tuneGrid = rfGrid)
  } 
  
  # 5. Conditional Inference Random Forest:
  else if (key == 'cforest') {
    set.seed(31)
    rfGrid <-  expand.grid(mtry = seq(from = 1, by = 2, length.out = 20))
    
    
    model <- train(Class ~., data = train, method = "cforest", trControl=train_control.simp,
                   tuneGrid = rfGrid)
  } 
  
  # 6. Adaboost: 
  else if (key == 'AdaBoost.M1') {
    set.seed(30)
    model <- train(Class ~., data = train, method = "AdaBoost.M1", trControl=train_control.simp)
  } 
  
  return(model)
}


calculate_measures <- function(tp, fp, tn, fn, roc.area){
  
  tpr = round(tp / (tp + fn), digits = 4) # TPR
  fpr = round(fp / (fp + tn), digits = 4) # FPR
  tnr = round(tn / (fp + tn), digits = 4) # TNR
  fnr = round(fn / (fn + tp), digits = 4) # FNR
  precision = round(tp / (tp + fp), digits = 4) # Precision
  recall = round(tpr, digits = 4) # Recall
  f_measure <- round((2 * precision * recall) / (precision + recall), digits = 4) #F-measure
  mcc <- round((tp*tn - fp*fn)/(sqrt(tp+fp)*sqrt(tp+fn)*sqrt(tn+fp)*sqrt(tn+fn)), digits = 4) # MCC
  total = (tp + fn + fp + tn)
  p_o = (tp + tn) / total
  p_e1 = ((tp + fn) / total) * ((tp + fp) / total)
  p_e2 = ((fp + tn) / total) * ((fn + tn) / total)
  p_e = p_e1 + p_e2
  k = round((p_o - p_e) / (1 - p_e), digits = 4) #Kappa
  
  values <- c(tpr, fpr, tnr, fnr, precision, recall, f_measure, roc.area, mcc, k)

  return (values)
}

result_table <- function(pred, actual){
  pred <- ordered(pred)
  actual <- ordered(test$Class)
  
  roc_obj <- roc(actual, pred)
  roc.area <- auc(roc_obj)
  
  roc.area <- round(roc.area, digit = 4)
  
  cm <- confusionMatrix(pred, actual)
  print(cm)
  cm <- cm$table
  
  # CLASS 0
  tp = cm[1,1]
  fp = cm[1,2]
  tn = cm[2,2]
  fn = cm[2,1]
  
  performance_measures0 <- calculate_measures(tp, fp, tn, fn, roc.area)
  performance_measures0
  
  # CLASS 1
  tp = cm[2,2]
  fp = cm[2,1]
  tn = cm[1,1]
  fn = cm[1,2]
  
  performance_measures1 <- calculate_measures(tp, fp, tn, fn, roc.area)
  performance_measures1
  
  performance_measures_wt.vg <- (table(test$Class)[1]*performance_measures0 + table(test$Class)[2]*performance_measures1)/nrow(test)
  performance_measures_wt.vg <- round(performance_measures_wt.vg, digit = 4)
  
  matrix_data <- matrix(c(performance_measures0, performance_measures1, performance_measures_wt.vg), nrow = 3, byrow = TRUE)
  
  # Column and row names
  col_names <- c('TPR', 'FPR', 'TNR', 'FNR', 'Precision', 'Recall', 'F-measure', 'ROC', 'MCC', 'Kappa')
  row_names <- c("Class 0", "Class 1", "Wt. Average")
  
  # Create dataframe with column and row names
  results <- as.data.frame(matrix_data)
  colnames(results) <- col_names
  rownames(results) <- row_names
  
  # Print the dataframe
  return(results)
}

## Experiments------------------------------------------------------------------

# Splitting data into training and testing
set.seed(32)

split <- initial_split(df, prop = 0.667, strata = Class)

train <- training(split)
# write.csv(train, "initial_train.csv", row.names = F)

test <- testing(split)
# write.csv(test, "initial_test.csv", row.names = F)

# Defining all step keys
balancings <- c('undersampling', 'oversampling')
feature_selections <- c('cfs', 'Boruta', 'IG')
keys <- c('rpart', 'fda', 'bagFDA', 'rf', 'cforest', 'AdaBoost.M1')

# Loop for the 36 experiments
for(balance in balancings){
  train_balanced <- balancing_data(train, balance)
  
  print(table(train_balanced$Class))
  
  print(dim(train_balanced))

  for(feature in feature_selections){
    # Feature selection
    
    imp.columns <- feature_selection(train_balanced, feature)
    
    train_fs <- cbind(train_balanced[,imp.columns], 'Class' = train_balanced$Class)
    
    print(dim(train_fs))
    
    for (k in keys) {
      train_fs$Class <- as.factor(train_fs$Class)
      
      model <- model_training(train_fs, k)
      pred <- predict(model, test)
      actual <- test$Class
      
      result.table <- result_table(pred, actual)
      
      label = paste(balance, '+', feature, '+', k, '_stats.csv', sep = '')
      #write.csv(result.table, file = label)
    }
  }
}

#Best Model---------------------------------------------------------------------
# Complete preprocessed dataframe
df <- read.csv("preprocessed_data.csv")
# Printing dimensions of the preprocessed dataframe
print(dim(df))

# Converting Class column to a factor column
df$Class<-as.factor(df$Class)

# Display class distribution
table(df$Class)

# Splitting data into training and testing
set.seed(32)

split <- initial_split(df, prop = 0.667, strata = Class)

train <- training(split)
write.csv(train, "initial_train.csv", row.names = F)

# Display class distribution
table(train$Class)

test <- testing(split)
write.csv(test, "initial_test.csv", row.names = F)

train_balance <- balancing_data(train, 'undersampling')

# Display class distribution
table(train_balance$Class)

print(dim(train_balance))

imp.columns <- feature_selection(train_balance, 'IG')

train_fs <- cbind(train_balance[,imp.columns], 'Class' = train_balance$Class)

print(dim(train_fs))

train_fs$Class <- as.factor(train_fs$Class)

set.seed(32)
model <- train(Class ~ ., data = train_fs, method = "fda", trControl = train_control)
pred <- predict(model, test)
actual <- test$Class

cm <- confusionMatrix(pred, actual)

cm
