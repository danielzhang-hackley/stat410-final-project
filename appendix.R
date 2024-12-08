######## START OF `preprocess.R` ########

# drop_cols are the columns we should remove numerical are the numerical columns,
# and should be the argument to `convert_categorical`
drop_cols <- c(
  "Model", "Location", "Color", "Owner", "Seller.Type", "Max.Power", "Max.Torque", "Drivetrain"
)
numerical <- c("price", "year", "odometer")

# possible interactions include region x state, posting date - year, manufacturer x model
# possible transformations include image or no image, clustering for description


get_num_null <- function(df) {
  #' Finds the number of null/empty values in each column of data.frame df. Each
  #' column of the returned result is the fraction for the associated column in
  #' df.
  
  result <- data.frame(matrix(nrow = 1, ncol = ncol(df)))
  colnames(result) <- colnames(df)
  
  for (column in colnames(df)) {
    result[1, column] <- sum(is.na(df[, column]) | df[, column] == "")
  }
  
  return (result)
}

get_frac_null <- function(df) {
  #' Finds the fraction of null/empty values in each column of data.frame df.
  #' Each column of the returned result is the fraction for the associated column
  #' in df.
  
  result <- get_num_null(df)
  result[1, ] <- result[1, ] / nrow(df)
  
  return (result)
}

remove_cols <- function(df, to_remove) {
  #' Return df without the columns specified in to_remove
  
  return (df[, !(colnames(df) %in% to_remove)])
}

remove_null <- function(df) {
  #' Return df without rows that have null/empty values
  
  idx <- !(is.na(df[, 1]) | (df[, 1] == ""))
  for (column in 2:ncol(df)) {
    idx <- idx & !(is.na(df[, column]) | (df[, column] == ""))
  }
  
  return (df[idx, ])
}

convert_categorical <- function(df, categorical) {
  #' Convert the categorical variables into factors; exclude the numerical
  #' values specified in the "factorization." Returns a list with "dummy" being
  #' the new design matrix, and "reference" being the reference levels for each
  #' categorical value.
  
  copy <- df
  ref <- list()
  
  for (column in colnames(copy)) {
    if (column %in% categorical){
      copy[, column] <- factor(copy[, column])
      ref[column] <- levels(copy[, column])[1]
    }
  }
  
  final <- model.matrix(~., data = copy)
  final <- remove_cols(final, c("(Intercept)"))
  
  return (list("dummy" = final, "factor" = copy, "reference" = ref))
}

get_num_unique <- function(df) {
  #' Finds the number of unique values in each column of data.frame df. Each
  #' column of the returned result is the fraction for the associated column in
  #' df.
  
  result <- data.frame(matrix(nrow = 1, ncol = ncol(df)))
  colnames(result) <- colnames(df)
  
  for (column in colnames(df)) {
    result[, column] <- length(unique(df[, column]))
  }
  
  return (result)
}

remove_outliers <- function(df, cols){
  copy <- df
  for (col in cols) {
    Q1 <- quantile(df[,col], 0.25, na.rm=TRUE)
    Q3 <- quantile(df[,col], 0.75, na.rm=TRUE)
    IQR <- Q3-Q1 
    lower_bound <- Q1-1.5*IQR
    upper_bound <- Q3+1.5*IQR
    copy <- copy[copy[,col] >= lower_bound & copy[,col] <= upper_bound,]
  }
  return (copy)
}

######## END OF `preprocess.R` ########



######## START OF `clean_data.R` ########
# source("preprocess.R") # uncomment this if you have separated the code into multiple files

# Load data
data <- read.csv("car details v4.csv")

df <- data
df <- remove_null(df)
df <- df[!duplicated(df), ]

# Remove the " cc" suffix from Engine
df$Engine <- as.numeric(gsub("[^0-9]", "", df$Engine)) # Convert to numbers, ex. "123 cc" -> 123

# Split max torque and the RPM at which max torque occurs
df <- tidyr::separate(df, Max.Torque, into = c("Max.Torque.Value", "Max.Torque.RPM"), sep = "@")
df$Max.Torque.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.Value))
df$Max.Torque.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.RPM))

# Split the max horsepower and the RPM at which max horsepower occurs
df <- tidyr::separate(df, Max.Power, into = c("Max.Power.Value", "Max.Power.RPM"), sep = "@")
df$Max.Power.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Power.Value))
df$Max.Power.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Power.RPM))

# remove nulls again in case the power and torque columns are malformed
df <- remove_null(df)

# Numerical and categorical regressors
# Note that Seating.Capacity was omitted to exempt it from 1.5 IQR
numerical <- c(
  "Price", "Year", "Kilometer", "Engine",
  "Max.Power.Value", "Max.Power.RPM", "Max.Torque.Value", "Max.Torque.RPM",
  "Length", "Width", "Height", "Fuel.Tank.Capacity"
)
categorical <- c(
  "Make", "Fuel.Type", "Transmission", "Location", "Color", 
  "Owner", "Seller.Type", "Drivetrain"
)

# Outliers for numerical regressors
df <- remove_outliers(df, numerical)

######## END OF `clean_data.R` ########



######## START OF `exploratory.Rmd` ########
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(faraway)

# Clean data
# source("preprocess.R") # uncomment this if you have separated code into multiple files
drop_cols <- c(
  "Model", "Location", "Color", "Owner", "Seller.Type", "Max.Power", "Max.Torque", "Drivetrain", "Make", "Fuel.Type"
)
data <- read.csv("car details v4.csv")
df <- remove_cols(data, drop_cols)
df <- remove_null(df)
df$Transmission <- ifelse(df$Transmission == "Manual", 0, 1) #Manual = 0, Automatic = 1
df$Engine <- as.numeric(gsub("[^0-9]", "", df$Engine)) #Convert to numbers, ex. "123 cc" -> 123


# Histogram
df %>%
  gather(key = "var", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15, fill = "blue", color = "black", stat="bin") +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")


# Linearity
df %>%
  gather(-Price, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(title = "Pairwise Scatterplot", x = "Value", y = "Price")


# Log Transformation
df_log <- df
df_log$Price <- log(df_log$Price)
df_log %>%
  gather(-Price, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(title = "Pairwise Scatterplot", x = "Value", y = "Price")


# Correlation
cor_matrix <- cor(df_log[, sapply(df_log, is.numeric)])
cor_melted <- melt(cor_matrix)
ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", title = "Correlation Matrix") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3)


# MLR Model
model <- lm(Price ~ ., data = df_log)
summary(model)


vif(model) #Looks like Length have a high Variance Inflation Factor so we can try removing it


model_without_length <- lm(Price ~ . - Length, data = df_log)
summary(model_without_length)
vif(model_without_length)


cat("\nInterpretation of Model Parameter Estimates:\n")
print(coef(summary(model_without_length)))

# Residual plot
residuals_values <- residuals(model_without_length)
plot(residuals_values, type = 'p', main = "Residual Plot", xlab = "Observation Index", ylab = "Residuals")
abline(h = 0, col = "red")


# Histogram of Residuals
model_residuals = model_without_length$residuals
hist(model_residuals)


qqnorm(model_residuals)
qqline(model_residuals)


######## START OF `manual_select_no_interaction.Rmd` ########

library(car)
library(reshape2)
library(ggplot2)
library(MASS)
library(interactions)

# source("clean_data.R") # uncomment if running as separate file
# contents of `clean_data.R`. Set condition to false if running as separate file
if (T) {
  # Load data
  data <- read.csv("car details v4.csv")
  
  df <- data
  df <- remove_null(df)
  df <- df[!duplicated(df), ]
  
  # Remove the " cc" suffix from Engine
  df$Engine <- as.numeric(gsub("[^0-9]", "", df$Engine)) # Convert to numbers, ex. "123 cc" -> 123
  
  # Split max torque and the RPM at which max torque occurs
  df <- tidyr::separate(df, Max.Torque, into = c("Max.Torque.Value", "Max.Torque.RPM"), sep = "@")
  df$Max.Torque.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.Value))
  df$Max.Torque.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.RPM))
  
  # Split the max horsepower and the RPM at which max horsepower occurs
  df <- tidyr::separate(df, Max.Power, into = c("Max.Power.Value", "Max.Power.RPM"), sep = "@")
  df$Max.Power.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Power.Value))
  df$Max.Power.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Power.RPM))
  
  # remove nulls again in case the power and torque columns are malformed
  df <- remove_null(df)
  
  # Numerical and categorical regressors
  # Note that Seating.Capacity was omitted to exempt it from 1.5 IQR
  numerical <- c(
    "Price", "Year", "Kilometer", "Engine",
    "Max.Power.Value", "Max.Power.RPM", "Max.Torque.Value", "Max.Torque.RPM",
    "Length", "Width", "Height", "Fuel.Tank.Capacity"
  )
  categorical <- c(
    "Make", "Fuel.Type", "Transmission", "Location", "Color", 
    "Owner", "Seller.Type", "Drivetrain"
  )
  
  # Outliers for numerical regressors
  df <- remove_outliers(df, numerical)
}

df <- remove_cols(df, c("Color", "Model"))


# Remove columns with only one observation and affected rows
res <- convert_categorical(df, categorical)
design <- as.data.frame(res$dummy)

singles <- c()
bad_idx <- c()
for (col in colnames(design)) {
  if (sum(design[, col] != 0) <= 1) {
    singles <- c(singles, col)
    bad_idx <- c(bad_idx, which(design[, col] != 0))
  }
}
singles
bad_idx

df <- df[-bad_idx, ]

## Box cox
bc <- boxcox(Price ~., data = df)
bc$x[which.max(bc$y)]

# For the sake of interpretability, we will use a log transformation, since $\lambda \approx 0$.


x <- df
x$Price <- log(df$Price)
names(x)[names(x) == "Price"] <- "log(Price)"

colnames(x)


## Inspect correlation
cor_matrix <- cor(x[, sapply(x, is.numeric)])
cor_melted <- melt(cor_matrix)
ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", title = "Correlation Matrix") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3)


## Trim Regressors Based on Intuition
# Engine has high correlation with other regressors, and is highly related to more relevant statistics like torque and horsepower, so it is dropped.

# We need really only one dimension of the car. Height and length are not as correlated to price as width, and width gives more information about engine capacity, 

# All the torque and horsepower regressors are highly related. A typical car driver will prioritize maximum horsepower over the others, so only Max.Power.Value was kept.

# Fuel tank capacity information seems to be included in many other stats due its high correlation with many other regressors, so it was dropped.

# We do not think that color will be a useful predictor. Additionally, there are too many car models, so color and model are removed.

x_trim <- remove_cols(x, c("Engine", "Length", "Max.Torque.RPM", "Max.Torque.Value", "Max.Power.RPM", "Color", "Model", "Height", "Fuel.Tank.Capacity"))
colnames(x_trim)


## Inspect Multicollinearity and Leverage
final <- lm(`log(Price)` ~., data = x_trim) 
vif(final, type = "predictor")

# We get that GVIF^(1/(2*Df)) $> 2.236068 \approx \sqrt5$, for "Max.Power.Value," but this is known to be important, does not exceed the threshold by a lot, and no interactions are indicated, so we keep it.


which(abs(rstudent(final)) > 4)
# After inspecting these data points, we do not find a good reason to remove them (i.e., they are not clerical errors).


plot(final)


## Inspect Model Coefficients
summary(final)
anova(final)


# Drivetrain is not a significant predictor. We will drop it for model simplicity.
temp <- final
final <- lm(`log(Price)` ~ . - Drivetrain, data = x_trim)

summary(final)
anova(final)


## Normalized Coefficients
normalized <- x_trim
for (col in colnames(normalized)) {
  if (col != "log(Price)" && !(col %in% categorical)) {
    normalized[, col] <- (normalized[, col] - mean(normalized[, col])) / sd(normalized[, col])
  }
}

normalized_model <- lm(`log(Price)` ~ . - Drivetrain, data = normalized)

summary(normalized_model)

sort(abs(normalized_model$coefficients), decreasing = T)


## Confidence Intervals
confint(normalized_model, level = 0.95)


## Inspection of Seller Type

# First we check the data in general.
comp <- aov(`log(Price)`~Seller.Type, data = x)
summary(comp)
TukeyHSD(comp, conf.level=.95)

# Next, we check what our model would say.
linearHypothesis(final, c("Seller.TypeCorporate = Seller.TypeIndividual"))

# Corporate sellers are able to sell their cars at a statistically significantly higher price compared to individual sellers. Considering that our model states that their impact on price is not significant, it could mean that there are covariates that affect the seller's ability get a price, and this covariates could be handled by our model.


## Inspection of Owner Number

# First inspect the data alone.
comp <- aov(`log(Price)`~Owner, data = x)
summary(comp)
TukeyHSD(comp, conf.level=.95)

# Next, we check what our model would say.
linearHypothesis(final, c("OwnerSecond = 0", "OwnerThird = 0", "OwnerUnRegistered Car = 0"))

# None are significantly different from baseline (first owner) according to our model, but for some reason third owner is significantly different from first and second in the original data. This heavily implies that there is some other covariate impacting the price for third owner cars.

######## END OF `manual_select_no_interaction.Rmd` ########


######## START OF `auto_select_aic_no_interaction.pdf` ########
library(car)
library(reshape2)
library(ggplot2)
library(MASS)
library(interactions)

# source("clean_data.R") # uncomment if running as separate file
# contents of `clean_data.R`. Set condition to false if running as separate file
if (T) {
  # Load data
  data <- read.csv("car details v4.csv")
  
  df <- data
  df <- remove_null(df)
  df <- df[!duplicated(df), ]
  
  # Remove the " cc" suffix from Engine
  df$Engine <- as.numeric(gsub("[^0-9]", "", df$Engine)) # Convert to numbers, ex. "123 cc" -> 123
  
  # Split max torque and the RPM at which max torque occurs
  df <- tidyr::separate(df, Max.Torque, into = c("Max.Torque.Value", "Max.Torque.RPM"), sep = "@")
  df$Max.Torque.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.Value))
  df$Max.Torque.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.RPM))
  
  # Split the max horsepower and the RPM at which max horsepower occurs
  df <- tidyr::separate(df, Max.Power, into = c("Max.Power.Value", "Max.Power.RPM"), sep = "@")
  df$Max.Power.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Power.Value))
  df$Max.Power.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Power.RPM))
  
  # remove nulls again in case the power and torque columns are malformed
  df <- remove_null(df)
  
  # Numerical and categorical regressors
  # Note that Seating.Capacity was omitted to exempt it from 1.5 IQR
  numerical <- c(
    "Price", "Year", "Kilometer", "Engine",
    "Max.Power.Value", "Max.Power.RPM", "Max.Torque.Value", "Max.Torque.RPM",
    "Length", "Width", "Height", "Fuel.Tank.Capacity"
  )
  categorical <- c(
    "Make", "Fuel.Type", "Transmission", "Location", "Color", 
    "Owner", "Seller.Type", "Drivetrain"
  )
  
  # Outliers for numerical regressors
  df <- remove_outliers(df, numerical)
}


df <- remove_cols(df, c("Color", "Model"))

# Remove columns with only one observation and affected rows
res <- convert_categorical(df, categorical)
design <- as.data.frame(res$dummy)

singles <- c()
bad_idx <- c()
for (col in colnames(design)) {
  if (sum(design[, col] != 0) <= 1) {
    singles <- c(singles, col)
    bad_idx <- c(bad_idx, which(design[, col] != 0))
  }
}
singles
bad_idx

df <- df[-bad_idx, ]


## Box Cox
bc <- boxcox(Price ~., data = df)
bc$x[which.max(bc$y)]
# For the sake of interpretability, we will use a log transformation, since $\lambda \approx 0$.

## Iteratively Remove Multicollinear Regressors
x <- df
model <- lm(log(Price) ~., data = x)
removed <- c()

finished <- F
while(!finished) {
  temp <- car::vif(model)[, "GVIF^(1/(2*Df))"]
  worst <- names(which.max(temp))
  if (length(temp) > 0 && temp[worst] > sqrt(5)) {
    x <- remove_cols(x, c(worst))
    model <- lm(log(Price) ~., data = x)
    
    removed <- c(removed, worst)
  } else {
    finished <- T
  }
}

removed


which(abs(rstudent(model)) > 4)
# After inspecting these data points, we do not find a good reason to remove them (i.e., they are not clerical errors).


vif(model, type = "predictor")
# We get that GVIF^(1/(2*Df)) $< 2.236068 \approx \sqrt5$ for all regressors, and no interactions are indicated, so there is likely no multicollinearity.


plot(model)


## Backwards Stepwise Search
# We do a backwards search since our model already conforms to the linear assumptions and is performing well. We simply wish to reduce the model size now.
reduced_model <- step(model, direction = "backward", data = x, trace = 0)
old <- names(model$coefficients)
new <- names(reduced_model$coefficients)

old[!(old %in% new)]
# Seller type and seating capacity were removed by AIC.


## Inspect Model Coefficients
summary(reduced_model)
anova(reduced_model)


## Final Model Verification
plot(reduced_model)


## Normalized Coefficients
# removed by AIC. remember multicollinear columns were already removed
normalized <- remove_cols(x, c("Seating.Capacity", "Seller.Type")) 

for (col in colnames(normalized)) {
  if (col != "Price" && !(col %in% categorical)) {
    normalized[, col] <- (normalized[, col] - mean(normalized[, col])) / sd(normalized[, col])
  }
}

normalized_model <- lm(log(Price) ~ ., data = normalized)
summary(normalized_model)

sort(abs(normalized_model$coefficients), decreasing = T)

## Confidence Intervals
confint(normalized_model, level = 0.95)

######## END OF `auto_select_aic_no_interaction.Rmd` ########


######## START OF `auto_select_bic_no_interaction.Rmd` ########
library(car)
library(reshape2)
library(ggplot2)
library(MASS)
library(interactions)

# source("clean_data.R") # uncomment if running as separate file
# contents of `clean_data.R`. Set condition to false if running as separate file
if (T) {
  # Load data
  data <- read.csv("car details v4.csv")
  
  df <- data
  df <- remove_null(df)
  df <- df[!duplicated(df), ]
  
  # Remove the " cc" suffix from Engine
  df$Engine <- as.numeric(gsub("[^0-9]", "", df$Engine)) # Convert to numbers, ex. "123 cc" -> 123
  
  # Split max torque and the RPM at which max torque occurs
  df <- tidyr::separate(df, Max.Torque, into = c("Max.Torque.Value", "Max.Torque.RPM"), sep = "@")
  df$Max.Torque.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.Value))
  df$Max.Torque.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.RPM))
  
  # Split the max horsepower and the RPM at which max horsepower occurs
  df <- tidyr::separate(df, Max.Power, into = c("Max.Power.Value", "Max.Power.RPM"), sep = "@")
  df$Max.Power.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Power.Value))
  df$Max.Power.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Power.RPM))
  
  # remove nulls again in case the power and torque columns are malformed
  df <- remove_null(df)
  
  # Numerical and categorical regressors
  # Note that Seating.Capacity was omitted to exempt it from 1.5 IQR
  numerical <- c(
    "Price", "Year", "Kilometer", "Engine",
    "Max.Power.Value", "Max.Power.RPM", "Max.Torque.Value", "Max.Torque.RPM",
    "Length", "Width", "Height", "Fuel.Tank.Capacity"
  )
  categorical <- c(
    "Make", "Fuel.Type", "Transmission", "Location", "Color", 
    "Owner", "Seller.Type", "Drivetrain"
  )
  
  # Outliers for numerical regressors
  df <- remove_outliers(df, numerical)
}

df <- remove_cols(df, c("Color", "Model"))

# Remove columns with only one observation and affected rows
res <- convert_categorical(df, categorical)
design <- as.data.frame(res$dummy)

singles <- c()
bad_idx <- c()
for (col in colnames(design)) {
  if (sum(design[, col] != 0) <= 1) {
    singles <- c(singles, col)
    bad_idx <- c(bad_idx, which(design[, col] != 0))
  }
}
singles
bad_idx

df <- df[-bad_idx, ]


## Box Cox
bc <- boxcox(Price ~., data = df)
bc$x[which.max(bc$y)]
# For the sake of interpretability, we will use a log transformation, since $\lambda \approx 0$.


## Iteratively Remove Multicollinear Regressors
x <- df
model <- lm(log(Price) ~., data = x)
removed <- c()

finished <- F
while(!finished) {
  temp <- car::vif(model)[, "GVIF^(1/(2*Df))"]
  worst <- names(which.max(temp))
  if (length(temp) > 0 && temp[worst] > sqrt(5)) {
    x <- remove_cols(x, c(worst))
    model <- lm(log(Price) ~., data = x)
    
    removed <- c(removed, worst)
  } else {
    finished <- T
  }
}

removed


which(abs(rstudent(model)) > 4)
# After inspecting these data points, we do not find a good reason to remove them (i.e., they are not clerical errors).


vif(model, type = "predictor")
# We get that GVIF^(1/(2*Df)) $< 2.236068 \approx \sqrt5$ for all regressors, and no interactions are indicated, so there is likely no multicollinearity.


plot(model)


## Backwards Stepwise Search
# We do a backwards search since our model already conforms to the linear assumptions and is performing well. We simply wish to reduce the model size now.
reduced_model <- step(model, direction = "backward", data = x, trace = 0, k = log(nrow(x)))
old <- names(model$coefficients)
new <- names(reduced_model$coefficients)

old[!(old %in% new)]
# Location, seller type, owner type, and seating capacity were removed by BIC.

## Inspect Model Coefficients
summary(reduced_model)
anova(reduced_model)


## Final Model Verification
plot(reduced_model)


## Normalized Coefficients
# removed by BIC. remember multicollinear rows are already removed
normalized <- remove_cols(x, c("Location", "Seating.Capacity", "Seller.Type", "Owner")) 

for (col in colnames(normalized)) {
  if (col != "Price" && !(col %in% categorical)) {
    normalized[, col] <- (normalized[, col] - mean(normalized[, col])) / sd(normalized[, col])
  }
}

normalized_model <- lm(log(Price) ~ ., data = normalized)
summary(normalized_model)

sort(abs(normalized_model$coefficients), decreasing = T)


## Confidence Intervals
confint(normalized_model, level = 0.95)

