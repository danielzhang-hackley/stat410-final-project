# helper functions
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





# exploratory analysis
## clean data
source("preprocess.R")
drop_cols <- c(
  "Model", "Location", "Color", "Owner", "Seller.Type", "Max.Power", "Max.Torque", "Drivetrain", "Make", "Fuel.Type"
)
data <- read.csv("car details v4.csv")
df <- remove_cols(data, drop_cols)
df <- remove_null(df)
df$Transmission <- ifelse(df$Transmission == "Manual", 0, 1) #Manual = 0, Automatic = 1
df$Engine <- as.numeric(gsub("[^0-9]", "", df$Engine)) #Convert to numbers, ex. "123 cc" -> 123

## histogram
df %>%
  gather(key = "var", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15, fill = "blue", color = "black", stat="bin") +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(title = "Histograms of Variables", x = "Value", y = "Count")

## correlation heatmap matrix
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

## pairwise scatterplot
df %>%
  gather(-Price, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(title = "Pairwise Scatterplot", x = "Value", y = "Price")

## log transformation
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





# manual selection
## clean data
data <- read.csv("car details v4.csv")
df <- data
df <- remove_null(df)
df <- df[!duplicated(df), ]

df$Engine <- as.numeric(gsub("[^0-9]", "", df$Engine)) # Remove the " cc" suffix from Engine and Convert to numbers, ex. "123 cc" -> 123

df <- tidyr::separate(df, Max.Torque, into = c("Max.Torque.Value", "Max.Torque.RPM"), sep = "@") # Split max torque and the RPM at which max torque occurs
df$Max.Torque.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.Value))
df$Max.Torque.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Torque.RPM))

df <- tidyr::separate(df, Max.Power, into = c("Max.Power.Value", "Max.Power.RPM"), sep = "@") # Split the max horsepower and the RPM at which max horsepower occurs
df$Max.Power.Value <- as.numeric(gsub("[^0-9]", "", df$Max.Power.Value))
df$Max.Power.RPM <- as.numeric(gsub("[^0-9]", "", df$Max.Power.RPM))

df <- remove_null(df)

numerical <- c(
  "Price", "Year", "Kilometer", "Engine",
  "Max.Power.Value", "Max.Power.RPM", "Max.Torque.Value", "Max.Torque.RPM",
  "Length", "Width", "Height", "Fuel.Tank.Capacity"
)
categorical <- c(
  "Make", "Fuel.Type", "Transmission", "Location", "Color", 
  "Owner", "Seller.Type", "Drivetrain"
)

df <- remove_outliers(df, numerical)
df <- remove_cols(df, c("Color", "Model"))
res <- convert_categorical(df, categorical)
design <- as.data.frame(res$dummy)

## Remove columns with only one observation and affected rows
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

## apply log transformation
x <- df
x$Price <- log(df$Price)
names(x)[names(x) == "Price"] <- "log(Price)"

## inspect correlation
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
x_trim <- remove_cols(x, c("Engine", "Length", "Max.Torque.RPM", "Max.Torque.Value", "Max.Power.RPM", "Color", "Model", "Height", "Fuel.Tank.Capacity"))
model <- lm(`log(Price)` ~., data = x_trim)

## Attempt to add interactions
interactions <- lm(
  `log(Price)` ~ . + Year:Kilometer + Year:Max.Power.Value 
  + Year:Width + Max.Power.Value:Width,
  data = x_trim
)

anova(interactions)

## Remove drivetrain and year x width interaction as they are not significant. All the others are significant even with Bonferroni adjustment.
x_final <- remove_cols(x_trim, c("Drivetrain"))

final <- lm(
  `log(Price)` ~ . + Year:Kilometer + Year:Max.Power.Value + Max.Power.Value:Width,
  data = x_final
)

## Inspect Multicollinearity and Leverage and remove ones with high VIF
vif(final, type = "predictor")
final <- lm(
  `log(Price)` ~ . + Year:Kilometer + Year:Max.Power.Value,
  data = x_final
)

vif(final, type = "predictor")

## check for high leverage points, but do not remove because they are not clerical errors
which(abs(rstudent(final)) > 4)

## inspect residual plots to ensure model validity
plot(final)
anova(final)
summary(final)

## normalized coefficients
normalized <- x_final
for (col in colnames(normalized)) {
  if (col != "log(Price)" && !(col %in% categorical)) {
    normalized[, col] <- (normalized[, col] - mean(normalized[, col])) / sd(normalized[, col])
  }
}

normalized_model <- lm(`log(Price)` ~ . + Year:Kilometer + Year:Max.Power.Value, data = normalized)
sort(abs(normalized_model$coefficients), decreasing = T)

## compare seller types
comp <- aov(`log(Price)`~Seller.Type, data = x)
summary(comp)
TukeyHSD(comp, conf.level=.95)





# model based on AIC
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

## check for high leverage points, but do not remove because they are not clerical errors
which(abs(rstudent(final)) > 4)

## add interactions
names(model$coefficients)[names(model$coefficients) %in% numerical]
model <- lm(
  log(Price) ~ . + Year:Kilometer + Year:Max.Torque.RPM 
  + Year:Length + Year:Width 
  + Max.Torque.RPM:Length + Max.Torque.RPM:Width
  + Length:Width,
  data = x
)

anova(model)

## remove insignificant interactions
model <- lm(
  log(Price) ~ . + Year:Kilometer + Year:Max.Torque.RPM 
  + Year:Length + Max.Torque.RPM:Length,
  data = x
)

anova(model)

## Backwards Stepwise Search based on AIC
reduced_model <- step(model, direction = "backward", data = x, trace = 0)
plot(reduced_model)
anova(reduced_model)
summary(reduced_model)

## Normalized Coefficients
old <- names(model$coefficients)
new <- names(reduced_model$coefficients)
normalized <- remove_cols(x, old[!(old %in% new)]) # removed from multicollinearity test
normalized <- remove_cols(x, c("Seating.Capacity", "Seller.Type"))
for (col in colnames(normalized)) {
  if (col != "Price" && !(col %in% categorical)) {
    normalized[, col] <- (normalized[, col] - mean(normalized[, col])) / sd(normalized[, col])
  }
}

normalized_model <- lm(log(Price) ~ . + Year:Kilometer + Year:Max.Torque.RPM 
                       + Year:Length + Max.Torque.RPM:Length, data = normalized)
sort(abs(normalized_model$coefficients), decreasing = T)





# model selected from BIC
## Backwards Stepwise Search based on BIC
reduced_model <- step(model, direction = "backward", data = x, trace = 0, k = log(nrow(x)))
plot(reduced_model)
anova(reduced_model)
summary(reduced_model)

## Normalized Coefficients
old <- names(model$coefficients)
new <- names(reduced_model$coefficients)
normalized <- remove_cols(x, old[!(old %in% new)]) # removed from multicollinearity test
normalized <- remove_cols(x, c("Seating.Capacity", "Seller.Type"))
for (col in colnames(normalized)) {
  if (col != "Price" && !(col %in% categorical)) {
    normalized[, col] <- (normalized[, col] - mean(normalized[, col])) / sd(normalized[, col])
  }
}

normalized_model <- lm(log(Price) ~ . + Year:Kilometer + Year:Max.Torque.RPM 
                       + Year:Length + Max.Torque.RPM:Length, data = normalized)
sort(abs(normalized_model$coefficients), decreasing = T)

