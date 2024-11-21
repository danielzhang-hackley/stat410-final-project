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

convert_categorial <- function(df, categorical) {
  #' Convert the categorical variables into factors; exclude the numerical
  #' values specified in the "factorization." Returns a list with "design" being
  #' the new design matrix, and "reference" being the reference levels for each
  #' categorical value.
  
  copy <- df
  ref <- data.frame(matrix(nrow = 1, ncol = ncol(df)))
  colnames(ref) <- colnames(df)
  
  for (column in colnames(copy)) {
    if (column %in% categorical){
      copy[, column] <- factor(copy[, column])
      ref[1, column] <- levels(copy[, column])[1]
    }
  }
  
  return (list("design" = model.matrix(~., data = copy), "reference" = ref, "converted"=copy))
}

convert_categorial_2 <- function(df, numerical) {
  #' Convert the categorical variables into factors; exclude the numerical
  #' values specified in the "factorization." Returns a list with "design" being
  #' the new design matrix, and "reference" being the reference levels for each
  #' categorical value.
  
  copy <- df
  ref <- data.frame(matrix(nrow = 1, ncol = ncol(df)))
  colnames(ref) <- colnames(df)
  
  for (column in colnames(copy)) {
    if (!(column %in% numerical)){
      copy[, column] <- factor(copy[, column])
      ref[1, column] <- levels(copy[, column])[1]
    }
  }
  
  return (list("design" = copy, "reference" = ref))
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