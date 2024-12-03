source("preprocess.R")

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