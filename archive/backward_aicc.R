library(MuMIn) # For AICc calculation

# Define a custom stepwise function with AICc
backward_aicc <- function(full_model) {
  current_model <- full_model
  
  # Backward elimination loop
  while (TRUE) {
    # Get all possible single-term deletions
    scope <- drop1(current_model, test = "none")
    
    # Calculate AICc for each model
    aicc_values <- sapply(scope$terms[-1], function(x) {
      reduced_model <- update(current_model, paste(". ~ . -", x))
      AICc(reduced_model)
    })
    
    sprintf("%s\n", str(rownames(scope)))
    cat("\n")
    sprintf("%s\n", str(rownames(scope)[2]))
    cat("\n")
    sprintf("%s\n", str(rownames(scope)[3]))
    
    # Check if removing any term improves the AICc
    best_term <- names(which.min(aicc_values))
    best_aicc <- min(aicc_values)
    
    if (best_aicc < AICc(current_model)) {
      # Update the model by removing the term with the lowest AICc
      current_model <- update(current_model, paste(". ~ . -", best_term))
    } else {
      # Stop if no improvement in AICc
      break
    }
  }
  
  return(current_model)
}
