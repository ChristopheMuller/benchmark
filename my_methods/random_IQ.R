# put all the dependencies here:

# this is an example
# the name of the imputing functioin or it's wrapper should start wth "impute_"
# INPUT: incomplete data as an argument
# OUTPUT: return an imputed data 

impute_random_IQ <- function(missdf, ...) {
  
  # apply returns a matrix or data frame, so we catch it
  imputed_mat <- apply(missdf, 2, function(col) {
    if (is.numeric(col)) {
      observed_values <- col[!is.na(col)]
      if (length(observed_values) > 0) {
        Q1 <- quantile(observed_values, 0.25, na.rm = TRUE)
        Q3 <- quantile(observed_values, 0.75, na.rm = TRUE)
        IQR_values <- observed_values[observed_values >= Q1 & observed_values <= Q3]
        
        if (length(IQR_values) > 0) {
          # Use regular assignment <- instead of <<-
          col[is.na(col)] <- sample(IQR_values, sum(is.na(col)), replace = TRUE)
        } else {
          col[is.na(col)] <- sample(observed_values, sum(is.na(col)), replace = TRUE)
        }
      } else {
        col[is.na(col)] <- 0
      }
    } else {
      observed_values <- col[!is.na(col)]
      if (length(observed_values) > 0) {
        most_frequent <- names(sort(table(observed_values), decreasing = TRUE))[1]
        col[is.na(col)] <- most_frequent
      }
    }
    return(col)
  })

  # apply often converts data.frames to matrices; convert back if needed
  return(as.data.frame(imputed_mat))
}