#' Introduce Missingness in Variables Conditionally
#'
#' @param X A dataframe or matrix
#' @param p Probability of making other variables missing
#' @return Modified dataframe or matrix with potential missingness
introduce_missingness <- function(X, p) {
  # Ensure X is a matrix or dataframe
  X <- as.matrix(X)
  
  # Get dimensions of X
  n_rows <- nrow(X)
  n_cols <- ncol(X)
  
  # Check if there are at least two columns
  if (n_cols < 2) {
    stop("Input matrix/dataframe must have at least two columns")
  }
  
  # Select the last two columns
  last_two_cols <- X[, (n_cols-1):n_cols]
  
  # Calculate column-wise means of the last two columns
  col_means <- colMeans(last_two_cols, na.rm = TRUE)
  
  # Create a logical matrix indicating where values exceed mean
  exceeds_mean <- t(apply(last_two_cols, 1, function(row) row > col_means))
  
  # Determine if row meets missingness condition
  trigger_missingness <- apply(exceeds_mean, 1, any)
  
  # Create a matrix of missingness probabilities
  missingness_matrix <- matrix(
    ifelse(trigger_missingness, p, 0), 
    nrow = n_rows, 
    ncol = n_cols - 2
  )
  
  # Introduce missingness in remaining columns
  if (n_cols > 2) {
    for (i in 1:(n_cols-2)) {
      X[, i] <- ifelse(
        runif(n_rows) < missingness_matrix[, i], 
        NA, 
        X[, i]
      )
    }
  }
  
  return(X)
}




set.seed(123)  # for reproducibility

n<-3000
p<-3

# Simulate 3D standard Gaussian
X <- matrix(rnorm(n*p), ncol=p)

X[,1]<- 2*X[,2] + 1*X[,3] + rnorm(n)



# Apply missingness function
X.NA <- introduce_missingness(X, p=0.8)


# Create a side-by-side histogram
par(mfrow=c(1,2))

# Histogram when result[,1] is NOT missing
hist(X[!is.na(X.NA[,1]), 1], 
     main="X[,1] When No Missingness", 
     xlab="Value", 
     col="blue")

# Histogram when result[,1] is missing
hist(X[is.na(X.NA[,1]), 1], 
     main="X[,1] When Missing", 
     xlab="Value", 
     col="red")


library(ggplot2)

plt_dat <- rbind(data.frame(value = X[!is.na(X.NA[,1]), 1],
                            missing = FALSE),
                 data.frame(value = X[is.na(X.NA[,1]), 1], 
                            missing = TRUE))

ggplot(plt_dat, aes(x = value, col = missing, fill = missing)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()

