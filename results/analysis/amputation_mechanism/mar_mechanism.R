#' Introduce Missingness in Variables Conditionally
#'
#' @param X A dataframe or matrix
#' @param p Probability of making other variables missing
#' @return Modified dataframe or matrix with potential missingness
#' 

dist_shift <- function(X, ratio = 0.5, ...) {
  
  X <- data.frame(X, check.names = FALSE)
  
  # Get dimensions of X
  n_rows <- nrow(X)
  n_cols <- ncol(X)
  
  # Check if there are at least two columns
  if (n_cols < 2) {
    stop("Input matrix/dataframe must have at least two columns")
  }
  
  # Select random two columns
  last_two_cols <- X[, (n_cols-1):n_cols]
  random_two_cols_id <- sample(1:ncol(X), 2, replace = FALSE)
  random_two_cols <- X[, random_two_cols_id]
  
  # Calculate column-wise means of the last two columns
  col_means <- colMeans(random_two_cols, na.rm = TRUE)
  
  # Create a logical matrix indicating where values exceed mean
  exceeds_mean <- cbind(random_two_cols[, 1] > col_means[1], random_two_cols[, 2] > col_means[2])
  
  # Determine if row meets missingness condition
  trigger_missingness <- rowSums(exceeds_mean) > 0
  
  n0 <- sum(trigger_missingness)
  
  ## Expected number of missingness: n0*(d-2)*p (Expectation of Binomial)
  ## We would like to control r=n0*(d-2)*p/(n*d), resulting in this formula:
  p <- (ratio * n_rows * n_cols) / (n0 * (n_cols - 2))
  
  trigger_missingness <- ifelse(trigger_missingness, p, 0)
  p <- min(max(p, 0), 1)
  
  # Introduce missingness in remaining columns
  for (i in setdiff(1:n_cols, random_two_cols_id)) {
    X[trigger_missingness & (runif(n_rows) < p), i] <- NA
  }
  
  return(as.data.frame(X, check.names = FALSE))
}




set.seed(33)  # for reproducibility

n<-10000
p<-3


# Simulate 3D standard Gaussian
X <- matrix(rnorm(n*p), ncol=p)

c


# Apply missingness function
X.NA <- dist_shift(X, ratio = 0.8)


library(ggplot2)

plt_dat <- rbind(data.frame(value = X[!is.na(X.NA[,3]), 3],
                            missing = FALSE),
                 data.frame(value = X[is.na(X.NA[,3]), 3], 
                            missing = TRUE))

ggplot(plt_dat, aes(x = value, col = missing, fill = missing)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()


sum(is.na(X.NA))/prod(dim(X.NA))




insert_MAR <- function(dat, ratio = 0, complete_cols = 1) {
  n <- nrow(dat)
  p <- ncol(dat)
  total_missing <- round(n * p * ratio, 0)
  
  if(p < 2) {
    stop(paste0("The data should contain at least two columns!",
                "Your data contains ", p, "."))
  }
  
  tmp_missing_per_column <- rmultinom(1, total_missing, rep(1/p, p - complete_cols))[, 1]
  random_complete_col <- sample(1:p, size = complete_cols)
  
  missing_per_column <- numeric(p)
  missing_per_column[-c(random_complete_col)] <- tmp_missing_per_column
  
  ids_0 <- which(missing_per_column == 0)
  ids_non_0 <- which(missing_per_column != 0)
  
  for(i in ids_non_0) {
    
    n_cols_to_sample <- sample(1:length(ids_0), size = 1)
    sampled_cols_ind <- sample(ids_0, n_cols_to_sample)
    
    sampled_cols <- scale(dat[, sampled_cols_ind])
    sampled_scales <- rnorm(n_cols_to_sample)
    print(sampled_scales)
    
    scaled_sum <- as.matrix(sampled_cols) %*% sampled_scales
    
    dat[order(scaled_sum)[1:missing_per_column[i]], i] <- NA
  }
  print("dupa")
  dat
}




n<-10000
p<-3


# Simulate 3D standard Gaussian

library(ggplot2)

generate <- function() {
  X <- matrix(rnorm(n*p), ncol=p)
  
  X[,1] <- 2*X[,2] + 1*X[,3] + rnorm(n)
  
  X.NA <- insert_MAR(X, 0.4, 1)
  
  if(any(is.na(X.NA[,1]))) {
    plt_dat <- rbind(data.frame(value = X[!is.na(X.NA[,1]), 1],
                                missing = FALSE),
                     data.frame(value = X[is.na(X.NA[,1]), 1], 
                                missing = TRUE))
    
    return(ggplot(plt_dat, aes(x = value, col = missing, fill = missing)) +
             geom_density(alpha = 0.5) + 
             theme_minimal())
  }
}

set.seed(2)

res <- lapply(1:16, function(i) generate())

res <- res[which(!sapply(res, is.null))]

a <- patchwork::wrap_plots(res, 3, 4) & theme(legend.position = "bottom") 

a + plot_layout(guides = "collect")




# Simulate 3D standard Gaussian

library(ggplot2)

n<-10000
p<-3



generate <- function(n, p, ratio = 0.5) {
  X <- matrix(rnorm(n*p), ncol=p)
  
  X[,1] <- 2*X[,2] + 1*X[,3] + rnorm(n)
  
  X.NA <- mice::ampute(X, prop = 0.5)$amp
  
  if(any(is.na(X.NA[,1]))) {
    plt_dat <- rbind(data.frame(value = X[!is.na(X.NA[,3]), 3],
                                missing = FALSE),
                     data.frame(value = X[is.na(X.NA[,3]), 3], 
                                missing = TRUE))
    
    print(colSums(is.na(X.NA))/nrow(X.NA))
    
    return(ggplot(plt_dat, aes(x = value, col = missing, fill = missing)) +
             geom_density(alpha = 0.5) + 
             theme_minimal())
  }
}

set.seed(2)

res <- lapply(1:16, function(i) generate(n, p, ratio))

res <- res[which(!sapply(res, is.null))]

a <- patchwork::wrap_plots(res, 3, 4) & theme(legend.position = "bottom") 

a + plot_layout(guides = "collect")




