# Function to generate simulation data
generate_simulation_data <- function(example = 1, case = 1, n = 200, p = 1000, ninf = 10, seed = 42) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # Define the beta coefficient
  beta <- rep(0, p)
  beta[1:2] <- 1
  beta[3:4] <- -1
  beta[5] <- 1
  beta[6] <- -1
  beta[7:8] <- 1
  beta[9:10] <- -1
  # The rest (990 elements) are already set to 0
  
  # Create covariance matrix Sigma with sigma_ij = 0.5^|i-j|
  Sigma <- matrix(0, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      Sigma[i, j] <- 0.5^abs(i-j)
    }
  }
  
  # Generate non-influential observations
  Z <- matrix(NA, n, p + 1)  # +1 for the response variable Y
  colnames(Z) <- c(paste0("X", 1:p), "Y")
  
  # Create a data frame to hold all observations
  data <- matrix(NA, n, p + 1)
  
  # Different examples and cases
  if (example == 1) {
    # Example 1: Linear model, normal predictor and normal error
    
    # Generate non-influential observations
    X <- mvtnorm::rmvnorm(n, sigma = Sigma)
    epsilon <- rnorm(n, 0, sqrt(0.25))
    Y <- X %*% beta + epsilon
    
    # Store non-influential observations
    data[, 1:p] <- X
    data[, p + 1] <- Y
    
    # Generate influential observations based on the case
    if (case == 1) {
      # Case 1: Y is shifted by +/- 2.5
      sigma_values <- sample(c(1, -1), ninf, replace = TRUE, prob = c(0.5, 0.5))
      data[1:ninf, p + 1] <- Y[1:ninf] + 2.5 * sigma_values
    } else if (case == 2) {
      # Case 2: X is set to 3 and Y is shifted by theta = 2
      X_inf <- matrix(3, ninf, p)
      Y_inf <- X_inf %*% beta + 2 + rnorm(ninf, 0, sqrt(0.25))
      
      data[1:ninf, 1:p] <- X_inf
      data[1:ninf, p + 1] <- Y_inf
    }
    
  } else if (example == 2) {
    # Example 2: Linear model, non-normal predictor and normal error
    
    # Define mu1
    mu1 <- rep(0, p)
    mu1[1:2] <- 1
    mu1[3:4] <- -1
    
    # Generate non-influential observations from mixture of normals
    X <- matrix(0, n, p)
    for (i in 1:n) {
      if (runif(1) < 0.5) {
        X[i, ] <- mvtnorm::rmvnorm(1, mean = mu1, sigma = Sigma)
      } else {
        X[i, ] <- mvtnorm::rmvnorm(1, mean = -mu1, sigma = Sigma)
      }
    }
    
    epsilon <- rnorm(n, 0, sqrt(0.25))
    Y <- X %*% beta + epsilon
    
    # Store non-influential observations
    data[, 1:p] <- X
    data[, p + 1] <- Y
    
    # Generate influential observations based on the case
    if (case == 1) {
      # Case 1: Y is set to 4 + epsilon
      Y_inf <- 4 + rnorm(ninf, 0, sqrt(0.25))
      data[1:ninf, p + 1] <- Y_inf
    } else if (case == 2) {
      # Case 2: X is from a different distribution and Y follows the model
      mu2 <- rep(0, p)
      mu2[5:6] <- 2
      
      X_inf <- matrix(0, ninf, p)
      for (i in 1:ninf) {
        if (runif(1) < 0.5) {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = mu2, sigma = Sigma)
        } else {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = -mu2, sigma = Sigma)
        }
      }
      
      Y_inf <- X_inf %*% beta + rnorm(ninf, 0, sqrt(0.25))
      
      data[1:ninf, 1:p] <- X_inf
      data[1:ninf, p + 1] <- Y_inf
    } else if (case == 2.1) {
      # Case 2.1: Non-sparse model with s0 = 100
      beta_nonsparse <- rep(0, p)
      beta_nonsparse[1:100] <- 0.5
      
      # Generate influential observations
      mu2 <- rep(0, p)
      mu2[5:6] <- 2
      
      X_inf <- matrix(0, ninf, p)
      for (i in 1:ninf) {
        if (runif(1) < 0.5) {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = mu2, sigma = Sigma)
        } else {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = -mu2, sigma = Sigma)
        }
      }
      
      Y_inf <- X_inf %*% beta_nonsparse + rnorm(ninf, 0, sqrt(0.25))
      
      data[1:ninf, 1:p] <- X_inf
      data[1:ninf, p + 1] <- Y_inf
      
      # Also update non-influential observations to use the same beta
      data[, p + 1] <- data[, 1:p] %*% beta_nonsparse + rnorm(n, 0, sqrt(0.25))
      
    } else if (case == 2.2) {
      # Case 2.2: Non-sparse model with s0 = 1000
      beta_dense <- rep(5/sqrt(1000), p)
      
      # Generate influential observations
      mu2 <- rep(0, p)
      mu2[5:6] <- 2
      
      X_inf <- matrix(0, ninf, p)
      for (i in 1:ninf) {
        if (runif(1) < 0.5) {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = mu2, sigma = Sigma)
        } else {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = -mu2, sigma = Sigma)
        }
      }
      
      Y_inf <- X_inf %*% beta_dense + rnorm(ninf, 0, sqrt(0.25))
      
      data[1:ninf, 1:p] <- X_inf
      data[1:ninf, p + 1] <- Y_inf
      
      # Also update non-influential observations to use the same beta
      data[, p + 1] <- data[, 1:p] %*% beta_dense + rnorm(n, 0, sqrt(0.25))
    }
    
  } else if (example == 3) {
    # Example 3: Linear model, non-normal predictor and non-normal error
    
    if (case == 1) {
      # Define mu1
      mu1 <- rep(0, p)
      mu1[1:2] <- 1
      mu1[3:4] <- -1
      
      # Generate non-influential observations from mixture of normals
      X <- matrix(0, n, p)
      for (i in 1:n) {
        if (runif(1) < 0.5) {
          X[i, ] <- mvtnorm::rmvnorm(1, mean = mu1, sigma = Sigma)
        } else {
          X[i, ] <- mvtnorm::rmvnorm(1, mean = -mu1, sigma = Sigma)
        }
      }
      
      epsilon <- rt(n, df = 3)
      Y <- X %*% beta + epsilon
      
      # Store non-influential observations
      data[, 1:p] <- X
      data[, p + 1] <- Y
      
      # Generate influential observations
      epsilon_inf <- rt(ninf, df = 3)/3
      sigma_inf <- sign(epsilon_inf)
      Y_inf <- 4 * sigma_inf + epsilon_inf
      
      data[1:ninf, p + 1] <- Y_inf
      
    } else if (case == 2) {
      # Generate U from Gamma distribution
      U <- matrix(rgamma(n * p, shape = 2, rate = 3) * 0.5, n, p)
      med_U <- apply(U, 2, median)
      
      # Generate X
      X <- matrix(0, n, p)
      for (i in 1:p) {
        X[, i] <- Sigma[i, i] * (U[, i] - med_U[i])
      }
      
      epsilon <- rt(n, df = 3)
      Y <- X %*% beta + epsilon
      
      # Store non-influential observations
      data[, 1:p] <- X
      data[, p + 1] <- Y
      
      # Generate influential observations
      epsilon_inf <- rt(ninf, df = 3)/3
      sigma_inf <- sign(epsilon_inf)
      Y_inf <- 3.5 * sigma_inf + epsilon_inf
      
      data[1:ninf, p + 1] <- Y_inf
    }
    
  } else if (example == 4) {
    # Example 4: Nonlinear model
    
    if (case == 1) {
      # Define mu1
      mu1 <- rep(0, p)
      mu1[1:2] <- 1
      mu1[3:4] <- -1
      
      # Adjust beta for nonlinear model
      beta_adj <- beta / 3.5
      
      # Generate non-influential observations from mixture of normals
      X <- matrix(0, n, p)
      for (i in 1:n) {
        if (runif(1) < 0.5) {
          X[i, ] <- mvtnorm::rmvnorm(1, mean = mu1, sigma = Sigma)
        } else {
          X[i, ] <- mvtnorm::rmvnorm(1, mean = -mu1, sigma = Sigma)
        }
      }
      
      epsilon <- rnorm(n, 0, sqrt(0.25))
      Y <- sin(X %*% beta_adj) + epsilon
      
      # Store non-influential observations
      data[, 1:p] <- X
      data[, p + 1] <- Y
      
      # Generate influential observations with mu2
      mu2 <- rep(0, p)
      mu2[5:6] <- 2
      
      X_inf <- matrix(0, ninf, p)
      for (i in 1:ninf) {
        if (runif(1) < 0.5) {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = mu2, sigma = Sigma)
        } else {
          X_inf[i, ] <- mvtnorm::rmvnorm(1, mean = -mu2, sigma = Sigma)
        }
      }
      
      Y_inf <- sin(X_inf %*% beta_adj) + rnorm(ninf, 0, sqrt(0.25))
      
      data[1:ninf, 1:p] <- X_inf
      data[1:ninf, p + 1] <- Y_inf
      
    } else if (case == 2) {
      # Adjust beta for nonlinear model
      beta_adj <- beta / 7.5
      
      # Generate U from Gamma distribution
      U <- matrix(rgamma(n * p, shape = 2, rate = 3) * 0.5, n, p)
      med_U <- apply(U, 2, median)
      
      # Generate X
      X <- matrix(0, n, p)
      for (i in 1:p) {
        X[, i] <- Sigma[i, i] * (U[, i] - med_U[i])
      }
      
      epsilon <- rt(n, df = 3)
      Y <- sin(X %*% beta_adj) + epsilon
      
      # Store non-influential observations
      data[, 1:p] <- X
      data[, p + 1] <- Y
      
      # Generate influential observations
      sigma_values <- sample(c(1, -1), ninf, replace = TRUE, prob = c(0.5, 0.5))
      Y_inf <- sin((data[1:ninf, 1:p] %*% beta_adj) + 1.1 * sigma_values) + epsilon[1:ninf]
      
      data[1:ninf, p + 1] <- Y_inf
    }
  }
  
  # Convert to data frame
  df <- as.data.frame(data)
  colnames(df) <- c(paste0("X", 1:p), "Y")
  
  # Create indicator for influential points
  df$influential <- FALSE
  df$influential[1:ninf] <- TRUE
  
  return(df)
}

# Example usage:
# Generate data for Example 1, Case 1
example1_case1 <- generate_simulation_data(example = 1, case = 1)

# Generate data for Example 1, Case 2
example1_case2 <- generate_simulation_data(example = 1, case = 2)

# Generate data for Example 2, Case 1
example2_case1 <- generate_simulation_data(example = 2, case = 1)

# Generate data for Example 2, Case 2
example2_case2 <- generate_simulation_data(example = 2, case = 2)

# Generate data for Example 2, Case 2.1 (non-sparse with s0 = 100)
example2_case2_1 <- generate_simulation_data(example = 2, case = 2.1)

# Generate data for Example 2, Case 2.2 (non-sparse with s0 = 1000)
example2_case2_2 <- generate_simulation_data(example = 2, case = 2.2)

# Generate data for Example 3, Case 1
example3_case1 <- generate_simulation_data(example = 3, case = 1)

# Generate data for Example 3, Case 2
example3_case2 <- generate_simulation_data(example = 3, case = 2)

# Generate data for Example 4, Case 1
example4_case1 <- generate_simulation_data(example = 4, case = 1)

# Generate data for Example 4, Case 2
example4_case2 <- generate_simulation_data(example = 4, case = 2)


