# DIP
Distribution-insensitive influential point detection algorithm


(1) DIP method.txt contains R codes for DIP methods, which contains all functions and usage of DIP method
## DIP_ID=newmethod(X,Y,n,p,n_subset,ep=0.05,alpha)
## input: X : predictor n*p 
          Y : response 1*n
		  n: number of observations
		  p: dimension of predictors
		  n_subset: number of RSSampling times
		  ep: tuning parameter 
		  alpha: FDR significance level
## output: ID of influential points

(2) generate_simulation_data.txt illustrate the data generation procedure in  paper "Distribution-insensitive  Influential Point Detection for High
Dimensional Regression Model". It contains 8 cases for data generation. The Example and cases are corresponding to the main paper.
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


(3) Comparison of four methods.txt
Comparision of 4 method in the main paper:  DIP HIM MIP IPOD (PIQ method was implemented in matlab)
