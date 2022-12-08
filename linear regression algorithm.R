xy <- tibble(
  x = seq(-4, 5, .1),
  y = x^2 - x + 10
)

ggplot(xy,
       aes(x, y))+
  geom_line()

cal <- function(x){
  x ^ 2 - x + 10
}

optim(par = 3, fn = cal)

cal_1 <- function(coeffs){
  x <- coeffs[1]
  x ^ 2 - x + 10
}

optim(par = c(x = 5), fn = cal_1)

#  A linear regression algorithm 
# define a function to calculate the sum of squares 

calc_sum_squares <- function(coeffs){
  intercept <- coeffs[1]
  slope <- coeffs[2]
}

# then call optim passing the initial guess of the coefficients and the function
optim(par, fn)



# Linear regression algorithm
# To truly understand linear regression, it is helpful to know how
# the algorithm works. The code for lm() is hundreds of lines because
# it has to work with any formula and any dataset. However, in the case
# of simple linear regression for a single dataset, you can implement a
# linear regression algorithm in just a few lines of code.
#
# The workflow is
#
# Write a script to calculate the sum of squares.
# Turn this into a function.
# Use R's general purpose optimization function find the coefficients that minimize this.


# Set the intercept to 10
intercept <- 10

# Set the slope to 1
slope <- 1

# Calculate the predicted y values
y_pred <- intercept + slope *x_actual

# Calculate the differences between actual and predicted
y_diff <- y_actual - y_pred

# Calculate the sum of squares
sum(y_diff^2)



calc_sum_of_squares <- function(coeffs) {
  # Get the intercept coeff
  intercept <- coeffs[1]
  
  # Get the slope coeff
  slope <- coeffs[2]
  
  # Calculate the predicted y values
  y_pred <- intercept + slope * x_actual
  
  # Calculate the differences between actual and predicted
  y_diff <- y_actual - y_pred
  
  # Calculate the sum of squares
  sum(y_diff^2)
}


# Optimize the metric
optim(
  # Initially guess 0 intercept and 0 slope
  par = c(intercept = 0, slope = 0), 
  # Use calc_sum_of_squares as the optimization fn
  fn = calc_sum_of_squares
)

# Compare the coefficients to those calculated by lm()
lm(price_twd_msq ~ n_convenience, data = taiwan_real_estate)



