#1st Grade Equation - Solution 

library(dplyr)

linear_equation <- function(a,b) {
  if (a!=0){
    x <- b/a
  print(paste("The solution is x =",x, sep = " " ))
  }
  else if(a==0 && b!=0) {
    print("The equation is impossible to solve")
  }
  else if (a==0 && b==0){
    print("The equations has infinite solutions")
  }
}
linear_equation(0,0)

# For a and b not equal to zero, you can use and the solve function, like the below example:

# We want to solve the equation: 5x=15

solve(5,15)
