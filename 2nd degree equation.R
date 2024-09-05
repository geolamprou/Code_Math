# Solve 2nd degree equation - Quadratic Equation

# Libraries
library(dplyr)

# Set a function for solving quadratic equation

quadratic <- function(a,b,c) {
  if(a==0) {
    print("We have a first degree equation")
  }
  else if(a!=0) {
    Delta <- (b^2)-(4*a*c)
    if (Delta >0){
      x_1 <- (-b + sqrt(Delta))/(2*a)
      x_2 <- (-b - sqrt(Delta))/(2*a)
      print(paste("The 1st root of the equation are x_1 =", x_1, "and the second root is x_2=",x_2, sep = " " ))
    }
      else if(Delta == 0){
        x <- - b/(2*a)
        print(paste("The equation has the rout x= ",x, "as a double root.", sep = " "))
      }
      else if(Delta <0) {
        print("The equation has no roots in R set!")
      }
  }
}

quadratic(1, 4, 4)





