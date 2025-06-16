#Solve Linear Equation (1st_grade) and line graph (ax-b=0)

#Libaries
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)


linear_equation <- function(a,b) {
  if (a!=0){
    x_0 <- -b/a
    print(paste("The solution is x =",x_0, sep = " " ))
    root_double <- data.frame(x = x_0, y = 0)
    plot_functions <- ggplot(data.frame(x = c(-15, 15)), aes(x = x)) + 
      geom_vline(xintercept = 0, color = "black", size = 0.5) +  # y-axis
      geom_hline(yintercept = 0, color = "black", size = 0.5) +  # x-axis
      stat_function(aes(y = ..y..), fun = function(x){a*x +b}, color="red", lwd = 1) +
      annotate(geom="text",  label = paste("Line Chart: ", a, "*x - ", b), x = -14, y = 14, size =5, color='red') +
      coord_fixed()+ 
      geom_point(data = root_double, aes(x = x_0, y = y), color = "blue", size = 3) +  # plot roots
      theme_fivethirtyeight()+
      ylim(-15,15)
    ggplotly(plot_functions)
  }
  else if(a==0 && b!=0) {
    print("The equation is impossible to solve")
  }
  else if (a==0 && b==0){
    print("The equations has infinite solutions")
  }
}
linear_equation(1,1)

# For a and b not equal to zero, you can use and the solve function, like the below example:

# We want to solve the equation: 5x=15
