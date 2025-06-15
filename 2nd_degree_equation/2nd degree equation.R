# Solve 2nd degree equation - Quadratic Equation

# Libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)

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

quadratic(1, -10, 7)

# Function Plotting using ggplot2 - Based on DataDaft - https://www.youtube.com/watch?v=x7OXp3KvynU

#data for circle
theta <- seq(0, 2*pi, length.out = 300)
circle_df <- data.frame(
  x = 1 * cos(theta),
  y = 1 * sin(theta)
)

plot_functions <- ggplot(data.frame(x = c(-100, 100)), aes(x = x)) + 
  geom_vline(xintercept = 0, color = "black", size = 0.5) +  # y-axis
  geom_hline(yintercept = 0, color = "black", size = 0.5) +  # x-axis
  #geom_path(data = circle_df, aes(x = x, y = y), color = "purple", size = 1) +
  stat_function(aes(y = ..y..), fun = function(x){x^2 - 10*x+7}, color="red", lwd = 1) +
  #stat_function(aes(y = ..y..), fun = function(x){x}, color="blue", lwd = 1) +
  #stat_function(aes(y = ..y..), fun = function(x){sin(x)}, color="red", lwd = 1)+
  xlim(-50,50)+
  ylim(-50,50)+
  #annotate(geom="text", label = "Circle", x = -4.8, y = 4.9, size =4, color='purple') + 
  #annotate(geom="text", label = "Cubic", x = -4.8, y = 4.5, size =4, color='green') +
  #annotate(geom="text", label = "Sinx", x = -4.8, y = 4.1, size =4, color='red') +
  #annotate(geom="text", label = "Linear", x = -4.8, y = 3.7, size =4, color='blue') +
  annotate(geom="text", label = "Quadratic: x^2 -10x +7", x = -30, y = 49, size =4, color='red') +
  coord_fixed()+ 
  theme_fivethirtyeight()

plot_functions
  

ggplotly(plot_functions)


