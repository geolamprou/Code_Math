library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)

ui <- fluidPage(
  titlePanel("Solve a Quadratic Equation"),
  sidebarLayout(
    sidebarPanel(
      h3("Please input your numbers"),
      numericInput("a", "Type the a number:", value = 0),
      numericInput("b", "Type the b number:", value = 0),
      numericInput("c", "Type the c number:", value = 0)
    ),
    mainPanel(
      h4("Solution Output"),
      verbatimTextOutput("solution_text"),
      plotlyOutput("quad_plot")
    )
  )
)

server <- function(input, output) {
  output$solution_text <- renderPrint({
    a <- input$a
    b <- input$b
    c <- input$c
    
  if(a==0) {
    print("We have a first degree equation")
  }
  else if(a!=0) {
    Delta <- (b^2)-(4*a*c)
    if (Delta >0){
      x_1 <- (-b + sqrt(Delta))/(2*a)
      x_2 <- (-b - sqrt(Delta))/(2*a)
      print(paste("The 1st root of the equation are x_1 =", x_1, "and the second root is x_2=",x_2, sep = " " ))
      roots_df <- data.frame(x = c(x_1, x_2), y = c(0, 0))
      plot_functions <- ggplot(data.frame(x = c(-10, 10)), aes(x = x)) + 
        geom_vline(xintercept = 0, color = "black", size = 0.5) +  # y-axis
        geom_hline(yintercept = 0, color = "black", size = 0.5) +  # x-axis+
        stat_function(aes(y = ..y..), fun = function(x){a*(x^2) + (b*x)+c}, color="red", lwd = 1, n=10000) +
        geom_point(data = roots_df, aes(x = x, y = y), color = "blue", size = 3) +  # plot roots
        xlim(-15,15)+
        ylim(-15,15)+
        annotate(geom="text", label = "Chart of f", x = -14, y = 14, size =4, color='red') +
        coord_fixed()+ 
        theme_fivethirtyeight()
      ggplotly(plot_functions)
      }
    else if(Delta == 0){
      x_0 <- - b/(2*a)
      print(paste("The equation has the rout x= ",x_0, "as a double root.", sep = " "))
      root_double <- data.frame(x = x_0, y = 0)
      plot_functions <- ggplot(data.frame(x = c(-10, 10)), aes(x = x)) + 
        geom_vline(xintercept = 0, color = "black", size = 0.5) +  # y-axis
        geom_hline(yintercept = 0, color = "black", size = 0.5) +  # x-axis+
        stat_function(aes(y = ..y..), fun = function(x){a*(x^2) + (b*x)+c}, color="red", lwd = 1, n=10000) +
        geom_point(data = root_double, aes(x = x, y = y), color = "blue", size = 3) +  # plot roots
        xlim(-15,15)+
        ylim(-15,15)+
        annotate(geom="text", label = "Chart of f", x = -14, y = 14, size =4, color='red') +
        coord_fixed()+ 
        theme_fivethirtyeight()
      ggplotly(plot_functions)
      }
    else if(Delta <0) {
      print("The equation has no roots in R set!")
    }
  }
  
  }
  )

  output$quad_ploy <- renderPlotly({
    a <- input$a
    b <- input$b
    c <- input$c
    
    if(a==0) {
      print("We have a first degree equation")
    }
    else if(a!=0) {
      Delta <- (b^2)-(4*a*c)
      if (Delta >0){
        x_1 <- (-b + sqrt(Delta))/(2*a)
        x_2 <- (-b - sqrt(Delta))/(2*a)
        print(paste("The 1st root of the equation are x_1 =", x_1, "and the second root is x_2=",x_2, sep = " " ))
        roots_df <- data.frame(x = c(x_1, x_2), y = c(0, 0))
        plot_functions <- ggplot(data.frame(x = c(-10, 10)), aes(x = x)) + 
          geom_vline(xintercept = 0, color = "black", size = 0.5) +  # y-axis
          geom_hline(yintercept = 0, color = "black", size = 0.5) +  # x-axis+
          stat_function(aes(y = ..y..), fun = function(x){a*(x^2) + (b*x)+c}, color="red", lwd = 1, n=10000) +
          geom_point(data = roots_df, aes(x = x, y = y), color = "blue", size = 3) +  # plot roots
          xlim(-15,15)+
          ylim(-15,15)+
          annotate(geom="text", label = "Chart of f", x = -14, y = 14, size =4, color='red') +
          coord_fixed()+ 
          theme_fivethirtyeight()
        ggplotly(plot_functions)
      }
      else if(Delta == 0){
        x_0 <- - b/(2*a)
        print(paste("The equation has the rout x= ",x_0, "as a double root.", sep = " "))
        root_double <- data.frame(x = x_0, y = 0)
        plot_functions <- ggplot(data.frame(x = c(-10, 10)), aes(x = x)) + 
          geom_vline(xintercept = 0, color = "black", size = 0.5) +  # y-axis
          geom_hline(yintercept = 0, color = "black", size = 0.5) +  # x-axis+
          stat_function(aes(y = ..y..), fun = function(x){a*(x^2) + (b*x)+c}, color="red", lwd = 1, n=10000) +
          geom_point(data = root_double, aes(x = x, y = y), color = "blue", size = 3) +  # plot roots
          xlim(-15,15)+
          ylim(-15,15)+
          annotate(geom="text", label = "Chart of f", x = -14, y = 14, size =4, color='red') +
          coord_fixed()+ 
          theme_fivethirtyeight()
        ggplotly(plot_functions)
      }
      else if(Delta <0) {
        print("The equation has no roots in R set!")
      }
    }
    
  }
  )

}

shinyApp(ui, server)