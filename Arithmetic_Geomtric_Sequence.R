# Arithmetic and Geometric Sequence

#Libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(bsts)
library(patchwork)


#Arithmetic Sequence

arithm_seq <- seq(from = 1,to= 50,by=3)


# Find the a_n term

find_a_n_arithm <- function(a_1,n,w) {
  a_n <- a_1 + (n-1)*w
  print(paste("The",n,"term of the numerical sequence is:", a_n))
}

find_a_n_arithm(1, 18, 3)

#Sum of all terms of

sum_all_arithm <- sum(arithm_seq)
sum_all_arithm

#Plot all the terms

arithm_seq_df <- data.frame(terms=index(arithm_seq), numbers=arithm_seq)
 

arithm_plot <- ggplot(arithm_seq_df, aes(x=terms, y=numbers))+
                  geom_point(color='red', size=2)+
                  theme_pander()+
                  ggtitle("Plotting the Arithmetic Sequence")+
                  xlab("Terms")+
                  ylab("Numbers")
ggplotly(arithm_plot)


#Geometric Sequence

geom_seq <- GeometricSequence(17, 1, 2)
geom_seq
# Find the a_n term

find_a_n_geom <- function(a_1,n,l) {
  a_n <- a_1 * (l^(n-1))
  print(paste("The",n,"term of the geometric sequence is:", a_n))
}

find_a_n_geom(1, 18, 2)

# Sum all terms of geometric sequence

sum_geom <- sum(geom_seq)
sum_geom

#Plot all terms of geometric sequence

geom_seq_df <- data.frame(terms = index(geom_seq), numbers=geom_seq)

geom_plot <- ggplot(geom_seq_df, aes(x=terms, y=numbers)) +
                geom_point(color='blue', size=2)+
                theme_pander()+
                ggtitle("Plotting the Geometric Sequence")+
                xlab("Terms")+
                ylab("Numbers")
ggplotly(geom_plot)


#Combine Plots

#With Patchwork
arithm_plot / geom_plot

#Combine dataframes

geom_seq_df <- geom_seq_df %>% mutate(Sequence = "Geometric")
arithm_seq_df <- arithm_seq_df %>% mutate(Sequence = "Arithmetic")

combined_sequences <- bind_rows(geom_seq_df, arithm_seq_df)

geom_arithm_plot <- ggplot(combined_sequences, aes(x=terms, y=numbers, color=Sequence)) +
  geom_point(size=2)+
  theme_pander()+
  ggtitle("Plotting the Geometric and Arithmetic Sequence")+
  xlab("Terms")+
  ylab("Numbers")+
  scale_color_manual(values=c("Arithmetic"='red', 'Geometric' = 'blue'))

ggplotly(geom_arithm_plot)
  
