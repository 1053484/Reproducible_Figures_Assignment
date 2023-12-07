##
##
##Script name: Plotting.r
##
##Purpose: 
##  Functions for scatter plots for body mass and flipper length 
##


# Plot scatter plot of body mass and flipper length for all penguin species
plot_body_flipper<-function(subset){
  subset %>%
    ggplot(aes(x= body_mass_g, y=flipper_length_mm))+
    geom_point(aes(colour=species))+
    geom_smooth(method = lm, aes(colour=species))+
    labs(x="Body Mass (g)",
         y="Flipper Length (mm)")+
    theme_bw()
      
}

# Scatter plot for subset of data, with linear regression
scatter_with_correlation<-function(data, x, y){
  
    ggplot(data, aes(x= !!sym(x), y=!!sym(y)))+
    geom_point(size=1, colour="cyan4")+
    geom_smooth(method=lm, colour="cyan3")+
    labs(x="Body Mass (g)", y="Flipper Length (mm)",
         title=("Correlation between Body Mass (g) and Flipper Length (mm)"),
       caption = paste("Correlation:", round(correlation_test$estimate, 2),
                     "\n", "p-value:", format(correlation_test$p.value, 2)))+
      theme_minimal()+
    theme(plot.title = element_text(size=11,
                                    face="bold"),
          plot.title.position = "plot",
      plot.caption = element_text(size=10))
}


#Save plot as png and define size, resolution and scaling
save_plot_png<- function(plot, filename, size, res, scaling){
  agg_png(filename, width = size, 
          height= size, 
          units="cm",
          res=res,
          scaling=scaling)
  print(plot)
  dev.off()
}