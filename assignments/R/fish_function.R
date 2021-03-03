##############################################################################
## Code for: ESM 262 Assignment 4 (Factors)
## Authors: Ruben Sanchez Ramirez
## Contact: Ruben Sanchez Ramirez (rubensanchezramirez@bren.ucsb.edu)
################################################################################

## Background: Write a function that takes a vector of fish names and always returen three items:

  ## The most common fish
  ## The rarest fish
  ## The total number of fish

## Include an option (defaulted to FALSE) that if TRUE also returns a histogram of the number of each fish type.
## With a title giving the total number of fish caught.
## Save graph as variable.

################################################################################
## Function: Create function.
################################################################################

# name the function and set the inputs
fish_function <- function(fish_vector, print_plot)
{
  
  # on the chance that a data frame is entered, convert it into a vector
  fish_vector = unlist(fish_vector)
  # make the vector as factor in order to retrieve the min and max values
  fish_vector = as.factor(fish_vector)
  # create a factor that returns the name of the most commonly caught fish
  max_fish = names(which.max(summary(fish_vector)))
  # create a factor that returns the count of the most commonly caught fish
  max_count = max(summary(fish_vector))
  # create a factor that returns the name of the least commonly caught fish
  min_fish = names(which.min(summary(fish_vector)))
  # create a factor that returns the count of the least commonly caught fish
  min_count = min(summary(fish_vector))
  # create a factor that returns the total number of fish caught
  total_fish = sum(summary(fish_vector))
  
  # create a histogram of the number of each fish type
  # with a title giving the total number of fish caught
  
  # Create a plot title that starts with a string of words and ends with the sum of all fish caught
  plottitle=sprintf("The Total Number of Fish Caught = %0.0f", total_fish)
  # create and save the histogram plot as a variable
  fish_plot = ggplot(data.frame(fish_vector), 
                     aes(fish_vector, fill = fish_vector)) + 
    geom_histogram(stat = "count") +
    # Reference the plot title created and add labels for x and y axis
    labs(title = plottitle,
         x = "Fish Species",
         y = "Count") +
    # Add plot theme and text elements
    theme_minimal() +
    theme(text = element_text(family = "serif"),
          plot.title = element_text(size=14, face="bold"),
          plot.subtitle = element_text(size = 9, face = "bold.italic"),
          axis.title.x = element_text(size=10, face="bold", vjust = -1),
          axis.title.y = element_text(size=10, face="bold", vjust = 2),
          strip.text.x = element_text(size = 10, face = "bold.italic"),
          legend.title = element_blank(),
          legend.position = "right")
  
  # Make a list of the values that will be returned as outputs to this function
  fish_list = list(Max = sprintf("The most commonly caught fish (%s). Count = (%d)", max_fish, max_count),
                   Min = sprintf("The least commonly caught fish (%s). Count = (%d)", min_fish, min_count),
                   Total = sprintf("The total number of fish caught (%0.0f)", total_fish),
                   # create an option where the default is to not print the plot
                   if(missing(print_plot) || print_plot == FALSE) {
                     # Null
                   } else {
                     Plot = fish_plot
                   }
                   )
  
  # Make the function return the list
  return(fish_list)
  # Add print for situations where plot is present
  print(fish_list)
  
}




