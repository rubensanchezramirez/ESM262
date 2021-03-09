##############################################################################
## Code for: Stock Size at 50 Harvest Periods Function
## Authors: Ruben Sanchez Ramirez
## Contact: Ruben Sanchez Ramirez (rubensanchezramirez@bren.ucsb.edu)
################################################################################

## Background: 
## Write a function using fisheries dynamics to determine stock size after 50 harvest periods

## n_0: the stock size at period 0
## r: is the stock growth rate
## K: is the carrying capacity for that particular stock
## E: is the effort towards catching the stock
## q: is the catchability coefficient 

################################################################################
## Function: Create function.
################################################################################

# name the function and set the inputs
stock_at_50_harvest = function(n_0,r,K,E,q){
  
  # identify the repeating variable and for stock and harvest period
  i = n_0
  t = 1
  
  # run function until the stock size drops below 0 or after 50 harvest periods
  while ((i > 0) && (t <= 50)) {
    
    # add error checking to make sure no negative inputs are used
    if(n_0 < 0 | r < 0 | K < 0 | E < 0 | q < 0) return("Conditions must all be positive!")
    
    # equation for stock size at a given harvest period
    i = (((1 + r)*(1-(i/K)))-(q*E))*i
    # add harvest periods
    t = t + 1
  }
  
  # return the stock size after either 50 harvest periods of once reached 0
  
  return(sprintf("Fish Stock (%4.3f) at Period (%d)", i, t))
  
}

