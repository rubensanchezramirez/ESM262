################################################################################
## Code for: ESM 262 Assignment 2a
## Authors: Ruben Sanchez Ramirez
## Contact: Ruben Sanchez Ramirez (rubensanchezramirez@bren.ucsb.edu)
################################################################################

## Background: Power Required to Keep Car Moving Equation

## One of the equations used to compute automobile fuel efficiency is the power required to keep a car moving at a given speed.
## This equation is as follows:
## Pb = crolling * m * g * V + 1/2 * A *P_air *c_drag * V^3
## Where crolling and cdrag are rolling and aerodynamic resistive coefficients, typical values are 0.0015 and 0.3, respectively.
  ## V: is vehicle speed (assuming no headwind) in m/s (or mps)
  ## m: is vehicle mass in kg
  ## A: is surface area of car (m^2)
  ## g: is acceleration due to gravity (9.8m/s^2)
  ## p_air: is the density of air (1.2kg/m^3)
  ## Pb: is power in Watts

################################################################################
## Function: Create function to comput power, given m, A, and a rage of different highway speeds.
################################################################################

# change in the inputs for this one to include an input for rolling coefficient, mass, surface area, and velocity

power_watts <- function(rolling, mass_kg, gravity = 9.8, velocity, surface_area, air_density = 1.2, drag = 0.3){
  
  # check for error so that if user gives garbage data, the function does not compute inaccurate power
  
  # make sure we have the same number of mass_kg and surface_area for each car so that they are matched
  if(length(mass_kg) != length(surface_area)) return("number of vehical mass is diffrent from number of surface areas")
  # make sure inputs are positive and if not exit the function prematurely
  # positive mass input:
  mass_kg = ifelse((mass_kg < 0), return("mass must be greater than zero"), mass_kg)
  # positive surface area input:
  surface_area = ifelse((surface_area < 0), return("surface area must be greater than zero"), surface_area)
  # positive velocity input:
  velocity = ifelse((velocity < 0), return("velocity must be greater than zero"), velocity)
  
  result = rolling * mass_kg * gravity * velocity + (1/2) * surface_area* air_density * drag * (velocity)^3
  return(result)
}

################################################################################
## END
################################################################################