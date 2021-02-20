################################################################################
## Code for: ESM 262 Assignment 2a
## Authors: Ruben Sanchez Ramirez, Becca Reynolds, Jaleise Hall
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

pb <- function(rolling = 0.015, mass_kg, gravity = 9.8, velocity, surface_area, air_density = 1.2, drag = 0.3){
  result = rolling * mass_kg * gravity * velocity + (1/2) * surface_area* air_density * drag * (velocity)^3
  return(result)
}

################################################################################
## END
################################################################################

