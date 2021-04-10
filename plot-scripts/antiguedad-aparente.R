# load necessary libraries
library(readxl)  # load dataset
library(ggplot2) # plots
library(dplyr)   # piping 
library(tibble)  # view dataset
library(ggtext)  # add html & css
library(extrafont) # add fonts

# load dataset
antiguedad_aparente <- read_excel("data/1-B Antiguedad aparente.xlsx")
view(antiguedad_aparente)