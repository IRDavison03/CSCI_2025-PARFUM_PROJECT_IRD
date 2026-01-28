library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

tuesdata <- tidytuesdayR::tt_load('2024-12-10')

parfumo_data_clean <- tuesdata$parfumo_data_clean