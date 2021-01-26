## library() calls go here

# Check if the packages used in the workflow are installed and install them if they are not
RequiredPackages <- c("conflicted", 
                      "dotenv", 
                      "drake", 
                      "tidyverse",
                      "readxl",
                      "here",
                      "tools",
                      "janitor",
                      "skimr",
                      "openxlsx",
                      "ggrepel",
                      "ggplot2",
                      "cowplot")

new.packages <- RequiredPackages[!(RequiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(conflicted)
library(dotenv)
library(drake)


library(tidyverse)
library(readxl)
library(here)
library(tools)
library(janitor)
library(skimr)
library(openxlsx)


library(ggrepel)
library(ggplot2)
library(cowplot)
