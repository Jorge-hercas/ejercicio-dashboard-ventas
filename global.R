


library(shiny)
library(shinyjs)    
library(bs4Dash)
library(echarts4r)
library(reactable)
library(reactablefmtr)
library(mapboxer)
library(dplyr)
library(dtplyr, warn.conflicts = FALSE)
library(purrr)
library(tidyr)
library(lubridate)
library(prophet)
library(scales)
library(RColorBrewer)
library(htmlwidgets)
library(shinyWidgets)
library(openxlsx)
library(shinymanager)

# Options
opts <- list(
  `actions-box` = TRUE,
  `live-search`=TRUE)

source("data.R")
source("sidebar.R")
source("controlbar.R")
source("body.R")





 