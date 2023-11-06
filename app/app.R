
library(shiny)
library(purrr)
library(dplyr)
library(DT)
library(htmltools)
library(shinyWidgets)
library(datamods)
library(pointblank)

library(sortable)
library(stringr)
library(log4r)

# source all modules
mod_file_path <- paste0('./R')

# only specific pattern in the directory
mod_scripts <- list.files(path= mod_file_path, pattern = glob2rx("*.R")) %>% as_vector()

#
analyticscripts <- c(mod_scripts) %>% 
                   as_tibble() %>%
                   mutate(value = paste0(mod_file_path,'/',value))

# load all functions
purrr::walk(analyticscripts$value, source)

#? Shiny modules
# mod app
customApp <- function() {
  ui <- fluidPage(
   dashUI('id1')
  )
  server <- function(input, output, session) {
   dashServer('id1')
  }
  shinyApp(ui, server)  
}


customApp()