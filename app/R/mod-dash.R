dashUI <- function(id) {
  ns <- NS(id)
  tagList(
    homeUI(ns('home_id')) 
  )
}

# server ----
dashServer <- function(id) {
  moduleServer(id,
    function(input, output, session) {
      homeServer('home_id')      
    })
  }