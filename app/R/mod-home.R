# ui ----

#' homepage ui module
#'
#' @param id cll
#'
#' @return
#' @export
#'
#' @examples
homeUI <- function(id) {
  ns <- NS(id)
 
  
tagList(
  tabsetPanel(
      tabPanel("Data", 
      load_dataUI(ns('mod_df'))
      ),
      tabPanel("Select", 
      select_dataUI(ns('select1'))
      ),
      tabPanel("Scan",  
      scanUI(ns('scan1'))
      ),
      tabPanel("Validate",  
      pipelineUI(ns('pipe1'))
      )
      )
)


 }


# server ----

#' homepage module serverside
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
 homeServer <- function(id) {
  moduleServer(id,
    function(input, output, session) {
    
    load_df <- load_dataServer('mod_df')  
    select_out <- select_dataServer('select1', select_df = load_df)
    scanServer('scan1', scan_in_df = select_out)
    pipelineServer('pipe1', pipe_in_df = select_out)


    }
    )}

##
