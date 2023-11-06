# ui ----

#' load data ui module
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
load_dataUI <- function(id) {
  ns <- NS(id)
  tagList(      
 
      linebreaks(2)

        , fluidRow(
           column(1)
          , column(
                width = 6,
                import_ui(ns("id1"),  from = c("file", "url", "copypaste"),)
            )
          , column(1)  
          , column(
                width = 4,
                tags$b("Name:"),
                verbatimTextOutput(outputId = ns("f_name"))                
            )       
            ) 
         
  #? end
  )
}

# server ----

#' load data server module
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
load_dataServer <- function(id) {       
  moduleServer(id,
    function(input, output, session) {

   #?
   ns <- session$ns 


   imported <- import_server("id1")

    output$f_name <- renderPrint({
      imported$name()
    })

    return(reactive(imported$data()))

###
    }
  )}