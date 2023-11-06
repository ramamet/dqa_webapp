# ui ----

#' select data ui module
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
select_dataUI <- function(id) {
  ns <- NS(id)
  tagList(     
  fluidRow(
         column(2,  linebreaks(10),
                
                fluidRow(
                switchInput(inputId = ns("chk_col_clean"),  label = "Clean Column Names", labelWidth = "220px", width = "95%") 
                ),

                fluidRow(
                switchInput(inputId = ns("chk_empty_na"),  label = "Convert Empty strings to NA", labelWidth = "220px", width = "95%")  
                ),

                fluidRow(
                switchInput(inputId = ns("get_sample_switch"),  label = "Get sample (0.1% rows) from a table", labelWidth = "220px", width = "95%") 
                )
                
                ),
         column(
          width = 8,
          update_variables_ui(ns("vars"))
          ),
          column(2) 
          )
   
 #? end
  )
}


# server ----

#' select data server module
#'
#' @param id 
#' @param select_df 
#'
#' @return
#' @export
#'
#' @examples
select_dataServer <- function(id, select_df) {
  moduleServer(id,
    function(input, output, session) { 

     ns <- session$ns 
     
    # clean_df <- eventReactive(c(input$chk_col_clean, input$chk_empty_na),{
    # if((input$chk_col_clean == FALSE) && (input$chk_empty_na == FALSE)){        
    #   select_df()
    # } else if((input$chk_col_clean == TRUE) && (input$chk_empty_na == FALSE)) {
    #   select_df() %>%
    #   janitor::clean_names()
    # } else if((input$chk_col_clean == FALSE) && (input$chk_empty_na == TRUE)) {
    #   select_df() %>%
    #   dplyr::mutate_if(is.character, ~na_if(., ''))
    # } else if((input$chk_col_clean == TRUE) && (input$chk_empty_na == TRUE)) {
    #   select_df() %>%
    #   janitor::clean_names() %>%
    #   dplyr::mutate_if(is.character, ~na_if(., ''))
    # }else {
    #   select_df()
    # }
    # })

    #? three permutation conditions
    clean_df <- eventReactive(c(input$chk_col_clean, input$chk_empty_na, input$get_sample_switch),{
        if((input$chk_col_clean == FALSE) && (input$chk_empty_na == FALSE) && (input$get_sample_switch == FALSE)){        
          select_df()
        } else if((input$chk_col_clean == TRUE) && (input$chk_empty_na == FALSE) && (input$get_sample_switch == FALSE)) {
          select_df() %>%
          janitor::clean_names()
        } else if((input$chk_col_clean == FALSE) && (input$chk_empty_na == TRUE) && (input$get_sample_switch == FALSE)) {
          select_df() %>%
          dplyr::mutate_if(is.character, ~na_if(., ''))
        } else if((input$chk_col_clean == FALSE) && (input$chk_empty_na == FALSE) && (input$get_sample_switch == TRUE)) {
          select_df() %>%
          dplyr::sample_frac(0.1)
        }else if((input$chk_col_clean == TRUE) && (input$chk_empty_na == TRUE) && (input$get_sample_switch == FALSE)) {
          select_df() %>%
          janitor::clean_names() %>%
          dplyr::mutate_if(is.character, ~na_if(., ''))
        } else if((input$chk_col_clean == TRUE) && (input$chk_empty_na == FALSE) && (input$get_sample_switch == TRUE)) {
          select_df() %>%
          janitor::clean_names() %>%
          dplyr::sample_frac(0.1)
        }else if((input$chk_col_clean == FALSE) && (input$chk_empty_na == TRUE) && (input$get_sample_switch == TRUE)) {
          select_df() %>%
          dplyr::mutate_if(is.character, ~na_if(., '')) %>%
          dplyr::sample_frac(0.1)
        }else if((input$chk_col_clean == TRUE) && (input$chk_empty_na == TRUE) && (input$get_sample_switch == TRUE)) {
          select_df() %>%
          janitor::clean_names() %>%
          dplyr::mutate_if(is.character, ~na_if(., '')) %>%
          dplyr::sample_frac(0.1)
        }else {
          select_df()
        }
    })

    #?
    # sample_df <- eventReactive(c(input$get_sample_switch),{

    # req(select_df())  

    # if(input$get_sample_switch == FALSE){        
    #   select_df()
    # } else{
    #   select_df() %>% dplyr::sample_frac(0.1)
    # }

    # })

    
    updated_data <- update_variables_server(       
        id = "vars",
        data = reactive(clean_df())
       )
   
  return(reactive(updated_data()))

   
# end
    })
}