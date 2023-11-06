
scanUI <- function(id) {
  ns <- NS(id)
  tagList( 

  br()  


  #?

    , fluidRow(
        column(2, align = 'center',

        prettyRadioButtons(
        inputId = ns("rad_mode"),
        label = "Choose Profiling Type", 
        choices = c("minimal", "advanced"),
        inline = TRUE, 
        status = "danger",
        fill = TRUE
        )
        )
       
       , column(3, uiOutput(ns("moreControls")))

        , column(3, align='left',
           actionButton(ns("pb_scan_btn"), "Scan Data", icon = icon("barcode", lib = "font-awesome"))  
           )

          
       , column(4, align='center',
            tags$div(downloadButton(ns("download_scan"),'Download Report'))  
            )    
    )

  #? 

  , br()

  , htmlOutput(ns("scan_html")) 

 
    
#end
  )
}

##

# server ----

scanServer <- function(id, scan_in_df) {
  moduleServer(id,
    function(input, output, session) {

    ns <- session$ns 

    #? advanced scan report
      adv_options <- c("Overview" = "O", 
                          "Variables" = "V",
                          "Interactions" = "I", 
                          "Correlations" = "C",
                          "Missing Values" = "M", 
                          "Sample" = "S")

     # more options
       output$moreControls <- renderUI({
        
        if(input$rad_mode == "advanced"){
 
         checkboxGroupInput(
          inputId = ns("check_config"),
          label = "Advanced Configuration", 
          choices = adv_options,
          selected = adv_options
        )            
        
        }

       })

  

    #?

    shinyjs::disable('download_scan')
        
    observeEvent(req(input$pb_scan_btn),{  
    shinyjs::enable('download_scan')
    }) 

      load_comp_df <- reactive({
      validate(
      need(nrow(scan_in_df())>0, "Please check dataframe!")
      )  
      scan_in_df()
      })

##?
      #? get custom configuration file
      react_config <- eventReactive(input$rad_mode, {
      
      if(input$rad_mode == "advanced"){
      paste(input$check_config, collapse='')
      } else{
      paste('OS')
      }

      })

#?
    observeEvent(input$pb_scan_btn, {

        filtered_data <- load_comp_df()

                scan_tab <- scan_data(
                        filtered_data,
                        sections = react_config(),
                        navbar = TRUE,
                        width = NULL,
                        lang = NULL,
                        locale = NULL
                    )

         export_report(
            scan_tab,
            filename = "./www/scan-small_table.html"
          )

          output$scan_html<-renderUI({
                tags$iframe(seamless="seamless", 
                  src = "./scan-small_table.html", 
                  width=1600, 
                  height=800)
          })

      
         })



       #?
        output$download_scan <- downloadHandler(
            filename = function() {
              paste0('scan', format(Sys.time(),'_%Y%m%d_%H%M%S'), '.html')
            },
            content=function(con) {
                file.copy("./www/scan-small_table.html", con)
                }
            )  


     
# end
    })
}