# ui ----

#' pipeline ui module
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
pipelineUI <- function(id) {
  ns <- NS(id)
  tagList( 

    br(),
    
    wellPanel(style ="
    padding-top: 2% !important; padding-bottom: 1% !important; 
    background: #fafafd !important;", 
    fluidRow(    
    column(8, align = 'center',  
    uiOutput(ns("sel_col_ui"))),
    column(2, align = 'right', 
    actionButton(ns("btn_select_all"), "Select All", icon = icon("object-ungroup", lib = "font-awesome"))  

    ) ,
    column(2, align = 'left', 
    actionButton(ns("btn_deselect_all"), "Deselect All", icon = icon("rotate", lib = "font-awesome"))  
    )
    )), 

    br(),

    uiOutput(ns("col_tabs_ui"))

# box(
#       width= 12, 
#       title = 'Validation functions',
#       id = ns("pipebox"),
#       collapsible = TRUE, maximizable = TRUE,
#       closable = FALSE ,
#       uiOutput(ns("col_tabs_ui"))
#       )

  , br()

   , fluidRow(
      column(1)
    , column(2, align= 'center',
         actionButton(ns("action_lvl"), "Action level", icon = icon("sliders", lib = "font-awesome"))
      )
    ,column(2, align= 'center',
     actionButton(ns("pb_rep_btn"), "Generate Report", icon = icon("wand-magic-sparkles", lib = "font-awesome"))  
    )
    ,column(1)

    ,column(6)


  )


  
  , br()

  , htmlOutput(ns("rep_html"))
  
  # ,  box(
  #     width= 12,
  #     title = 'Pointblank report',
  #     id = ns("rep_box"),
  #     collapsible = TRUE,
  #     closable = FALSE, maximizable = TRUE,
  #     htmlOutput(ns("rep_html"))
  #       ) 
#end
  )
}

##

# server ----

#' pipeline server module
#'
#' @param id 
#' @param pipe_in_df 
#'
#' @return
#' @export
#'
#' @examples
pipelineServer <- function(id, pipe_in_df) {
  moduleServer(id,
    function(input, output, session) {

    ns <- session$ns 

    rv <- reactiveValues(warn_at = 0.4, stop_at = 0.9, notify_at = 0.5)

    #?
   # updateSelectizeInput(session = session, inputId = 'sel_col_names', choices = c(Choose = '', names(iris)), server = TRUE)

   observeEvent(input$action_lvl, {
    showModal(modalDialog(
    
          numericInput(ns("warn_at"), "Warn_at", rv$warn_at , min = 0.0, max = 1.0, step=0.01),
          numericInput(ns("stop_at"), "Stop_at", rv$stop_at, min = 0.0, max = 1.0, step=0.01),    
          numericInput(ns("notify_at"), "Notify_at", rv$notify_at, min = 0.0, max = 1.0, step=0.01) , 

      footer=tagList(
        actionButton(ns('submit'), 'Submit'),
        modalButton('cancel')
      )
    ))
  })

    

    # only store the information if the user clicks submit
    observeEvent(input$submit, {
          removeModal()
          rv$warn_at <- input$warn_at
          rv$stop_at <- input$stop_at
          rv$notify_at <- input$notify_at
        })
  

    ##
    shinyjs::disable('pb_rep_btn')
    shinyjs::disable('download_rep')
    shinyjs::disable('yaml_btn')
    shinyjs::disable('testthat_btn')
        
    #?    
    observeEvent(req(input$action_lvl),{ 
    shinyjs::enable('pb_rep_btn') 
           observeEvent(req(input$pb_rep_btn),{
              shinyjs::enable('download_rep')
              shinyjs::enable('yaml_btn')
              shinyjs::enable('testthat_btn')
           })
    }) 

    #? subset data
       user_picked_df <- eventReactive(input$sel_col_names,{ 
        pipe_in_df() %>%
        dplyr::select(all_of(input$sel_col_names))
       })

    ##?
   sel_df <- reactive({
      validate(
      need(nrow(user_picked_df())>0, "Please check dataframe!")
      )
      user_picked_df()
      })
     
    numb_cols <- reactive({
    sel_df() %>% ncol()
    }) 

    name_cols <- reactive({
    sel_df() %>% colnames()
    })

    class_cols <- reactive({
    sel_df() %>% 
              purrr::map_df(class) %>%
              slice(1L) %>% 
              flatten_chr()
    })

    #?
     output$sel_col_ui <- renderUI({  

      selectizeInput(
        ns("sel_col_names"),
        NULL,
        choices = names(pipe_in_df()),
        #selected = name_cols()[1],
        multiple = TRUE,
        width = "100%",
        options = list(
          placeholder = 'Select the columns here to apply validation functions...',
           plugins = list('drag_drop', 'remove_button')
        )
      )   

     })

      # select all
      observeEvent(input$btn_select_all, {
      updateSelectizeInput(session = session, inputId = 'sel_col_names', choices = names(pipe_in_df()), selected = names(pipe_in_df()), server = TRUE)      
      })

      # deselect all
      observeEvent(input$btn_deselect_all, {
      updateSelectizeInput(session = session, inputId = 'sel_col_names', choices = names(pipe_in_df()), selected = NULL, server = TRUE)      
      })





    #?
      
     #store the ui code in a list to use later.

        output$col_tabs_ui <- renderUI({  

        
          tabs <- purrr::map(1:numb_cols(), function(i) {

           ii <- name_cols()[i]

           tabPanel(title = stringr::str_c(ii)
            
              , linebreaks(2)

              , fluidRow(
                 column(                 
                  width = 6,
                  tags$b(glue::glue("Validation functions - ",ii, .open = "{{")),
                  bucket_list(
                    header = "The agent needs directives on what to do with the table, so, we provide validation functions. We can use as many of these as necessary for satisfactory validation testing of the table in question. Drag the items to selection bucket to validate the colunm. ",
                    group_name =  ns(stringr::str_c("bucket_list_group_", ii)),
                    orientation = "horizontal",
                    add_rank_list(
                      text = "Drag from here",
                      labels = list(                        
                        "col_vals_null()",
                        "col_vals_not_null()",
                        "rows_distinct()",
                        "rows_complete()",
                        "col_vals_lt()",
                        "col_vals_lte()",
                        "col_vals_equal()",
                        "col_vals_not_equal()",
                        "col_vals_gte()",
                        "col_vals_gt()",
                        "col_vals_regex()",
                        "col_vals_increasing()",
                        "col_vals_decreasing()",
                        "col_vals_between()",
                        "col_vals_not_between()",
                        "col_vals_in_set()",
                        "col_vals_not_in_set()",
                        "col_vals_make_set()",
                        "col_vals_make_subset()"
                      ),
                      input_id = ns(stringr::str_c("rank_list1_", ii)) #"rank_list_1"
                    ),
                    add_rank_list(
                      text = "to here (selection)",
                      labels = NULL,
                      input_id = ns(stringr::str_c("rank_list2_", ii))
                    )
                  ) 
                )   
                 #, column(1)             
                 , column(3, align= 'center'
                 , linebreaks(5)

                 , wellPanel(
                  br(),

                  fluidRow(
                  column(8,
                  textInput(ns(stringr::str_c("lt_", ii)), 'col_vals_lt()', placeholder ="value", value = NULL, width = txt_wt) 
                  ),
                   div(column(
                    awesomeCheckbox(ns(stringr::str_c("lt_na_", ii)), label = "na_pass", value = FALSE, status = "primary") , width = 4),                    
                     style = 'top: 40px; left: -16px; position:relative;')
                  )

                 , fluidRow(
                   column(8, textInput(ns(stringr::str_c("lte_", ii)), "col_vals_lte()", placeholder ="value", value = NULL, width = txt_wt)
                   ),
                   div(column(
                    awesomeCheckbox(ns(stringr::str_c("lte_na_", ii)), label = "na_pass", value = FALSE, status = "primary") , width = 4),                    
                     style = 'top: 40px; left: -16px; position:relative;')
                  )

                 , fluidRow(
                   column(8, textInput(ns(stringr::str_c("eq_", ii)), "col_vals_equal()", placeholder ="value", value = NULL, width = txt_wt)  
                   ),
                   div(column(
                    awesomeCheckbox(ns(stringr::str_c("eq_na_", ii)), label = "na_pass", value = FALSE, status = "primary") , width = 4),                    
                     style = 'top: 40px; left: -16px; position:relative;')
                  )

                 , fluidRow(
                   column(8, textInput(ns(stringr::str_c("not_eq_", ii)), "col_vals_not_equal()", placeholder ="value", value = NULL, width = txt_wt) 
                   ),
                  div(column(
                    awesomeCheckbox(ns(stringr::str_c("not_eq_na_", ii)), label = "na_pass", value = FALSE, status = "primary") , width = 4),                    
                     style = 'top: 40px; left: -16px; position:relative;')
                  )

                 , fluidRow(
                   column(8, textInput(ns(stringr::str_c("gte_", ii)), "col_vals_gte()", placeholder ="value", value = NULL, width = txt_wt) 
                   ),
                  div(column(
                    awesomeCheckbox(ns(stringr::str_c("gte_na_", ii)), label = "na_pass", value = FALSE, status = "primary") , width = 4),                    
                     style = 'top: 40px; left: -16px; position:relative;')
                  )

                 , fluidRow(
                   column(8, textInput(ns(stringr::str_c("gt_", ii)), "col_vals_gt()", placeholder ="value", value = NULL, width = txt_wt) 
                   ),
                  div(column(
                    awesomeCheckbox(ns(stringr::str_c("gt_na_", ii)), label = "na_pass", value = FALSE, status = "primary") , width = 4),                    
                     style = 'top: 40px; left: -16px; position:relative;')
                  )

                
                , textInput(ns(stringr::str_c("regex_", ii)), "col_vals_regex()", placeholder ="enter regex pattern, without quotes", value = NULL, width = txt_wt) 
                  
                #? col_vals_increasing
                , div(class='div_valid_txt', p('col_vals_increasing()')) 

                , div(fluidRow(                 
                  column(6, awesomeCheckbox(ns(stringr::str_c("incr_allow_stat_", ii)), label = "allow_stationary", value = FALSE, status = "primary")),
                  column(5, 
                  awesomeCheckbox(ns(stringr::str_c("incr_na_pass_", ii)), label = "na_pass", value = FALSE, status = "primary")                 
                  )), style = 'top: -8px; left: -12px; position:relative;') 

                #? col_vals_decreasing
                , div(class='div_valid_txt', p('col_vals_decreasing()')) 

                , div(fluidRow(                 
                  column(6, 
                  awesomeCheckbox(ns(stringr::str_c("decr_allow_stat_", ii)), label = "allow_stationary", value = FALSE, status = "primary")),
                  column(5, 
                  awesomeCheckbox(ns(stringr::str_c("decr_na_pass_", ii)), label = "na_pass", value = FALSE, status = "primary")             
                  )), style = 'top: -8px; left: -12px; position:relative;') 

                 )

                )
                
                , column(3
                
                , linebreaks(5)

                , wellPanel(

                div(class='div_btw_txt', p('col_vals_between()'))   
                   
                , div(fluidRow( 
                    column(4, align= 'right', textInput(ns(stringr::str_c("btw_l_", ii)), '_left', placeholder ="left value", value = NULL, width = '90%'))
                   ,column(4, align= 'left', 
                    textInput(ns(stringr::str_c("btw_r_", ii)), '_right', placeholder ="right value", value = NULL, width = '90%')                    
                    )
                   ,div(column(
                    awesomeCheckbox(ns(stringr::str_c("btw_na_", ii)), label = "na_pass", value = FALSE, status = "primary")                    
                    , width = 4),                    
                     style = 'top: 40px; left: -16px; position:relative;')
                  ), style = 'top: -8px; left: -24px; position:relative;')
                  
                  , div(class='div_btw_txt', p('col_vals_not_between()')) 

                  ,div(fluidRow( 
                    column(4, align= 'right', textInput(ns(stringr::str_c("notbtw_l_", ii)), '_left', placeholder ="left value", value = NULL, width = '90%'))
                   ,column(4, align= 'left', 
                    textInput(ns(stringr::str_c("notbtw_r_", ii)), '_right', placeholder ="right value", value = NULL, width = '90%') 
                    )
                   ,div(column(awesomeCheckbox(ns(stringr::str_c("notbtw_na_", ii)), label = "na_pass", value = FALSE, status = "primary"), width=4),
                    style = 'top: 40px; left: -16px; position:relative;')
                  ), style = 'top: -8px; left: -24px; position:relative;')

                  
                  ,fluidRow(
                  textInput(ns(stringr::str_c("in_set_", ii)), "col_vals_in_set()", placeholder ="set values without quotes, separated by commas", value = NULL, width = txt_wt) 
                  )

                ,fluidRow(
                  textInput(ns(stringr::str_c("notin_set_", ii)), "col_vals_not_in_set()", placeholder ="set values without quotes, separated by commas", value = NULL, width = txt_wt) 
                  )

                ,fluidRow(
                  textInput(ns(stringr::str_c("make_set_", ii)), "col_vals_make_set()", placeholder ="set values without quotes, separated by commas", value = NULL, width = txt_wt)
                  ) 

                ,fluidRow(
                  textInput(ns(stringr::str_c("make_subset_", ii)), "col_vals_make_subset()", placeholder ="set values without quotes, separated by commas", value = NULL, width = txt_wt)
                  )      


                )
                
                )             
              
                )             
               ) # end of tabpanel     

                
              })
        
       
        rlang::exec(tabsetPanel, !!!tabs) 
         
        #?
        }) 

  

    #?        

      observeEvent(input$pb_rep_btn, {

        ##? for loop 
        list_df <- list()

        for(i in 1:numb_cols()){

        ii <- name_cols()[i]
        list2 <- (glue::glue("rank_list2_", ii, .open = "{{"))

        ##
        #? warn
       #!! global action level condition
       al <- action_levels(warn_at = rv$warn_at, stop_at = rv$stop_at, notify_at = rv$notify_at)

        ##
        if(length(input[[list2]])==0){      
           #? Do the columns contain numeric/character/string/date/factor/logical values?
            if(class_cols()[i] %in% c('numeric', 'double')){
             list_df[[i]] <-  glue::glue('%>% col_is_numeric(',ii, ')', .open = "{{")  
            } else if(class_cols()[i] %in% c('integer')){
             list_df[[i]] <- glue::glue('%>% col_is_integer(',ii, ')', .open = "{{")  
            } else if(class_cols()[i] %in% c('character')){
             list_df[[i]] <- glue::glue('%>% col_is_character(',ii, ')',.open = "{{")  
            } else if(class_cols()[i] %in% c('POSIXct','POSIXt')){
             list_df[[i]] <- glue::glue('%>% col_is_posix(',ii, ')',.open = "{{")  
            }else if(class_cols()[i] %in% c('Date', 'IDate')){
             list_df[[i]] <- glue::glue('%>% col_is_date(',ii, ')',.open = "{{")  
            } else if(class_cols()[i] %in% c('logical')){
             list_df[[i]] <- glue::glue('%>% col_is_logical(',ii,')',.open = "{{")  
            } else if(class_cols()[i] %in% c('factor')){
             list_df[[i]] <- glue::glue('%>% col_is_factor(',ii,')',.open = "{{")  
            } 
        } else if(length(input[[list2]])!=0){   

            if(class_cols()[i] %in% c('numeric', 'double')){
             list_df[[i]] <-  glue::glue('%>% col_is_numeric(',ii,')',.open = "{{")  
            } else if(class_cols()[i] %in% c('integer')){
             list_df[[i]] <- glue::glue('%>% col_is_integer(',ii,')',.open = "{{")   
            } else if(class_cols()[i] %in% c('character')){
             list_df[[i]] <- glue::glue('%>% col_is_character(',ii,')',.open = "{{")  
            } else if(class_cols()[i] %in% c('POSIXct','POSIXt')){
             list_df[[i]] <- glue::glue('%>% col_is_posix(',ii,')',.open = "{{")    
            }else if(class_cols()[i] %in% c('Date', 'IDate')){
             list_df[[i]] <- glue::glue('%>% col_is_date(',ii,')',.open = "{{")  
            } else if(class_cols()[i] %in% c('logical')){
             list_df[[i]] <- glue::glue('%>% col_is_logical(',ii,')',.open = "{{")  
            } else if(class_cols()[i] %in% c('factor')){
             list_df[[i]] <- glue::glue('%>% col_is_factor(',ii,')',.open = "{{")  
            }
              

          gsub_brack <- function(x){gsub('[()]','',x)}
          lst1 <- req(input[[list2]])
          mp1 <- map(lst1, gsub_brack)

          list_dd1 <- list()
          for(j in 1:length(lst1)){
          jj <- mp1[[j]]

          #? value related conditions
          if(jj == "col_vals_lt"){
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', as.numeric(input[[str_c("lt_", ii)]]), ', na_pass =', input[[str_c("lt_na_", ii)]], ')', .open = "{{") 
          } else if(jj == "col_vals_lte") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', as.numeric(input[[str_c("lte_", ii)]]) , ', na_pass =', input[[str_c("lte_na_", ii)]], ')',.open = "{{") 
          } else if(jj == "col_vals_equal") {
            if(class_cols()[i] %in% c('numeric', 'double')){
             list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', as.numeric(input[[str_c("eq_", ii)]]) , ', na_pass =', input[[str_c("eq_na_", ii)]], ')',.open = "{{") 
            } else{
             list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', shQuote(input[[str_c("eq_", ii)]]) , ', na_pass =', input[[str_c("eq_na_", ii)]], ')',.open = "{{") 
            }
          }else if(jj == "col_vals_not_equal") {
             if(class_cols()[i] %in% c('numeric', 'double')){
             list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', as.numeric(input[[str_c("not_eq_", ii)]]) , ', na_pass =', input[[str_c("not_eq_na_", ii)]],')',.open = "{{") 
            } else{
             list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', shQuote(input[[str_c("not_eq_", ii)]]) , ', na_pass =', input[[str_c("not_eq_na_", ii)]], ')',.open = "{{")  
            }          
          }else if(jj == "col_vals_gte") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', as.numeric(input[[str_c("gte_", ii)]]) , ', na_pass =', input[[str_c("gte_na_", ii)]], ')',.open = "{{") 
          }else if(jj == "col_vals_gt") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  value =', as.numeric(input[[str_c("gt_", ii)]]) , ', na_pass =', input[[str_c("gt_na_", ii)]], ')',.open = "{{") 
          }else if(jj == "col_vals_regex") {
          pattern <- input[[str_c("regex_", ii)]]
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  regex = ', shQuote(pattern) ,')', .open = "{{") 
          }else if(jj == "col_vals_increasing") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),   allow_stationary =', (input[[str_c("incr_allow_stat_", ii)]]) ,
          ', na_pass =', input[[str_c("incr_na_pass_", ii)]] , ')', .open = "{{") 
          }else if(jj == "col_vals_decreasing") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),   allow_stationary =', (input[[str_c("decr_allow_stat_", ii)]]) ,
          ', na_pass =', input[[str_c("decr_na_pass_", ii)]] , ')', .open = "{{") 
          }else if(jj == "col_vals_between") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  left =', as.numeric(input[[str_c("btw_l_", ii)]]),
          ', right =', as.numeric(input[[str_c("btw_r_", ii)]]), ', na_pass =', input[[str_c("btw_na_", ii)]] ,')', .open = "{{") 
          }else if(jj == "col_vals_not_between") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  left =', as.numeric(input[[str_c("notbtw_l_", ii)]]),
          ', right =', as.numeric(input[[str_c("notbtw_r_", ii)]]), ', na_pass =', input[[str_c("notbtw_na_", ii)]],')', .open = "{{")        
          }else if(jj == "col_vals_in_set") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  set = c(', get_vect(input[[str_c("in_set_", ii)]]) ,') )', .open = "{{")       
          }else if(jj == "col_vals_not_in_set") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  set = c(', get_vect(input[[str_c("notin_set_", ii)]]) ,') )', .open = "{{") 
          }else if(jj == "col_vals_make_set") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  set = c(', get_vect(input[[str_c("make_set_", ii)]]) ,') )', .open = "{{") 
          }else if(jj == "col_vals_make_subset") {
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,' ),  set = c(', get_vect(input[[str_c("make_subset_", ii)]]) ,') )', .open = "{{") 
          }else {
          #? conditons => "col_vals_null()", "col_vals_not_null()", "rows_distinct()", "rows_complete()",  
          list_dd1[[j]] <- glue::glue('%>%', jj, '(columns = vars( ',ii,'))', .open = "{{")
          }

          }

          #? convert for loop to single vector
          sub_ls1 <- purrr::list_c(list_dd1)         

          list_df[[i]] <- glue::glue(glue::glue(list_df[[i]], .open = "{{"), glue::glue(paste(sub_ls1, collapse = ""), .open = "{{"), .open = "{{")
               
        }
       }


        #? convert for loop to single vector
        sub_ag1 <- purrr::list_c(list_df)
        sub_ag2 <- glue::glue(paste(sub_ag1, collapse = ""), .open = "{{")
        
         #?
         filtered_data <<- sel_df()
         agent_temp <- create_agent(tbl = ~filtered_data, actions = al)
         agent_create <- glue::glue("agent_temp", .open = "{{")
         ag_interrogate <- glue::glue("%>% interrogate()", .open = "{{")      
         comb_ag <- glue::glue(agent_create, sub_ag2, ag_interrogate, .open = "{{")   
        
        #?
        agent <- eval(rlang::parse_expr(comb_ag))
        
        # #? YAML file 
        # yaml_write(agent, filename = "agent_pb_data.yml", path = "./www/")       

        # #? testthat report
        # write_testthat_file(agent = agent, name = "pb_data", path = "./www/", overwrite = TRUE) 

        #? export html
        export_report(agent,filename = "www/agent_pb_rep.html")

        #? show html
        output$rep_html<-renderUI({
                tags$iframe(seamless="seamless", 
                  src = "./agent_pb_rep.html", 
                  width=1600, 
                  height=800)
          })

       

         })

      #? download html files
        output$download_rep <- downloadHandler(
            filename = function() {
              paste0('pipe', format(Sys.time(),'_%Y%m%d_%H%M%S'), '.html')
            },
            content=function(con1) {
                file.copy("./www/agent_pb_rep.html", con1)
                }
            )      
       
       
        #? download html         
          output$yaml_btn <- downloadHandler(
            filename = function() {
              paste0('agent_pb', format(Sys.time(),'_%Y%m%d_%H%M%S'), '.yml')
            },
            content=function(con2) {
                file.copy("./rep/agent_pb_data.yml", con2)
                }
            )

        #? testthat download
        output$testthat_btn <- downloadHandler(
            filename = function() {
              paste0('testthat_pb_data', format(Sys.time(),'_%Y%m%d_%H%M%S'), '.R')
            },
            content=function(con3) {
                file.copy("./rep/test-pb_data.R", con3)
                }
            )    



# end
    })
}