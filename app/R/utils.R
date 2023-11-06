# ----

#' extend custom daterange
#'
#' @param var 
#'
#' @return
#' @export
#'
#' @examples
extend_cust_range <- function(var) {
  if (hour(var) == 23 && minute(var) == 59) {
    var + (lubridate::days(1) + lubridate::minutes(1))    
  } else {
    var + lubridate::days(1)
    
  }
}


# ----

#? UI modifications

#' Navbar tabpanel ui
#'
#' @param tabName 
#' @param ... 
#' @param icon 
#'
#' @return
#' @export
#'
#' @examples
navbarTab <- function(tabName, ..., icon = NULL) {
    tags$li(
        class = "nav-item",
        tags$a(
            class = "nav-link",
            id = paste0("tab-", tabName),
            href = paste0("#shiny-tab-", tabName),
            `data-toggle` = "tab",
            `data-value` = tabName,
            icon,
            tags$p(...)
        )
    )
}

#' Navbar menu ui
#'
#' @param ... 
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
navbarMenu <- function(..., id = NULL) {
    if (is.null(id)) id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
    
    tags$ul(
        class = "navbar-nav dropdown sidebar-menu", 
        role = "menu",
        ...,
        div(
            id = id,
            class = "sidebarMenuSelectedTabItem",
            `data-value` = "null"            
        )
    )
}



#' linebreak function to avoid repeat use of br()
#'
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
linebreaks <- function(n){
    HTML(strrep(br(), n))
    }

txt_wt <- "90%"

#' Get words separated by comma
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_vect <- function(x){
      words <- str_trim(unlist(strsplit(x, ","))) 
      paste(sQuote(words, q = FALSE), collapse = ", ")
}

#? add double quotes to vector


#' add quotes to string
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
add_sQuote <- function(x){
         paste(sQuote(x, q = FALSE))
}


# ----

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)


#?

#' Title
#'
#' @param var 
#'
#' @return
#' @export
#'
#' @examples
assert_count_df <- function(var){
  msg <- paste(substitute(var), "must be a count dataframe (output of dplyr::count)")
  assertthat::assert_that(is.data.frame(var),
                          assertthat::has_name(var, "n"),
                          msg = msg)

  n_col <- var$n
  assertthat::assert_that(is.numeric(n_col), msg = msg)
}


#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
all_non_n_cols_to_char <- function(df){
  df %>%
    dplyr::mutate(dplyr::across(!matches("^n$"), as.character))
}

#?
#' Title
#'
#' @param df 
#' @param col_name 
#'
#' @return
#' @export
#'
#' @examples
fun_col_len <- function(df=dat, col_name='col1'){
        col_var = sym(col_name)
        df %>%
        drop_na(!!col_var) %>%
        distinct(!!col_var) %>%
        pull(!!col_var) %>%
        length()
}

#? function to convert char to numeric
#' Title
#'
#' @param x 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
func_numb <- function(x, n){
    x <- as.numeric(as.character(x))
    x <- round(x, n)
    return(x)
}

###
## from datamods
#' Title
#'
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || x == ""
}

#' @importFrom data.table .SD
dropListColumns <- function(x) {
  type_col <- vapply(
    X = x,
    FUN = typeof,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  if (inherits(x, "data.table")) {
    x[, .SD, .SDcols = type_col != "list"]
  } else {
    x[, type_col != "list", drop = FALSE]
  }
}


#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}




#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
as_out <- function(x, return_class = c("data.frame", "data.table", "tbl_df", "raw")) {
  if (is.null(x))
    return(NULL)
  return_class <- match.arg(return_class)
  if (identical(return_class, "raw"))
    return(x)
  is_sf <- inherits(x, "sf")
  x <- if (identical(return_class, "data.frame")) {
    as.data.frame(x)
  } else if (identical(return_class, "data.table")) {
    as.data.table(x)
  } else {
    as_tibble(x)
  }
  if (is_sf)
    class(x) <- c("sf", class(x))
  return(x)
}


#' Title
#'
#' @param bytes 
#'
#' @return
#' @export
#'
#' @examples
genId <- function(bytes = 12) {
  paste(format(as.hexmode(sample(256, bytes, replace = TRUE) - 1), width = 2), collapse = "")
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
makeId <- function(x) {
  if (length(x) < 1)
    return(NULL)
  x <- as.character(x)
  x <- lapply(X = x, FUN = function(y) {
    paste(as.character(charToRaw(y)), collapse = "")
  })
  x <- unlist(x, use.names = FALSE)
  make.unique(x, sep = "_")
}


#' Title
#'
#' @param x 
#' @param table 
#'
#' @return
#' @export
#'
#' @examples
`%inT%` <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(TRUE, length(x))
  }
}



#' Title
#'
#' @param x 
#' @param table 
#'
#' @return
#' @export
#'
#' @examples
`%inF%` <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(FALSE, length(x))
  }
}


# ----

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
inline <- function (x) {
tags$div(style="display:inline-block;", x)
}
