#' Fuzzy lookup. Soft search through a lookup table.
#'
#' Runs str_detect(string, regex(query)) under the hood
#'
#' @param lookup description
#' @param search_term description
#' @param replace_term description
#' @param .df description
#' @param search_col description
#' @param new_col description
#' @param .default description
#' @param ignore.case dscp
#'
#' @return A data frame / tibble.
#' @examples

#' require(tibble)
#' mtcars_tbl <- tibble::as_tibble(mtcars,rownames='model')
#'
#' lookup_tbl <- tibble::tribble(~key1, ~key2,
#'                       'mazda rx4', 'Mazda RX4',
#'                     # 'Merc', 'Mercedes',
#'                       'merc', 'Mercedes',
#'                       'HORNET','Hornet',
#'                       'hornet','Hornet')
#'
#' fuzzy_lookup(lookup = lookup_tbl,
#'              #lookup = lookup_tbl %>% dplyr::slice(-1),
#'              search_term=key1, replace_term=key2,
#'              .df=mtcars_tbl, search_col='model', new_col='model_clean',
#'              .default = wt, ignore.case=T)
#'
#' fuzzy_lookup(lookup = lookup_tbl %>% dplyr::slice(-1),
#'              search_term=key1, replace_term=key2,
#'              .df=mtcars_tbl, search_col='model', new_col='model_clean'
#' )
#'
#' @export
fuzzy_lookup <- function(lookup, search_term, replace_term,
                         .df, search_col, new_col,
                         .default='other', ignore.case=F){

  lookup <- lookup %>%
    dplyr::select(x1={{search_term}}, x2={{replace_term}}) #only allow 2 cols & constrain colnames.

  df <- .df %>% dplyr::distinct() %>% tibble::rownames_to_column(var = 'rowdummy')
  df <-  df %>% dplyr::mutate(rowdummy=as.numeric(rowdummy))


  #NB pmap_dfr() superseded by pmap ...> list_rbind()
  res1 <- purrr::pmap(lookup, function(x1, x2, ...){

    df %>% dplyr::filter(stringr::str_detect(.data[[search_col]],
                        stringr::regex(x1, ignore_case = ignore.case))) %>%
      dplyr::mutate({{new_col}} := x2) 
    
  }) %>% purrr::list_rbind()

  message(glue::glue('Updated {nrow(res1)} rows'))


  dfmod <- df %>% dplyr::mutate({{new_col}} := as.character({{.default}}))
  #print(dfmod)

  res2 <- dplyr::anti_join(dfmod,  res1 , by='rowdummy')

  #Preserve original order:

  #res <- left_join(distinct(df), res3) # , by={{search_col}})

  res <- dplyr::bind_rows(res1,res2) %>% dplyr::arrange(rowdummy) %>%
    dplyr::select(-rowdummy)

  if(nrow(res) != nrow(.df)){
    warning('Returned data row number is different to input data row number. Run distinct(df), and then check for redundancy in the lookup table search terms. Use case_when() to control the heirarchy for redundant strings.')
  }

  return(res)


}
