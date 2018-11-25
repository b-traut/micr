#' Function to get information around MIC with data frame format
#' @param df : Data frame object. This is input to compute mic.
#' @importFrom dplyr select_if
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tibble as_tibble
#' @importFrom minerva mine
#' @importFrom purrr map
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom magrittr set_colnames
#' @importFrom stats na.omit


get_mic_df <- function(df){
  
  # Drop non-numeric columns
  df_num <- df %>% 
    na.omit() %>% 
    select_if(
      function(x) {is.numeric(x) | is.integer(x)}
    )
  
  cols <- df_num %>% names()
  
  # Validation
  if (length(names(df)) != length(cols)) {
    warning("Input df has non-numeric columns.")
  }
  
  tmp <- df_num %>% 
    mine() %>% 
    map(
      ~ as_tibble(.x) %>%
        mutate(col = cols) %>% 
        gather(key = 'col2', value = 'val', -col))
  
  res <- foreach(i = names(tmp), .combine = bind_rows) %do% {
    
    tmp[[i]] %>%
      as_tibble() %>% 
      set_colnames(c("col", "col2", "val")) %>% 
      mutate(idx = i)
    
  } %>%
    spread(key = idx, value = val) %>% 
    select(col, col2, MIC, MAS, MEV, MCN, MICR2, TIC)
  
  return(res)
  
}


#' Function to compute indexes around mic for each hierarchy.
#' @param df : Data frame object which is input of mic.
#' @param group_key : characters, which is key of hierarchy
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by_
#' @importFrom dplyr count
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom utils data


get_group_mic_df <- function(df, group_key) {
  
  gr <- df %>% 
    group_by_(group_key)
  
  cnt <- gr %>% count()
  
  mics <- gr %>% 
    filter(n() >= 2L) %>% 
    nest() %>% 
    mutate(
      mics = map(.$data, ~ get_mic_df(.x))
    ) %>% 
    select(-data) %>%
    unnest()
  
  res <- cnt %>% left_join(mics, by = group_key)
  
  return(res)
  
}

#' Utility function to call get_mic_df and get_group_mic_df
#' @param df : Data frame object which is input of mic.
#' @param group_key : If NA, this function compute mic of flat df, otherwise mic of each group. 
#' @export

get_mic <- function(df, group_key = NA) {
  
  if (is.na(group_key)) {
    out <- get_mic_df(df)
  } else {
    out <- get_group_mic_df(df, group_key)
  }
  
  return(out)
  
}
