#' Function to get information around MIC with data frame format
#' @param df : Data frame object. This is input to compute mic.
#' @param is_flat : Boolean, whether you want to get the return with plain data frame format.
#' @param ... : parameters for minerva::mine.
#' @importFrom dplyr select_if
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tibble as.tibble
#' @importFrom minerva mine
#' @importFrom purrr map
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom magrittr set_colnames
#' @importFrom magrittr %<>%
#' @export
get_mic_df <- function(df, is_flat = TRUE, ...){
  
  extract_cols <- function(x) {
    return(is.numeric(x) | is.integer(x))
  }
  
  df_selected <- df %>% select_if(extract_cols)
  
  # Confirm existance of integer or numeric columns.
  if (ncol(df_selected) == 0L){
    stop(
      "Input data frame doesn't have any integer or numeric column.")
  }
  
  cols <- df_selected %>% names()
  
  # Compute mic and transform to list of data frame
  mics <- df_selected %>% 
    mine(use = "complete.obs", ...) %>% 
    map(~ as.tibble(.x) %>% 
          mutate(col = cols) %>% 
          select(dim(.x)[2] + 1, 1:(dim(.x)[2])))
  
  if(is_flat){
    # Transform mic result to flat data frame
    mics_tidy <- mics %>% 
      map(gather, key = "col2", value = "val", -col)
    
    mics_df <- foreach(
      i = 1:length(names(mics_tidy)),
      .combine = bind_rows
    ) %do% {
      
      mics_tidy[i] %>% 
        as.data.frame() %>%
        set_colnames(c("col", "col2", "val")) %>% 
        mutate(idx = names(mics_tidy)[i])
      
    } %>% 
      select(idx, col, col2, val) %>% 
      spread(idx, val)
    
    mics <- mics_df
    
  }
  
  return(mics)
}


#' Function to compute indexes around mic for each hierarchy.
#' @param df : Data frame object which is input of mic.
#' @param group_key : characters, which is key of hierarchy
#' @param min_value : Minimum number of records in each group.
#' @param ... : parameters for minerva::mine.
#' 
#' @importFrom dplyr select_if
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr slice
#' @importFrom tidyr drop_na
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr nest
#' @importFrom tibble as.tibble
#' @importFrom minerva mine
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom magrittr set_colnames
#' @export
get_group_mic_df <- function(df, group_key, min_value, ...){
  
  extract_cols <- function(x) {
    return(is.numeric(x) | is.integer(x))
  }
  
  df_group <- df %>% select(group_key)
  df_selected <- df %>% select_if(extract_cols)
  
  # Confirm existance of integer or numeric columns. 
  if (ncol(df_selected) == 0L){
    stop(
      "Input data_frame doesn't have any integer or numeric column.")
  }
  
  # Drop NA and count records for each hierarchy.
  df_tmp <- df_group %>% 
    cbind(df_selected) %>% 
    drop_na() %>% 
    group_by_(group_key) %>% 
    filter(n() >= min_value)
  
  if(nrow(df_tmp) == 0L){
    stop("No records for input of mic")
  } 
  
  df_tmp %<>% nest()
  group_var <- df_tmp %>% select(group_key)
  
  cols <- df_selected %>% colnames()
  
  mics <- df_tmp$data %>%
    map(
      ~ as.data.frame(.) %>%
        mine(use = "complete.obs", ...)
    )
  
  idx <- names(mics[[1]])
  
  res_df <- NULL
  for (i in 1:nrow(group_var)) {
    
    tmp <- mics %>%
      pluck(i) %>% 
      map(
        ~ as.tibble(.x) %>%
          mutate(col = cols) %>%
          select(dim(.x)[2] + 1, 1:(dim(.x)[2])) %>%
          gather(key = 'col2', value = 'val', -col)) 
    
    mics_tidy_i <- foreach(
      j = 1:length(idx),
      .combine = bind_rows
    ) %do% {
      
      tmp[j] %>% 
        as.data.frame() %>%
        set_colnames(c("col", "col2", "val")) %>% 
        mutate(idx = idx[j])
      
    }
    
    key_i <- foreach(
      k = 1:nrow(mics_tidy_i),
      .combine = bind_rows
    ) %do% {
      
      group_var %>% slice(i) 
      
    }
    
    res <- bind_cols(key_i, mics_tidy_i)
    res_df %<>% bind_rows(res)
    
  }
  
  df_idx <- res_df %>% select(idx)
  df_col_val <- res_df %>% select(col, col2, val)
  df_gr <- res_df %>% select(-idx, -col, -col2, -val)
  
  res_df_ordered <- df_idx %>% 
    bind_cols(df_gr) %>% 
    bind_cols(df_col_val)
  
  return(res_df_ordered)
  
}

#' Utility function to call get_mic_df and get_group_mic_df
#' @param df : Data frame object which is input of mic.
#' @param group_key : If NA, this function compute mic of flat df, otherwise mic of each group. 
#' @param min_value : Minimum records of each group.
#' @param is_flat : When computing non-grouped MIC, if TRUE, this function returns flat df.
#' 
#' @export

get_mic <- function(df, group_key = NA, min_value = 1L, is_flat = TRUE){
  
  if(is.na(group_key)){
    out <- get_mic_df(df, is_flat)
  } else {
    out <- get_group_mic_df(df, group_key, min_value)
  }
  
  return(out)
  
}
