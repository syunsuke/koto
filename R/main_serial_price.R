#' Making serial price Rdata
#'
#' @param csvfile
#'
#' @return df
#' @export
make_serial_price_data <- function(csvfile){

  df <- read_all_koujidata(csvfile[1])

  # 当該データファイルの作成年
  data_year <- (df$`年次`)[1]

  base_index <-
    which(stringr::str_detect(names(df), "選定年次ビット"))

  end_index <- index_of_price(base_index, data_year)


  ans <- df %>% select(bind_id, (base_index+1):end_index)

  year_v <- paste0(1983:data_year,"-01-01")

  names(ans) <- c("bind_id",paste0(1983:data_year,"-01-01"))

  return(ans)
}
