#################################################################
# 地価公示のcsvデータを読み込む公開関数の定義
#################################################################

#' make R data by reading Chikakouji csv file
#'
#' @param csvfile paths of Chikakouji csv files
#'
#' @return dataframe
#' @importFrom magrittr `%>%`
#' @export
read_kouji_csv <- function(csvfile){

  lapply(csvfile, read_kouji_single_csv) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(date, `所在地コード`, `用途`, `連番`)
}

# ファイルを一つ受けとってdfを一つ返す
read_kouji_single_csv <- function(csvfile){

  print(sprintf("now reading %s", csvfile))

  ans <-
    readr::read_csv(

      # 読み込みファイルのパス
      csvfile,

      # 対象のファイルのエンコーディングはcp932
      locale = readr::locale(encoding = "cp932"),

      # 読み込み時に確実な型を確定しておく
      col_types = readr::cols(.default   = readr::col_character(),
                              `年次`     = readr::col_integer(),
                              `地積`     = readr::col_integer(),
                              `容積率`   = readr::col_integer()
      )) %>%
    dplyr::filter(!is.na(`所在地コード`))

  #return(ans)
  build_data(ans)
}
