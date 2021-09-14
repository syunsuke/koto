#' 大阪府の地価公示価格.
#'
#' 1983年から2021年までの大阪府の地価公示価格。
#' 国土交通省が公表している国土数値情報の地価公示に関するデータを
#' R言語環境で扱いやすくするために独自に加工したものです。
#'
#' @format A data frame with 64750 rows and 45 variables:
#' \describe{
#'   \item{価格時点}{価格時点}
#'   ...
#'   \item{標準地番号}{標準地番号}
#'   \item{標準地価格}{平米単価}
#'   \item{対前年比}{パーセント差}
#'   ...
#'   \item{point_id}{同一地点番号}
#'   ...
#' }
#' @source \url{https://nlftp.mlit.go.jp/ksj/old/datalist/old_KsjTmplt-L01.html}
"chikakouji_osaka"
