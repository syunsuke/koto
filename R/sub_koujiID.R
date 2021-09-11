#' Make string for koujiID
#'
#' Make string for koujiID which is Hyoujyuntachibabgou
#' by some element.
#'
#' @param youto digit string 0 or 5 or 7 etc
#' @param renban digit string for serial number
#' @param name string for name of city or town etc
#'
#' @return char
make_koujiID <- function(youto, renban, name){

  # 用途
  youto <- as.numeric(youto)

  # 連番
  renban <- as.numeric(renban)

  # 標準宅地番号の作成
  ans <-  ifelse(youto == 0,
                 sprintf("%s-%s"  , name, renban),
                 sprintf("%s%s-%s", name, youto, renban))

  return(ans)
}


