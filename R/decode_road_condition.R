##########################################################
# デコード用のサブルーチン集
#
# 「側道区分」列のデコード
# 2014(H26)のみデコードが必要
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-26P/L01-26P-27-01.0a.html
##########################################################

road_cond_decode <- function(road_cond_veoctor){
  tmp <- as.integer(road_cond_veoctor) + 1
  n <- 1:6
  names(n) <-   c("その他",
                  "側道",
                  "三方路",
                  "四方路",
                  "一方路・準角地",
                  "背面道")

  ans <- names(n[tmp])
  ans <- ifelse(is.na(ans),"_",ans)

  return(ans)
}
