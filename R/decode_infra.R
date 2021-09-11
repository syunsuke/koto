##########################################################
# デコード用のサブルーチン集
#
# 「施設」列のデコード
# 作成年度による違いが無い
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-58P/L01-58P-01-02.0a.html
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-2020P/L01-2020P-01-01.0a.html
##########################################################

decode_infra <- function(bitdata, current_year){

  item <- c("水道","ガス","下水")

  readable_string_for_bitdata(bitdata, item)
}
