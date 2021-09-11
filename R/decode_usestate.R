##########################################################
# デコード用のサブルーチン集
#
# 「利用の現況」列のデコード
# 作成年度による違いが無い
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-58P/L01-58P-01-02.0a.html
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-2020P/L01-2020P-01-01.0a.html
##########################################################

decode_usestate <- function(bitdata, current_year){

  item <- c("住宅", "店舗", "事務所", "銀行",
            "旅館", "給油所", "工場", "倉庫",
            "農地", "山林", "医院", "空地",
            "作業場", "原野", "その他", "用材", "雑木" )

  readable_string_for_bitdata(bitdata, item)
}

