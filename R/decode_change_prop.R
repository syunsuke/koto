##########################################################
# デコード用のサブルーチン集
#
# 「移動属性」列のデコード
# 移動属性列の値は、数字が並んだ文字列
# 左端一文字が選定状況を表し、それ以外が去年からの
# 変更点を表すビットデータ文字列になっている
#
# ビットの表わす意味はデータが作成された年により
# 異なるので、パターン分けした関数
# (decode_change_prop_px)を作り
# decode_change_prop関数に統合する
#
# 新しいパターンが出来るたびに
# 新しいパターン関数を作成して、
# それをdecode_change_prop関数で統合する
##########################################################


# 統合用の関数
decode_change_prop <- function(sentei_bitdata, year){

  target <- substring(sentei_bitdata,2, nchar(sentei_bitdata)[[1]])

  ans <- ""

  if(year %in% c(1984:2017)){
    # パターン1 1984〜2014
    ans <- decode_change_prop_p1(target)

  }else if(year %in% c(2018:2030)){
    # パターン2 2015〜
    ans <- decode_change_prop_p2(target)
  }

  return(ans)
}

# パターン１
# 1984(s58)〜2017(H29)
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-58P/L01-58P-01-02.0a.html
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-29P/L01-29P-01-01.0a.html
decode_change_prop_p1 <- function(encode_str){

  item <- c("住所漢字",
            "地積",
            "利用の現況",
            "建物構造",
            "供給施設",
            "駅からの距離",
            "用途地域",
            "建ペイ率",
            "容積率")

  readable_string_for_bitdata(encode_str, item)

}

# パターン２
# 2018(H30)〜
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-30P/L01-30P-01-01.0a.html
decode_change_prop_p2 <- function(encode_str){

  item <- c("住所漢字",
            "地積",
            "利用の現況",
            "建物構造",
            "供給施設",
            "駅からの距離",
            "用途区分",
            "防火区分",
            "都市計画区分",
            "森林区分",
            "公園区分",
            "建ペイ率",
            "容積率")

  readable_string_for_bitdata(encode_str, item)

}
