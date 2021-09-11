##########################################################
# デコード用のサブルーチン集
#
# 「移動属性」列のデコード
# 移動属性ビット列の値は、数字が並んだ文字列
# 左端一文字が選定状況を表す
#
# 数字の表わす意味はデータが作成された年により
# 異なるので、パターン分けした関数
# (decode_sentei_data_px)を作り
# decode_sentei_data関数に統合する
#
# 新しいパターンが出来るたびに
# 新しいパターン関数を作成して、
# それをdecode_sentei_data関数に統合する
##########################################################

# 統合用の関数
decode_sentei_data <- function(sentei_bitdata, year){

  target <- substring(sentei_bitdata,1,1)

  ans <- ""

  if(year %in% c(1984:2017)){
    # パターン1 1984〜2017
    ans <- decode_sentei_data_p1(target)

  }else if(year %in% c(2018:2030)){
    # パターン2 2018〜
    ans <- decode_sentei_data_p2(target)
  }


  return(ans)
}


# パターン１
# 1984(s58)〜2017(H29)
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-58P/L01-58P-01-02.0a.html
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-29P/L01-29P-01-01.0a.html
decode_sentei_data_p1 <- function(encode_char){

  ifelse(
    encode_char == "1",
    "継続",
    ifelse(
      encode_char == "2",
      "基準地・標準地番号変更",
      ifelse(
        encode_char == "3",
        "選定替えで当該の選定なし",
        ifelse(
          encode_char == "4",
          "選定替えで当該年追加",
          ifelse(
            encode_char == "5",
            "新設",
            ifelse(
              encode_char == "6",
              "廃止",
              ""
            )
          )
        )
      )
    )
  )

}

# パターン２
# 2018(H30)〜
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-30P/L01-30P-01-01.0a.html
decode_sentei_data_p2 <- function(encode_char){

  ifelse(
    encode_char == "1",
    "継続",
    ifelse(
      encode_char == "2",
      "基準地・標準地番号変更",
      ifelse(
        encode_char == "4",
        "新設・選定替えで当該年追加",
        ""
      )
    )
  )
}

