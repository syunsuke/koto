##########################################################
# デコード用のサブルーチン集
#
# 内部のヘルパー関数として利用するのみ
# 非公開
# 2021/09/13 ok.xmonad@gmail.com
##########################################################

##########################################################
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


##########################################################
# 「施設」列のデコード
# 作成年度による違いが無い
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-58P/L01-58P-01-02.0a.html
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-2020P/L01-2020P-01-01.0a.html
##########################################################

decode_infra <- function(bitdata, current_year){

  item <- c("水道","ガス","下水")

  readable_string_for_bitdata(bitdata, item)
}


##########################################################
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


##########################################################
# 用途地域等に関するデータをデコードして、
# 統一した４つの列をもつdataframe(tibble)を作成する。
# 統一列は以下の４つ
#
# youto
# bouka
# tokei
# sonota
#
# 数字の表わす意味やデータ列の構造がデータの作成年により
# 異なるので、パターン分けした関数
# (decode_youto_px)を作り
# decode_youto関数に統合する
#
# 新しいパターンが出来るたびに
# 新しいパターン関数を作成して、
# それをdecode_youto関数に統合する
##########################################################

decode_youto <- function(df, data_year){

  ans <- NULL

  if(data_year %in% c(1983:2007)){
    # パターン1 1983〜2007
    ans <- decode_youto_p1(df)

  }else if(data_year %in% c(2008:2017)){
    # パターン2 2008〜2017
    ans <- decode_youto_p2(df)

  }else if(data_year %in% c(2018:2030)){
    # パターン2 2017〜
    ans <- decode_youto_p3(df)
  }

  return(ans)
}


# パターン１
# 1984(s58)〜2007(H19)
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-58P/L01-58P-01-02.0a.html
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-19P/L01-19P-01-01.0a.html
decode_youto_p1 <- function(df){

  bit_str_vector <- df$`用途地域`

  # 用途地域
  # 1〜12が用途地域
  # 19〜22が用途地域(昔の名称)
  youto <- stringr::str_c(substring(bit_str_vector, 1,12),
                 substring(bit_str_vector, 19,22))

  # 13,14 防火地域
  bouka <- substring(bit_str_vector, 13,14)

  # 15,16 区域区分
  tokei <- substring(bit_str_vector, 15,16)

  # 17, 18その他
  sonota <- substring(bit_str_vector, 17,18)


  item_youto  <- c("１低専", "２低専", "１中専", "２中専", "１住居", "２住居", "準住居", "近商", "商業",
                   "準工", "工業", "工専", "１住専", "２住専", "住居", "住居専用")

  item_bouka <- c("防火", "準防")

  item_tokei <- c("調区", "都市")

  item_sonota <- c("地森計","国定公")


  tibble::tibble(
    youto = readable_string_for_bitdata(youto, item_youto),
    bouka = readable_string_for_bitdata(bouka, item_bouka),
    tokei = readable_string_for_bitdata(tokei, item_tokei),
    sonota = readable_string_for_bitdata(sonota, item_sonota))

}

# パターン２
# 2008(H20)〜2017(H29)
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-20P/L01-20P-01-01.0a.html
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-29P/L01-29P-01-01.0a.html
decode_youto_p2 <- function(df){

  bit_str_vector <- df$`法規制`

  # 1〜12が用途地域
  youto <- substring(bit_str_vector, 1,12)

  # 13,14 防火地域
  bouka <- substring(bit_str_vector, 13,14)

  # 15〜18 区域区分
  tokei <- substring(bit_str_vector, 15,18)

  # 19〜25 その他
  sonota <- substring(bit_str_vector, 19,25)


  item_youto  <- c("１低専", "２低専", "１中専", "２中専", "１住居", "２住居", "準住居", "近商", "商業",
                   "準工", "工業", "工専")

  item_bouka <- c("防火", "準防")

  item_tokei <- c("調区", "非線引", "都計外", "準都計")

  item_sonota <- c("地森計", "国立公（普通）", "国立公（２種）", "国立公（３種）", "国定公（普通）", "国定公（２種）", "国定公（３種）")


  tibble::tibble(
    youto = readable_string_for_bitdata(youto, item_youto),
    bouka = readable_string_for_bitdata(bouka, item_bouka),
    tokei = readable_string_for_bitdata(tokei, item_tokei),
    sonota = readable_string_for_bitdata(sonota, item_sonota))

}

# パターン３
# ビットデータによる表現から
# 文字列表現に変更になったため
# 対応する列に直接データを取り込む
#
# このため、値の種類の変化はルーチンに影響しない
# 一方、将来、列が変化した場合はルーチンの変更が必要になる。
#
# 2018(H30)〜
# https://nlftp.mlit.go.jp/ksj/old/type/L01/L01-30P/L01-30P-01-01.0a.html

decode_youto_p3 <- function(df){

  ans_youto <- df$`用途区分`
  ans_bouka <- df$`防火区分`
  ans_tokei <- df$`都市計画区分`
  ans_sonota <- df$`公園区分`

  tibble::tibble(youto = ifelse(ans_youto == "_" , "", ans_youto),
                 bouka = ifelse(ans_bouka == "_" , "", ans_bouka),
                 tokei = ifelse(ans_tokei == "_" , "", ans_tokei),
                 sonota = ifelse(ans_sonota == "_" , "", ans_sonota))

}


##########################################################
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



##########################################################
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
