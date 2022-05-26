###################################################################
#
# 共通ユティリティ関数
#
# 内部のヘルパー関数として利用するのみ
# 非公開
# 2021/09/13 ok.xmonad@gmail.com
###################################################################

##########################################################
# ビットデータ文字列を処理するヘルパー関数
# decode_xxx関数の内部で利用する
##########################################################

# 1の部分だけ、与えられた文字列に変換
# "101",c("a","b","c") → "a","c"
decode_bitdata_single <- function(bit_data, item_vector){

  logi_vector <-
    ifelse(strsplit(bit_data, "")[[1]] == "1",
           TRUE,
           FALSE)

  return(item_vector[logi_vector])
}

# 1の部分だけ、与えられた文字列に変換
# "101",c("a","b","c") → "a","c"
# ベクトル関数化し、答えはドットで繋いだ一つの文字列にする
# "101",c("a","b","c") → "a・c"
readable_string_for_bitdata <- function(bit_data, item_vector){

  sep_charactor <- "・"

  lapply(bit_data, decode_bitdata_single, item_vector) %>%
    sapply(stringr::str_flatten, sep_charactor)

}


##########################################################
# 地価公示のcsvデータは、
# 公示価格および状況ビットデータ列の経緯を毎年更新して
# 保持しているため、列の長さが可変であり、
# 対象年度の値がある列の場所を探す必要がある
#
# 当該年の評価額がある列
##########################################################

# 当該年の評価額がある列を求める関数
index_of_price <- function(base_index, current_year){

  base_index + (current_year - 1982)

}

# 当該年の移動属性ビットデータがある列
# 1983年(最も古いcsvデータ)にはこの値が無いのでNA
index_of_change_prop <- function(base_index, current_year){

  ifelse(current_year == 1983,
         NA,
         base_index + (current_year - 1982) * 2 -1)

}


##########################################################
# 標準地番号文字列を作成する
##########################################################

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


##########################################################
# 西暦数値から和暦の文字列を作成
##########################################################

make_wareki <- function(int_year){
  int_year <- as.numeric(int_year)

  if( int_year < 1873){
    ans <- int_year

  }else if(int_year %in% 1873:1911){
    ans <- sprintf("M%02d", int_year - 1873 + 6)

  }else if(int_year %in% 1912:1925){
    if(int_year == 1912){
      ans <- "M45/T01"
    }else{
      ans <- sprintf("T%02d", int_year - 1911)
    }

  }else if(int_year %in% 1926:1988){
    if(int_year == 1926){
      ans <- "T15/S01"
    }else{
      ans <- sprintf("S%02d", int_year - 1925)
    }

  }else if(int_year %in% 1989:2018){
    if(int_year == 1989){
      ans <- "S64/H01"
    }else{
      ans <- sprintf("H%02d", int_year - 1988)
    }

  }else{
    if(int_year == 2019){
      ans <- "H31/R01"
    }else{
      ans <- sprintf("R%02d", int_year - 2018)
    }

  }
  return(ans)
}

