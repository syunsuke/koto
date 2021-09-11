##########################################################
# サブルーチン集
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
