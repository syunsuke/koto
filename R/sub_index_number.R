##########################################################
# サブルーチン集
#
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
