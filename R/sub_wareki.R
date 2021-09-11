##########################################################
# サブルーチン集
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
