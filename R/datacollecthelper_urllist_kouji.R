###################################################################
# 内部のヘルパー関数として利用するのみ
# 非公開
# 国土数値情報のページにある地価公示データのダウンロードURL一覧を
# 作成するルーチン
# 2021/09/13 ok.xmonad@gmail.com
###################################################################

# 地価公示データURL LIST
# ファイルデータの形式が年度により異なり
# ファイル名も異なる

# 世界測地系
# S58〜S63 2.0a 58:63
# H1〜H17  2.0a 1:17
# H18〜H31 1.0a 18:31
# R2〜     1.0a 2020:2021
# 4ヶ所のフォルダー
# /L01-[年度]P/L01-[年度]P-[都道府県番号]-[形式].zip

# 都道府県単独の全期間リストを返す
url_list_kouji <- function(pref = 27){

  # URLのフォーマットは今の所、変化が無い
  url_format <-
    "https://nlftp.mlit.go.jp/ksj/old/data/L01/L01-%02dP/L01-%02dP-%02d-%s.zip"

  ans <- NULL

  for(year in c(58:63,1:31,2020:2021)){

    # 平成17年までのファイル名
    if (year %in% c(58:63,1:17)){
      url_string <- sprintf(url_format, year, year, pref, "02.0a")

    }else{
      url_string <- sprintf(url_format, year, year, pref, "01.0a")
    }

    ans <- c(ans, url_string)

  }

  return(ans)

}

url_list_all_kouji <- function(){
  lapply(1:47, url_list_kouji) %>% unlist()
}



