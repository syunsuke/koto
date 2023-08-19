###################################################################
#
# データ収集関連のヘルパー関数
#
# 内部のヘルパー関数として利用するのみ
# 非公開
# 2021/09/13 ok.xmonad@gmail.com
###################################################################


###################################################################
# download
# URLのリストをとって、特定のディレクトリにダウンロードする
# ダウンロード先ディレクトリの簡易確認機能付き
###################################################################

# URLのリストをとってデータダウンロードする
download_by_urls <- function(urls, dest_dir = "."){

  # dest_dirの確保
  if(!confirm_dir_path(dest_dir)){
    warning("dest_dir is not exist and is not made.")
    return()
  }

  # エラーURLを確認できるようにする
  e_url = vector()

  for (i in seq_along(urls) ){
    dest_path = paste0(dest_dir, "/", basename(urls[i]))

    Sys.sleep(0.3)

    tryCatch({
      download.file(url = urls[i], destfile = dest_path)
    },
    error = function(e){
      message("Error!!")
      message(e)
      message("")
      e_url <<- c(e_url, urls[i])
    }
    )
  }

  if(length(e_url)>0){
    message("following urls are error.")
    print(e_url)
  }

}

# ダウンロード先のディレクトリの確保のサブルーチン
confirm_dir_path <- function(path_string){

  if(dir.exists(paths = path_string)){
    return(TRUE)
  }

  message(sprintf("%s is not exist.", path_string))
  ans <- readline("Do you create this directory? y/n : ")

  if (ans == "y"){
    dir.create(path_string, recursive = TRUE)
    message((sprintf("%s has been created.", path_string)))
    return(TRUE)
  }

  return(FALSE)
}



###################################################################
# unzip
# zipファイルの中身のうち、パターンに合致するもののみを取り出し、
# 特定のディレクトリに保存
###################################################################

# zipファイルの中から特定の(csv)ファイルだけを抜き出して
# 特定のディレクトリに入れるためのunzip関数のラッパー
pickup_from_single_zip <- function(zipfile, pattern = "", dest_dir = "."){
  file_list <-
    unzip(zipfile, list = TRUE) %>%
    .$Name %>%
    stringr::str_subset(pattern)

  unzip(zipfile = zipfile, files = file_list, junkpaths = TRUE, exdir = dest_dir)
}

pickup_from_zip <- function(zipfiles, pattern = "", dest_dir = "."){

  lapply(zipfiles, pickup_from_single_zip, pattern, dest_dir)

}


###################################################################
# download list
# 国土数値情報のページにある地価公示データのダウンロードURL一覧を
# 作成するルーチン
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

  for(year in c(58:63,1:31,2020:2023)){

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


