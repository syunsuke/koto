###################################################################
# 内部のヘルパー関数として利用するのみ
# 非公開
# URLのリストをとって、特定のディレクトリにダウンロードする
# ダウンロード先ディレクトリの簡易確認機能付き
# 2021/09/13 ok.xmonad@gmail.com
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
