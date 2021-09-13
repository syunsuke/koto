###################################################################
# 内部のヘルパー関数として利用するのみ
# 非公開
# zipファイルの中身のうち、パターンに合致するもののみを取り出し、
# 特定のディレクトリに保存
# 2021/09/13 ok.xmonad@gmail.com
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
