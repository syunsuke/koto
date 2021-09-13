##################################################################
# chikakokuji_osaka 作成スクリプト
# 2021/09/13 ok.xmonad@gmail.com
# 国土交通省が公開している国土数値情報データの中になる
# 地価公示データのうち大阪府にかかるものを収集して
# kotoのツールで読み込み同一地点IDを付したもの
# data.frame(tibble)として作成
##################################################################

# パスはパッケージのトップディレクトリからの相対パス

library(tidyverse)

start_time <- proc.time()


# 大阪の地価公示URLリスト
url_list <- url_list_kouji(27)


# データのダウンロード
download_by_urls(url_list, "data-raw/dl_data")


# zipファイルパスのリストを作成
zipfiles <- dir("data-raw/dl_data", full.names = TRUE)


# csvファイルの抜き出し
pickup_from_zip(zipfiles, "\\.csv", "data-raw/csvfiles")


# csvファイルの読み込み
csvfiles <- dir("data-raw/csvfiles", full.names = TRUE)
chikakouji_expdata <- read_kouji_csv(csvfiles)


chikakouji_osaka <- chikakouji_expdata %>% add_pointid_col()

# １５分程度
print(proc.time() - start_time)

usethis::use_data(chikakouji_osaka, overwrite = TRUE)
