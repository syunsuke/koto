#' add same id to same point
#'
#' @param df kouji dataframe
#'
#' @return data.frame
#' @export
add_pointid_col <- function(df){

  year_serial <- df$`年次` %>% unique()

  # データの年度が連続していない場合
  # エーラーにする
  for(i in min(year_serial):max(year_serial)){
    if(! i %in% year_serial){
      stop("years are not serial")
    }
  }

  # 時間計測用
  start_time <- proc.time()
  total_length <- length(year_serial)


  # 最古のデータを一番初めのデータとして準備する
  oldest_data <- df %>%
    dplyr::filter(`年次` == min(year_serial))
  oldest_data$point_id <- 1:nrow(oldest_data)
  ans_df <- oldest_data

  # 年ごとの時間計測カウンター
  debug_cunt <- 0

  # 1年毎のデータを取り出してサブルーチンで処理し結合していく
  for(i in (min(year_serial) + 1):max(year_serial)){

    # ID追加処理関数の呼び出し
    ans_df <- add_pointid_sub(ans_df,
                              df %>% dplyr::filter(`年次` == i))


    # 経過報告用
    debug_cunt <- debug_cunt +1
    past_time <- proc.time() - start_time
    print(
      sprintf("%d/%d(%.1f%%):past time %.1f sec (%.1fsec/year): total predict %s minutes",
              debug_cunt,
              total_length,
              round(debug_cunt/total_length*100,1),
              past_time[1],
              past_time[1]/debug_cunt,
              round(past_time[1] / (debug_cunt/total_length) / 60, 0)))

  }


  print(proc.time() - start_time)

  return(ans_df)
}

# 連続する2年分データについて
# 古いほうのデータのidに新しいほうのデータのidを
# 紐付ける処理
# 古いほうのデータには「point_id」列が必須
add_pointid_sub <- function(old_df, new_df){

  # 利用可能point_id
  current_point_id <- max(old_df$point_id) + 1

  # 既処理データ側の最新年データ
  last_year_data <-
    old_df %>%
    dplyr::filter(`年次` == max(old_df$`年次`))

  # 所在地コード等の文字列をkey,idを値とするhashテーブル
  old_hash <- hash::hash()
  tmp_df <- last_year_data %>%
    mutate(keys = paste0(`所在地コード`,"-",`用途`,"-",`連番`))
  old_hash[tmp_df$keys] <- tmp_df$point_id



  # 追加データの中から既存データ最新年の翌年分の
  # データを得る
  next_year_data <-
    new_df %>%
    dplyr::filter(`年次` == max(old_df$`年次`) + 1) %>%
    dplyr::mutate(keys =
                  paste0(`前年所在地コード`,
                         "-",
                         `前年用途`,
                         "-",
                         `前年連番`))

  # hash検索の準備
  keys <- next_year_data$keys
  point_id <- rep(0,nrow(next_year_data))


  # 追加データのpoint_idを前年データから探して書き込む
  for(i in 1:nrow(next_year_data)){
    if (hash::has.key(keys[i], old_hash)){
      point_id[i] <- old_hash[[keys[i]]]
    }else{
      point_id[i] <- current_point_id
      current_point_id <-  current_point_id + 1
    }
  }

  next_year_data <-
    next_year_data %>%
    dplyr::mutate(point_id = point_id) %>%
    dplyr::select(-keys)

  # 既存データに処理した(翌年ぶんデータ)を追加して返す
  dplyr::bind_rows(old_df, next_year_data)

}
