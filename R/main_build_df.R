#########################################################
# データ作成のメインルーチン
#
# 地価公示に関するRデータの統一形式をここで定義
#
# csvから読み込んだdfを整理して
# 年度間の差異を調整し、
# 統一的で人が読むことが出来るデータとして
# 作成しなおしたデータフレームを返す関数
#
# データ読み込み時に行なう作業と連携すべきことに
# 注意する必要がある
#########################################################

build_data <- function(df){

  ##########################################
  # 列整理の前処理
  ##########################################

  # 当該データファイルの作成年
  data_year <- (df$`年次`)[1]


  # 選定年次ビット列のインデックスナンバー
  # この列が位置の基準になる
  base_index <-
    which(stringr::str_detect(names(df), "選定年次ビット"))


  # 移動属性ビット文字列の抜き出し
  # change_prop
  change_prop_vector <- NULL
  if(is.na(index_of_change_prop(base_index, data_year))){
    # S58年のデータ列が無い場合の処理
    change_prop_vector = rep(NA,nrow(df))
  }else{
    change_prop_vector <- df[[index_of_change_prop(base_index, data_year)]]
  }


  ##########################################
  # 列が無いものに関する処理
  ##########################################

  # 建蔽率列を統一
  if("建蔽率" %in% names(df)){
    df$`建ぺい率` <- df$`建蔽率`
  }
  df$`建ぺい率` <- df$`建ぺい率` %>% as.integer()


  # 列に駅名が無い場合
  if( ! "駅名" %in% names(df)){
    df$`駅名` <- ""
  }

  # 道路に関する項目がない場合
  if( ! "前面道路の幅員" %in% names(df)){
    df$`前面道路区分` <- ""
    df$`前面道路の方位区分` <- ""
    df$`前面道路の幅員` <- ""
    df$`前面道路の駅前区分` <- ""
    df$`前面道路の舗装状況` <- ""
    df$`側道区分` <- ""
    df$`側道方位区分` <- ""
    df$`交通施設との近接区分` <- ""
  }

  # 列に周辺の利用状況が無い場合
  if( ! "周辺の土地の利用の現況" %in% names(df)){
    df$`周辺の土地の利用の現況` <- ""
  }

  # 2014(H26)のみ前面道路区分の仕様が異なることに注意
  # v2.2用にroad_cond_decode関数で補正
  if( data_year %in% c(2014)){
    df$`前面道路区分` <- road_cond_decode(df$`前面道路区分`)
  }


  ##########################################
  # 用途地域列の処理をして整理して
  # 統一したものを結合してしまう
  ##########################################
  df <-
    dplyr::bind_cols(df, decode_youto(df, data_year))


  ##########################################
  # 既存データに付け加えと変更
  # 列名に日本語が含んでいることでのトリック
  ##########################################

  if(data_year %in% c(1983:2021)){
    # パターン1 1984〜2021

    # 都道府県
    df$`都道府県` <-
      df$`住居表示` %>%
      stringr::str_split("　",simplify = T) %>% .[,1]

    # 住居表示
    df$`住居表示` <-
      df$`住居表示` %>%
      stringr::str_split("　",simplify = T) %>% .[,2]

    # 所在並びに地番
    df$`所在並びに地番` <- ""

  }else{
    # パターン2 2022〜
    # 都道府県
    df$`都道府県` <-
      df$`所在並びに地番` %>%
      stringr::str_split("　",simplify = T) %>% .[,1]

    df$`所在並びに地番` <-
      df$`所在並びに地番` %>%
      stringr::str_split("　",simplify = T) %>% .[,2]
  }

  # 道路の幅員
  df$`前面道路の幅員` <-
    as.numeric(df$`前面道路の幅員`) / 10


  ##########################################
  # 加工したデータ列を
  # 新規データフレームとして作成
  # mutateより処理が早い？
  ##########################################

  # 当該年度価格
  price <-
    df[[index_of_price(base_index, data_year)]] %>%
    as.integer()

  # 前年度価格
  lastyear_price <- NULL
  if(data_year == 1983){
    lastyear_price <-
      rep(NA, nrow(df))
  }else{
    lastyear_price <-
      df[[index_of_price(base_index, data_year) - 1]] %>%
      as.integer()
  }

  # 対前年比
  mod_rate <-
    round(price / lastyear_price * 100 - 100 ,1)


  # 標準地番号
  std_number <-
    make_koujiID(df$`用途`, df$`連番`, df$`市区町村名`)


  # 変更点選定替え情報具体データ
  point_status <-
    decode_sentei_data(change_prop_vector, data_year)


  # 変更点具体的データ
  change_contents <-
    decode_change_prop(change_prop_vector, data_year)


  # 利用の現況具体データ
  genkyo <-
    decode_usestate(df$`利用の現況`)


  # 施設の具体データ
  sisetu <-
    decode_infra(df$`施設`)


  # 価格時点データ
  date <-
    as.Date(sprintf("%d-01-01", data_year))

  # 価格時点和暦データ
  wareki = df$`年次` %>% sapply(make_wareki)
  #wareki = ""


  # 位置情報
  long <-  as.double(df$`経度`) / 3600
  lat <-  as.double(df$`緯度`) / 3600


  ##########################################
  # 新規列から新しいデータフレームを作成
  ##########################################

  new_df <-
    tibble::tibble(
      date,
      wareki,
      std_number,
      price,
      lastyear_price,
      mod_rate,
      point_status,
      change_contents,
      genkyo,
      long,
      lat,
      sisetu
    )

  ##########################################
  # 既存データフレームから必要列を抜き出し
  ##########################################

  select_df <-
    df %>%
    dplyr::select(
      `都道府県`,
      `所在地コード`:`市区町村名`,
      `所在並びに地番`,
      `住居表示`,
      `行政`,
      `地積`,
      `利用状況表示`,
      `建物構造`,
      `建ぺい率`,
      `容積率`,
      `駅名`,
      `駅距離`,
      `前面道路区分`,
      `前面道路の方位区分`,
      `前面道路の幅員`,
      `前面道路の駅前区分`,
      `前面道路の舗装状況`,
      `側道区分`,
      `側道方位区分`,
      `交通施設との近接区分`,
      `周辺の土地の利用の現況`,
      youto,
      bouka,
      tokei,
      sonota,
      bind_id
    )

  ##########################################
  # 結合
  ##########################################

  ans <-
    dplyr::bind_cols(select_df,new_df) %>%
    dplyr::select(
      date,
      wareki,
      std_number,
      price,
      lastyear_price,
      mod_rate,
      `都道府県`,
      `所在地コード`:`地積`,
      genkyo,
      `利用状況表示`,
      `建物構造`,
      `周辺の土地の利用の現況`,
      sisetu,
      youto,
      bouka,
      tokei,
      sonota,
      建ぺい率,
      容積率,
      `駅名`,
      `駅距離`,
      `前面道路区分`:`交通施設との近接区分`,
      long, lat,
      point_status,
      change_contents,
      bind_id

    )

  return(ans)

}
