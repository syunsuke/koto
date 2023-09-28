# koto
this is tool for converting kouji data into R frame data

## install

```{r}
devtools::install_github("syunsuke/koto")
```

# How to use

## データをダウンロードする

国土数値情報ダウンロードサービスから、
地価公示のcsvデータをダウンロードします。

https://nlftp.mlit.go.jp/ksj/old/old_datalist.html

旧統一フォーマット形式
テキスト
地価
地価公示（ポイント）

データは都道府県毎と全国について、年毎に用意されている。

## Rデータの作成の考え方

年毎のcsvデータには、その年の標準地について過去の価格も付随しています。
しかし、価格以外の各項目について変更があった場合、その具体的な内容は把握できません。
また、過去に選定替えされたポイントについても把握することができません。

### 方法１

以上から、すべての地価公示データを正確に把握するためには、
全年のデータを取り込む必要があります。

次に、毎年の各標準地のデータは「標準宅地番号」が付されていますが、
同一地点でも年によって「標準宅地番号」が異なったり、
新規地点で同じ標準地番号が別の地点を指す場合もあります。

そこで、標準地の変遷を把握しやすくするために、
標準地の同一性を表すIDを作成する必要があります。
これは、毎年のデータ内に去年の標準宅地番号があるため、
すべてのデータについてこれを辿って、IDを割り振ることができます。
しかし、全国について全年分のデータの処理を行う場合、
まず、すべてのデータをダウンロードするための時間と手間、
また大量のデータを処理する時間がややかかります。
更に、全国、全年分のRデータ自体の容量も大きくなります。




### 方法２

一方で、正確にすべての標準地が必要ではなくて、
最新、あるいは、必要な年の標準地についての内容と
その標準地の過去の価格推移だけでよいならば、
CSVをRに取り込む処理も速く、Rデータの容量も小さく済みます。


## 具体的なコード

### 方法１について

全てのデータファイルを処理する場合、
まず、データファイルをダウンロードして、解凍する等の処理も必要になります。
それらについては、パッケージ内にヘルパー関数があるので、
data-rawディレクトリ内のDATASET.Rファイルを参照してください。


```{r}
# 大阪の地価公示URLリスト
url_list <- url_list_kouji(27)


# データのダウンロード
download_by_urls(url_list, "data-raw/dl_data")


# zipファイルパスのリストを作成
zipfiles <- dir("data-raw/dl_data", full.names = TRUE)


# csvファイルの抜き出し
pickup_from_zip(zipfiles, "\\.csv", "data-raw/csvfiles")

```

ファイルをダウンロードして解答できたら、
次の処理を行います。

```{r}
# csvファイルの読み込み
csvfiles <- dir("data-raw/csvfiles", full.names = TRUE)
chikakouji_expdata <- read_kouji_csv(csvfiles)

# 同一地点IDの割り振り処理
chikakouji_osaka <-
  chikakouji_expdata %>%
  add_pointid_col()
```

### 方法２について

最新年の全国データを一つダウンロードして解凍し、
そのcsvファイルを次のように処理します。

```{r}
# 単年度分の地価公示データ
main_data <- read_kouji_csv("L01-2022P-2K.csv")

# 各標準地の過去の価格一覧データ
serial_price_data <- make_serial_price_data("L01-2022P-2K.csv")
```

main_dataとserial_price_dataには、bind_id列がありこれで関連付けることができます。
