
# パッケージ読み込み ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gridExtra)

# サンプルデータ生成 ---------------------------------------------------------------

# シナリオ:2018年1月1日から50日間の商品の購入ログ。

# 各種パラメータ
start <- "2018-1-1 00:00:00" #開始日
n <- 10000 # 購入ログ数
duration_days <- 50 # ログの期間(日数)
list_price <- c(100, 500, 1000, 2000, 5000) # 商品の価格リスト
list_price_p <- c(100, 50, 10, 5, 2) # 発生率
list_id <- 1000001:1000300 # 会員id

# df作成
# タイムスタンプ(stamp)、会員ID(id)、購入金額(value)
df_log <- data.frame(
  stamp = ymd_hms(start) +
    days(sample(0:duration_days, n, replace = TRUE)) +
    hours(sample(0:23, n, replace = TRUE)) +
    minutes(sample(0:59, n, replace = TRUE)) +
    seconds(sample(0:59, n, replace = TRUE)),
  id = sample(list_id, n, replace = TRUE),
  value = sample(list_price, n, replace = TRUE, prob = list_price_p)
) %>% arrange(stamp)


# ハンドリング(1) ------------------------------------------------------------------

# 商品ラベルを変数として生成
# 今回はitem_1-5とする

item_price <- paste("item", 1:length(list_price), sep = "_") #itemのリスト作成
pat <- str_c("^", list_price, "$", collapse = "|") # 置換用ベクトル

# 商品ラベルを変数として追加
df_log <- df_log %>% 
  mutate(item = str_replace_all(value, pat, item_price))


# ユーザーごとの集計 ---------------------------------------------------------------

# ユーザーごとの集計を準備する
# 基本はgroup_by()

df_log_user <- df_log %>%
  group_by(id) %>% 
  summarise(
    n_buy = n(), #購入回数
    value_buy = sum(value), #期間内購入合計
    mean_buy = mean(value) #平均購入金額
  )
# とりあえず可視化
p_user <- ggplot(df_log_user)
p_user1 <- p_user + 
  geom_histogram(aes(x = value_buy))
p_user2 <- p_user +
  geom_histogram(aes(x = mean_buy))
p_user3 <- p_user + 
  geom_histogram(aes(x = n_buy))
p_user4 <- p_user +
  geom_point(aes(x = value_buy, y = mean_buy))
grid.arrange(p_user1, p_user2, p_user3, p_user4, nrow = 2)


# ユーザーとタイムスタンプ -------------------------------------------------------------

# ユーザーとタイムスタンプでグループ化
# ただし、日別の集計とする

df_log_user_stamp <- df_log %>% 
  mutate(date = date(stamp)) %>% 
  group_by(id, date) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

# 上位購入10名の購入金額をヒートマップ的に表現

top10user <- df_log_user %>% 
  top_n(10, value_buy)
df_log_top10user <- df_log_user_stamp %>% 
  ungroup(id) %>% 
  filter(id %in% top10user$id) %>% 
  mutate(id = as.factor(id))
ggplot(df_log_top10user) +
  geom_tile(aes(x = date, y = id, fill = value_buy))



# スタンプに関する内容 --------------------------------------------------------------

# タイムスタンプに関する処理を中心に
# 日付による変動

df_log_date <- df_log %>% 
  mutate(date = date(stamp)) %>% 
  group_by(date) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

# 日別推移を可視化

p_date <- ggplot(df_log_date)
p_date1 <- p_date + 
  geom_line(aes(x = date, y = n_buy))
p_date2 <- p_date + 
  geom_line(aes(x = date, y = value_buy))
p_date3 <- p_date + 
  geom_line(aes(x = date, y = mean_buy))
grid.arrange(p_date1, p_date2, p_date3, nrow = 2)

# 曜日別の違い

df_log_week <- df_log %>% 
  mutate(week = wday(stamp, label = TRUE, abbr = FALSE)) %>% 
  group_by(week) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

# 曜日別を可視化

p_week <- ggplot(df_log_week)
p_week1 <- p_week +
  geom_bar(aes(x = week, y = n_buy), stat = "identity")
p_week2 <- p_week +
  geom_bar(aes(x = week, y = value_buy), stat = "identity")
p_week3 <- p_week +
  geom_bar(aes(x = week, y = mean_buy), stat = "identity")
grid.arrange(p_week1, p_week2, p_week3)

# 時間別に分析

df_log_hour <- df_log %>% 
  mutate(hour = hour(stamp)) %>% 
  group_by(hour) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

# 時間別を可視化

p_hour <- ggplot(df_log_hour)
p_hour1 <- p_hour +
  geom_line(aes(x = hour, y = n_buy))
p_hour2 <- p_hour +
  geom_line(aes(x = hour, y = value_buy))
p_hour3 <- p_hour +
  geom_line(aes(x = hour, y = mean_buy))
grid.arrange(p_hour1, p_hour2, p_hour3)

# 応用。時間と項目でヒートマップ

df_log_hour_item <- df_log %>% 
  mutate(hour = hour(stamp)) %>% 
  group_by(hour, item) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

# 時間と項目で可視化
p_hour_item <- ggplot(df_log_hour_item)
p_hour_item1 <- p_hour_item +
  geom_tile(aes(x = hour, y = item, fill = n_buy))
p_hour_item2 <- p_hour_item +
  geom_tile(aes(x = hour, y = item, fill = value_buy))
p_hour_item3 <- p_hour_item +
  geom_tile(aes(x = hour, y = item, fill = mean_buy))
grid.arrange(p_hour_item1, p_hour_item2, p_hour_item3)

# 週間での売上推移

# 曜日と週間でのヒートマップ
# week(isoweek) x wdayで作成可能

df_log_weekday_no <- df_log %>% 
  mutate(weekday = wday(stamp, label = TRUE, abbr = FALSE),
         week_no = week(stamp)) %>% 
  group_by(week_no, weekday) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

# 曜日と週番号で可視化
p_weekday_no <- ggplot(df_log_weekday_no)
p_weekday_no1 <- p_weekday_no +
  geom_tile(aes(x = week_no, y = weekday, fill = n_buy))
p_weekday_no2 <- p_weekday_no +
  geom_tile(aes(x = week_no, y = weekday, fill = value_buy))
p_weekday_no3 <- p_weekday_no +
  geom_tile(aes(x = week_no, y = weekday, fill = mean_buy))
grid.arrange(p_weekday_no1, p_weekday_no2, p_weekday_no3)

# 一定区間(%within%)を指定しての分析
# 1/16から1/31までを指定してみる

# intervalオブジェクトを生成
target_interval <- interval(
  start = ymd("2018-1-16"),
  end = ymd("2018-1-31")
)

# ターゲット期間のみを取り出して集計
# 指定が効いているのを確認するため日別集計
df_log_interval <- df_log %>% 
  filter(stamp %within% target_interval) %>% 
  mutate(date = date(stamp)) %>% 
  group_by(date) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

# 指定区間を取り出した範囲で可視化
p_int_date <- ggplot(df_log_interval)
p_int_date1 <- p_int_date + 
  geom_line(aes(x = date, y = n_buy))
p_int_date2 <- p_int_date + 
  geom_line(aes(x = date, y = value_buy))
p_int_date3 <- p_int_date + 
  geom_line(aes(x = date, y = mean_buy))
grid.arrange(p_int_date1, p_int_date2, p_int_date3, nrow = 2)

# 区間指定の応用。一定期間とそれ以外で検証
# filterではなくif_elseでmutateする
# 3つ以上ならcase_whenでパターンを準備すればOK

df_log_interval_comp <- df_log %>% 
  mutate(target = if_else(
    stamp %within% target_interval, "target", "other"
  )) %>% 
  mutate(hour = hour(stamp)) %>% 
  group_by(hour, target) %>% 
  summarise(
    n_buy = n(),
    value_buy = sum(value),
    mean_buy = mean(value)
  )

p_int_comp_date <- ggplot(df_log_interval_comp)
p_int_comp_date1 <- p_int_comp_date + 
  geom_boxplot(aes(x = target, y = mean_buy, color = target)) +
  coord_flip()
p_int_comp_date2 <- p_int_comp_date + 
  geom_line(aes(x = hour, y = mean_buy, color = target))
grid.arrange(p_int_comp_date1, p_int_comp_date2, nrow = 2)

# ラグに関する分析
