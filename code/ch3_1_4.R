
# 3.1.4 さまざまな確率分布に従う乱数の生成 -----------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# モンテカルロ・シミュレーションによる面積の近似計算 ----------------------------

# 正規分布のパラメータを指定
mu  <- -1
sgm <- 1.5

# 確率計算用の範囲を指定
x_lower <- -0.5
x_upper <- 3

# 確率密度の最大値を確認
seq(from = x_lower, to = x_upper, length.out = 1000) |> 
  dnorm(mean = mu, sd = sgm) |> 
  max()

# サンプル用の範囲を指定
x_min <- -2
x_max <- 4
y_max <- 0.26

# 真の確率(面積)を計算
true_prob_val <- pnorm(q = x_upper, mean = mu, sd = sgm) - pnorm(q = x_lower, mean = mu, sd = sgm)

# 確率区間の正規分布を計算
norm_area_df <- tibble::tibble(
  x    = seq(from = x_lower, to = x_upper, length.out = 1000), 
  dens = dnorm(x = x, mean = mu, sd = sgm)
)

# サンプリング区間の正規分布を計算
norm_df <- tibble::tibble(
  x    = seq(from = x_min, to = x_max, length.out = 1000), 
  dens = dnorm(x = x, mean = mu, sd = sgm)
)

# 試行回数(サンプルサイズ)を指定
iter <- 50000

# サンプリング
set.seed(123)
dots_df <- tibble::tibble(
  n = 1:iter, # 試行回数(累積データ数)
  x = runif(n = iter, min = x_min, max = x_max), # x軸の値
  y = runif(n = iter, min = 0, max = y_max)      # y軸の値
) |> 
  dplyr::mutate(
    inner_flag  = x > x_lower & x < x_upper & y < dnorm(x = x, mean = mu, sd = sgm), # 領域内の判定
    inner_num   = cumsum(inner_flag), # 領域内の累積データ数
    inner_ratio = inner_num / n       # 領域内データ率
  )

# 領域内のデータの割合を取得
inner_ratio_val <- dots_df |> 
  dplyr::filter(n == iter) |> 
  dplyr::pull(inner_ratio)

# サンプリング範囲の面積を計算
sample_area_val <- (x_max - x_min) * y_max

# 確率の近似値を計算
approx_prob_val <- sample_area_val * inner_ratio_val


# ラベル位置を格納
label_df <- tibble::tibble(
  x = c(x_lower, x_upper, mu)
)

# サンプルを作図
ggplot() + 
  geom_rect(mapping = aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = y_max), 
            color = "black", alpha = 0, linewidth = 1) + # サンプリングの範囲
  geom_line(data = norm_df, 
            mapping = aes(x = x, y = dens), 
            linewidth = 1) + # 正規分布の曲線
  geom_area(data = norm_area_df, 
            mapping = aes(x = x, y = dens), 
            color = "black", alpha = 0.1, linewidth = 1) + # 真の確率の領域
  geom_point(data = dots_df, 
             mapping = aes(x = x, y = y, color = inner_flag), 
             alpha = 0.5, size = 1.5) + # サンプルの点
  geom_vline(mapping = aes(xintercept = mu), 
             color = "blue", linewidth = 1, linetype = "dashed") + # 正規分布の平均
  geom_label(data = label_df, 
             mapping = aes(x = x, y = 0, label = x), 
             vjust = 1) + # 区間ラベル
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("green", "orange")) + # 領域内外で色分け
  theme(legend.position = "none") + 
  labs(title = "Monte Carlo method", 
       subtitle = paste0("n = ", iter, ", approx: ", approx_prob_val, ", ture: ", round(true_prob_val, digits = 5)), 
       x = "x", y = "density")


# 1フレーム当たりのデータ数を指定
n_per_frame <- 500

# フレーム数を設定
frame_num <- iter / n_per_frame
frame_num

# アニメ用にサンプルを複製
trace_dots_df <- dots_df |> 
  dplyr::rename(i = n) |> # データ番号列に変更
  tidyr::uncount(weights = (iter-i)%/%n_per_frame+1, .id = "n") |> # バッチデータに複製
  dplyr::mutate(
    n = iter - (n-1)*n_per_frame # 複製行番号列をデータ数列に変換
  ) |> 
  dplyr::group_by(n) |> # 割合の計算用
  dplyr::mutate(
    inner_ratio = sum(inner_flag) / n, # 領域内データ率
    label = paste0(
      "n = ", n, 
      ", approx: ", round(sample_area_val*inner_ratio, digits = 5), 
      ", true: ", round(true_prob_val, digits = 5)
    )
  ) |> 
  dplyr::ungroup() |> 
  tibble::add_row(
    i = 0, 
    n = 0, 
    label = paste0("n = 0, approx: NA, true: ", true_prob_val)
  ) |> # 初回フレーム用にダミーを追加
  dplyr::arrange(n, i) |> # 因子レベルの設定用
  dplyr::mutate(
    label = factor(label, levels = unique(label)) # フレーム番号に応じてレベル付け
  )

# サンプルのアニメーションを作図
anim <- ggplot() + 
  geom_rect(mapping = aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = y_max), 
            color = "black", alpha = 0, linewidth = 1) + # サンプリングの範囲
  geom_line(data = norm_df, 
            mapping = aes(x = x, y = dens), 
            linewidth = 1) + # 正規分布の曲線
  geom_area(data = norm_area_df, 
            mapping = aes(x = x, y = dens), 
            color = "black", alpha = 0.1, linewidth = 1) + # 真の確率の領域
  geom_point(data = trace_dots_df, 
             mapping = aes(x = x, y = y, color = inner_flag), 
             alpha = 0.5, size = 1.5) + # サンプルの点
  gganimate::transition_manual(frames = label) + # フレーム
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("green", "orange")) + # 領域内外で色分け
  theme(legend.position = "none") + 
  labs(title = "Monte Carlo method", 
       subtitle = "{current_frame}", 
       x = "x", y = "density")

# 停止フレーム数を指定
pause_frame <- 10

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num+pause_frame, end_pause = pause_frame, fps = 10, 
  width = 800, height = 600, 
  renderer = gganimate::gifski_renderer()
)


