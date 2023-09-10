
# column 正規分布の再生性 ---------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# 正規乱数の和の分布 ---------------------------------------------------------------

### ・乱数生成・統計量の計算・分布の計算 -----

# サンプルサイズを指定
N <- 20000

# 正規分布のパラメータを指定
mu_x    <- 5
sigma_x <- 4
mu_y    <- -10
sigma_y <- 2.5

# サンプリング
set.seed(123)
sample_df <- tibble::tibble(
  i = 1:N, 
  x = rnorm(n = N, mean = mu_x, sd = sigma_x), 
  y = rnorm(n = N, mean = mu_y, sd = sigma_y), 
  z = x + y
) |> 
  tidyr::pivot_longer(
    cols = !i, 
    names_to  = "dist", 
    values_to = "x"
  )

# 確率変数の範囲を設定
x_min <- min(sample_df[["x"]])
x_max <- max(sample_df[["x"]])

# 正規分布を計算
norm_df <- tibble::tibble(
  x = seq(from = x_min, to = x_max, length.out = 1000), 
  dens_x = dnorm(x = x, mean = mu_x, sd = sigma_x), 
  dens_y = dnorm(x = x, mean = mu_y, sd = sigma_y),
  dens_z = dnorm(x = x, mean = mu_x+mu_y, sd = sqrt(sigma_x^2+sigma_y^2))
) |> 
  tidyr::pivot_longer(
    cols = !x, 
    names_prefix = "dens_", 
    names_to  = "dist", 
    values_to = "dens"
  )


### ・作図 -----

# ラベル用の文字列を作成
param_label_vec <- c(
  parse(text = paste0("x %~% N(", mu_x, ", ", sigma_x, ")")), 
  parse(text = paste0("y %~% N(", mu_y, ", ", sigma_y, ")")), 
  parse(text = paste0("z %~% N(mu[x]+mu[y], sqrt(sigma[x]^2+sigma[y]^2))"))
)
param_label <- paste0(
  "list(", 
  "mu[x]+mu[y] == ", round(mu_x+mu_y, digits = 2), 
  ", sigma[x]^2+sigma[y]^2 == ", round(sigma_x^2+sigma_y^2, digits = 2), 
  ", sqrt(sigma[x]^2+sigma[y]^2) == ", round(sqrt(sigma_x^2+sigma_y^2), digits = 2), 
  ", n == ", N, 
  ")"
)

# ヒストグラムを作成
ggplot() + 
  geom_line(data = norm_df, 
            mapping = aes(x = x, y = dens, color = dist), 
            linewidth = 1) + # 生成分布
  geom_histogram(data = sample_df, 
                 mapping = aes(x = x, y = ..density.., fill = dist), 
                 position = "identity", bins = 100, 
                 alpha = 0.5) + # サンプル
  scale_color_hue(labels = param_label_vec) + # (凡例ラベル用)
  scale_fill_hue(labels = param_label_vec) + # (凡例ラベル用)
  theme(legend.text.align = 0, 
        legend.position = "top") + 
  labs(title = expression(z[i] == x[i] + y[i]), 
       subtitle = parse(text = param_label), 
       color = "distribution", fill = "distribution", 
       x = "value", y = "density")


### ・アニメーションの作図 -----

# 1フレーム当たりのデータ数を指定
n_per_frame <- 200

# フレーム数を設定
frame_num <- N / n_per_frame
frame_num

# 確率変数zを抽出
sample_z_vec <- sample_df |> 
  dplyr::filter(dist == "z") |> 
  dplyr::pull(x)

# 標本統計量を計算
mean_z_vec <- cumsum(sample_z_vec) / 1:N
var_z_vec  <- cumsum((sample_z_vec - mean_z_vec)^2) / 1:N
sd_z_vec   <- sqrt(var_z_vec)

# アニメ用にサンプルを複製
trace_sample_df <- sample_df |> 
  tidyr::uncount(weights = (N-i)%/%n_per_frame+1, .id = "n") |> # バッチデータに複製
  dplyr::mutate(
    n = N - (n-1)*n_per_frame, # 複製行番号列をデータ数列に変換
    label = paste0(
      "n = ", n, 
      ", E[z] = ", round(mean_z_vec[n], digits = 3), 
      ", V[z] = ", round(var_z_vec[n], digits = 3), 
      ", s[z] = ", round(sd_z_vec[n], digits = 3)
    )
  ) |> 
  tibble::add_row(
    i = 0, 
    n = 0, 
    label = "n = 0, E[z] = NA, V[z] = NA, s[z] = NA"
  ) |> # 初回フレーム用にダミーを追加
  dplyr::arrange(n, dist, i) |> # 因子レベルの設定用
  dplyr::mutate(
    jitter = dplyr::case_when(
      dist == "x" ~ 1, 
      dist == "y" ~ 2, 
      dist == "z" ~ 3
    ), 
    label = factor(label, levels = unique(label)) # フレーム番号に応じてレベル付け
  )


# ラベル用の文字列を作成
param_label_vec <- c(
  parse(text = paste0("x %~% N(", mu_x, ", ", sigma_x, ")")), 
  parse(text = paste0("y %~% N(", mu_y, ", ", sigma_y, ")")), 
  parse(text = paste0("z %~% N(", round(mu_x+mu_y, digits = 2), ", ", 
                      round(sqrt(sigma_x^2+sigma_y^2), digits = 2), ")"))
)

# 確率密度の最大値を設定
y_max <- max(norm_df[["dens"]])

# サンプル点のプロット位置の調整値を指定
y_min <- -0.003

# ヒストグラムを作成
anim <- ggplot() + 
  geom_line(data = norm_df, 
            mapping = aes(x = x, y = dens, color = dist), 
            linewidth = 1) + # 生成分布
  geom_histogram(data = trace_sample_df, 
                 mapping = aes(x = x, y = ..density.., fill = dist), 
                 position = "identity", bins = 100, 
                 alpha = 0.5) + # サンプル
  geom_point(data = trace_sample_df, 
             mapping = aes(x = x, y = y_min*jitter, color = dist), 
             size = 2, alpha = 0.01) + # サンプル
  gganimate::transition_manual(frames = label) + # フレーム
  scale_color_hue(labels = param_label_vec) + # (凡例ラベル用)
  scale_fill_hue(labels = param_label_vec) + # (凡例ラベル用)
  coord_cartesian(ylim = c(y_min*3, y_max)) + 
  theme(legend.text.align = 0, 
        legend.position = "top") + 
  labs(title = expression(z[i] == x[i] + y[i]), 
       subtitle = "{current_frame}", 
       color = "distribution", fill = "distribution", 
       x = "value", y = "density")

# 停止フレーム数を指定
pause_frame <- 10

# gif画像を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num+pause_frame, end_pause = pause_frame, fps = 10, 
  width = 800, height = 600, 
  renderer = gganimate::gifski_renderer()
)


