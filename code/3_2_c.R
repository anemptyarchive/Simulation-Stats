
# 3.2.c 正規分布の再生性 ---------------------------------------------------------

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
sample_df <- tibble::tibble(
  i = 1:N, # データ番号
  x = rnorm(n = N, mean = mu_x, sd = sigma_x), 
  y = rnorm(n = N, mean = mu_y, sd = sigma_y), 
  z = x + y
) |> 
  tidyr::pivot_longer(
    cols = !i, 
    names_to  = "dist", 
    values_to = "x"
  ) # サンプル列をまとめる

# 確率変数の範囲を設定
x_min <- min(sample_df[["x"]])
x_max <- max(sample_df[["x"]])

# 正規分布を計算
norm_df <- tibble::tibble(
  x      = seq(from = x_min, to = x_max, length.out = 1000), 
  dens_x = dnorm(x = x, mean = mu_x, sd = sigma_x), 
  dens_y = dnorm(x = x, mean = mu_y, sd = sigma_y),
  dens_z = dnorm(x = x, mean = mu_x+mu_y, sd = sqrt(sigma_x^2+sigma_y^2))
) |> 
  tidyr::pivot_longer(
    cols = !x, 
    names_prefix = "dens_", 
    names_to  = "dist", 
    values_to = "dens"
  ) # 確率密度列をまとめる


### ・結果の作図 -----

# サンプル和を抽出
sample_z_vec <- sample_df |> 
  dplyr::filter(dist == "z") |> 
  dplyr::pull(x)

# サンプル和の標本統計量を計算:(ラベル用)
mean_z <- sum(sample_z_vec) / N
var_z  <- sum((sample_z_vec - mean_z)^2) / N
sd_z   <- sqrt(var_z)

# ラベル用の文字列を作成
variable_label <- paste0(
  "list(", 
  "z[i] == x[i] + y[i]", 
  ", mu[z] == mu[x] + mu[y]", 
  ", sigma[z]^2 == sigma[x]^2 + sigma[y]^2", 
  ", sigma[z] == sqrt(sigma[x]^2 + sigma[y]^2)", 
  ")"
)
stats_label <- paste0(
  "list(", 
  "n == ", N, 
  ", E*'['*z*']' == ", round(mean_z, digits = 3), 
  ", V*'['*z*']' == ", round(var_z, digits = 3), 
  ", s*'['*z*']' == ", round(sd_z, digits = 3), 
  ")"
)
param_label_vec <- c(
  parse(text = paste0("x %~% N(", mu_x, ", ", sigma_x, ")")), 
  parse(text = paste0("y %~% N(", mu_y, ", ", sigma_y, ")")), 
  parse(text = paste0("z %~% N(", mu_x+mu_y, ", ", 
                      round(sqrt(sigma_x^2+sigma_y^2), digits = 2), ")"))
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
        legend.position = "right") + 
  labs(title = parse(text = variable_label), 
       subtitle = parse(text = stats_label), 
       color = "distribution", fill = "distribution", 
       x = "value", y = "density")


### ・推移の作図 -----

# 1フレーム当たりのデータ数を指定
n_per_frame <- 200

# フレーム数を設定
frame_num <- N / n_per_frame
frame_num


# サンプル和の標本統計量を計算:(ラベル用)
mean_z_vec <- cumsum(sample_z_vec) / 1:N
var_z_vec  <- cumsum((sample_z_vec - mean_z_vec)^2) / 1:N
sd_z_vec   <- sqrt(var_z_vec)

# サンプルを複製
trace_sample_df <- sample_df |> 
  tidyr::uncount(weights = (N-i)%/%n_per_frame+1, .id = "n") |> # 累積ミニバッチに複製
  dplyr::mutate(
    n = N - (n-1)*n_per_frame, # 行番号をデータ数に変換
    label = paste0(
      "n = ", n, 
      ", E[z] = ", round(mean_z_vec[n], digits = 3), 
      ", V[z] = ", round(var_z_vec[n], digits = 3), 
      ", s[z] = ", round(sd_z_vec[n], digits = 3)
    ) # フレーム用ラベル
  ) |> 
  tibble::add_row(
    i = 0, n = 0, 
    label = "n = 0, E[z] = NA, V[z] = NA, s[z] = NA"
  ) |> # 初回フレーム用にダミーを追加
  dplyr::arrange(n, dist, i) |> # 因子レベルの設定用
  dplyr::mutate(
    jitter = dplyr::case_when(
      dist == "x" ~ 1, 
      dist == "y" ~ 2, 
      dist == "z" ~ 3
    ), # プロット位置の調整値を指定
    label = factor(label, levels = unique(label)) # フレーム番号に応じてレベル付け
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
        legend.position = "right") + 
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


# 正規乱数の平均の分布 ------------------------------------------------------------

### ・乱数生成・統計量の計算・分布の計算 -----

# 試行回数を指定
max_iter <- 10000

# サンプルサイズを指定
N <- 4

# 正規分布のパラメータを指定
mu_x    <- 50
sigma_x <- 10


# サンプリング
sample_x_df <- tidyr::expand_grid(
  iter = 1:max_iter, # 試行番号
  i    = 1:N         # データ番号:(サンプル)
) |> 
  dplyr::group_by(iter) |> # 乱数生成用
  dplyr::mutate(
    x = rnorm(n = N, mean = mu_x, sd = sigma_x)
  ) |> 
  dplyr::ungroup()

# 標本平均を計算
sample_z_df <- sample_x_df |> 
  dplyr::group_by(iter) |> # 平均の計算用
  dplyr::summarise(
    z = mean(x), .groups = "drop"
  )

# 確率変数の範囲を設定
x_min <- min(sample_x_df[["x"]])
x_max <- max(sample_x_df[["x"]])

# 正規分布を計算
norm_df <- tibble::tibble(
  x      = seq(from = x_min, to = x_max, length.out = 1000), 
  dens_x = dnorm(x = x, mean = mu_x, sd = sigma_x), 
  dens_z = dnorm(x = x, mean = mu_x, sd = sigma_x/sqrt(N))
) |> 
  tidyr::pivot_longer(
    cols = !x, 
    names_prefix = "dens_", 
    names_to  = "dist", 
    values_to = "dens"
  ) # 確率密度列をまとめる


### ・結果の作図 -----

# サンプル平均を抽出
sample_z_vec <- sample_z_df[["z"]]

# サンプル平均の標本統計量を計算:(ラベル用)
mean_z <- sum(sample_z_vec) / max_iter
var_z  <- sum((sample_z_vec - mean_z)^2) / max_iter
sd_z   <- sqrt(var_z)

# ラベル用の文字列を作成
variable_label <- paste0(
  "list(", 
  "z[j] == frac(1, n) ~ sum(x[ji], i==1, n)", 
  ", mu[z] == ", "mu[x]", 
  ", sigma[z]^2 == frac(sigma[x]^2, n)", 
  ", sigma[z] == frac(sigma[x], sqrt(n))", 
  ")"
)
stats_label <- paste0(
  "list(", 
  "n == ", N, 
  ", m == ", max_iter, 
  ", E*'['*z*']' == ", round(mean_z, digits = 3), 
  ", V*'['*z*']' == ", round(var_z, digits = 3), 
  ", s*'['*z*']' == ", round(sd_z, digits = 3), 
  ")"
)
param_label_vec <- c(
  parse(text = paste0("x %~% N(", mu_x, ", ", sigma_x, ")")), 
  parse(text = paste0("z %~% N(", mu_x, ", ", round(sigma_x/sqrt(N), digits = 2), ")"))
)

# サンプルを作図
ggplot() + 
  geom_line(data = norm_df, 
            mapping = aes(x = x, y = dens, color = dist), 
            linewidth = 1) + # 生成分布
  geom_histogram(data = sample_z_df, 
                 mapping = aes(x = z, y = ..density..), 
                 bins = 50, fill = scales::hue_pal()(n = 2)[2], 
                 alpha = 0.5) + # サンプル平均
  scale_color_hue(labels = param_label_vec) + # (凡例ラベル用)
  theme(legend.text.align = 0, 
        legend.position = "right") + 
  labs(title = parse(text = variable_label), 
       subtitle = parse(text = stats_label), 
       color = "distribution", 
       x = "value", y = "density")


### ・推移の作図 -----

# 1フレーム当たりのデータ数を指定
n_per_frame <- 100

# フレーム数を設定
frame_num <- max_iter / n_per_frame
frame_num


# サンプル平均の標本統計量を計算:(ラベル用)
mean_z_vec <- cumsum(sample_z_vec) / 1:max_iter
var_z_vec  <- cumsum((sample_z_vec - mean_z_vec)^2) / 1:max_iter
sd_z_vec   <- sqrt(var_z_vec)

# サンプル平均を複製
trace_sample_z_df <- sample_z_df |> 
  tidyr::uncount(weights = (max_iter-iter)%/%n_per_frame+1, .id = "m") |> # 累積ミニバッチに複製
  dplyr::mutate(
    j = iter, # データ番号:(サンプル番号)
    m = max_iter - (m-1)*n_per_frame, # 行番号をデータ数に変換
    label = paste0(
      "n = ", N, 
      ", m = ", m, 
      ", E[z] = ", round(mean_z_vec[m], digits = 3), 
      ", V[z] = ", round(var_z_vec[m], digits = 3), 
      ", s[z] = ", round(sd_z_vec[m], digits = 3)
    )
  ) |> 
  tibble::add_row(
    j = 0, m = 0, 
    label = paste0("n = ", N, ", m = 0, E[z] = NA, V[z] = NA, s[z] = NA") # フレーム用ラベル
  ) |> # 初回フレーム用にダミーを追加
  dplyr::arrange(m, j) |> # 因子レベルの設定用
  dplyr::mutate(
    label = factor(label, levels = unique(label)) # フレーム番号に応じてレベル付け
  )

# サンプルを複製
trace_sample_x_df <- sample_x_df |> 
  dplyr::mutate(
    frame = (iter-1) %/% n_per_frame + 1, # フレーム番号
    m     = frame * n_per_frame, # データ数:(サンプル)
    j     = iter, # データ番号:(サンプル平均)
    color_id = (j-1) %% n_per_frame, # ミニバッチ内のデータ番号
    label = unique(trace_sample_z_df[["label"]])[frame+1] # フレーム用ラベル
  ) |> 
  tibble::add_row(
    j = 0, m = 0, 
    label = paste0("n = ", N, ", m = 0, E[z] = NA, V[z] = NA, s[z] = NA")
  ) |> 
  dplyr::arrange(m, j)


# 確率密度の最大値を設定
y_max <- max(norm_df[["dens"]])

# サンプル点のプロット位置の調整値を指定
y_min <- -0.003

# サンプルを作図
anim <- ggplot() + 
  geom_line(data = norm_df, 
            mapping = aes(x = x, y = dens, color = dist), 
            linewidth = 1) + # 生成分布
  geom_histogram(data = trace_sample_z_df, 
                 mapping = aes(x = z, y = ..density..), 
                 bins = 50, fill = scales::hue_pal()(n = 2)[2], 
                 alpha = 0.5) + # サンプル平均
  geom_point(data = trace_sample_x_df, 
             mapping = aes(x = x, y = y_min*0.5, fill = factor(color_id)), 
             size = 2, alpha = 0.1, shape = "circle filled", stroke = 0, show.legend = FALSE) + # サンプル
  geom_point(data = trace_sample_z_df, 
             mapping = aes(x = z, y = y_min), 
             size = 2, color = scales::hue_pal()(n = 2)[2], alpha = 0.01) + # サンプル平均
  gganimate::transition_manual(frames = label) + # フレーム
  scale_color_hue(labels = param_label_vec) + # (凡例ラベル用)
  coord_cartesian(ylim = c(y_min, y_max)) + 
  theme(legend.text.align = 0, 
        legend.position = "right") + 
  labs(title = parse(text = variable_label), 
       subtitle = "{current_frame}", 
       color = "distribution", 
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


