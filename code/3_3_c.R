
# 3.3.c t分布とF分布の関係 --------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(ggplot2)


# カイ2乗分布とF分布の関係 --------------------------------------------------------------

### ・乱数生成・統計量の計算・分布の計算 -----

# カイ2乗分布のパラメータを指定
nu_1 <- 5
nu_2 <- 20

# サンプルサイズを指定
N <- 10000

set.seed(123)
sample_df <- tibble::tibble(
  i   = 1:N, 
  y_1 = rchisq(n = N, df = nu_1), 
  y_2 = rchisq(n = N, df = nu_2), 
  z   = (y_1 / nu_1) / (y_2 / nu_2)
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
dens_df <- tibble::tibble(
  x        = seq(from = x_min, to = x_max, length.out = 1000), 
  dens_y_1 = dchisq(x = x, df = nu_1), 
  dens_y_2 = dchisq(x = x, df = nu_2),
  dens_z   = df(x = x, df1 = nu_1, df2 = nu_2)
) |> 
  tidyr::pivot_longer(
    cols = !x, 
    names_prefix = "dens_", 
    names_to  = "dist", 
    values_to = "dens"
  ) # 確率密度列をまとめる



### ・結果の作図 -----

# 加工サンプルを抽出
sample_z_vec <- sample_df |> 
  dplyr::filter(dist == "z") |> 
  dplyr::pull(x)

# 加工サンプルの標本統計量を計算:(ラベル用)
mean_z <- sum(sample_z_vec) / N
var_z  <- sum((sample_z_vec - mean_z)^2) / N
sd_z   <- sqrt(var_z)

# ラベル用の文字列を作成
variable_label <- paste0(
  "list(", 
  "z[i] == frac(y[i1]/nu[1], y[i2]/nu[2])", 
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
  parse(text = paste0("y[1] %~% chi^2*(", nu_1, ")")), 
  parse(text = paste0("y[2] %~% chi^2*(", nu_2, ")")), 
  parse(text = paste0("z %~% F(", nu_1, ", ", nu_2, ")"))
)

# ヒストグラムを作成
ggplot() + 
  geom_line(data = dens_df, 
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
n_per_frame <- 100

# フレーム数を設定
frame_num <- N / n_per_frame
frame_num


# 加工サンプルの標本統計量を計算:(ラベル用)
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
      dist == "y_1" ~ 1, 
      dist == "y_2" ~ 2, 
      dist == "z"   ~ 3
    ), # プロット位置の調整値を指定
    label = factor(label, levels = unique(label)) # フレーム番号に応じてレベル付け
  )


# 確率密度の最大値を設定
y_max <- max(dens_df[["dens"]])

# サンプル点のプロット位置の調整値を指定
y_min <- -0.01

# ヒストグラムを作成
anim <- ggplot() + 
  geom_line(data = dens_df, 
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
  labs(title = parse(text = variable_label), 
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


