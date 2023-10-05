
# 確率分布のパラメータの推定量の一致性 ------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)

# チェック用
library(ggplot2)


# 正規分布 -----------------------------------------------------------------

# 母数を指定
mu    <- -5
sigma <- 2

# サンプルサイズを指定
group_num   <- 10
sample_size <- 1000

# サンプルを作成して、推定量を計算
sample_df <- tidyr::expand_grid(
  group_id = 1:group_num,  # グループ番号
  n        = 1:sample_size # データ番号・累積データ数
) |> # グループごとにデータ番号を複製
  dplyr::group_by(group_id) |> # サンプルの生成・計算用
  dplyr::mutate(
    x      = rnorm(n = sample_size, mean = mu, sd = sigma), # サンプリング
    mean_x = cumsum(x) / n, 
    var_x  = cumsum((x - mean_x)^2) / n, 
    sd_x   = sqrt(var_x)
  ) |> 
  dplyr::ungroup()


# 一時保存フォルダを指定
dir_path <- "figure/tmp"

# フレーム数を指定
frame_num   <- 100
n_per_frame <- sample_size %/% frame_num

# グラフサイズを設定
max_dens <- 1
max_dev  <- max(abs(min(sample_df[["x"]]) - mu), max(sample_df[["x"]]) - mu) # (母平均を中心に配置用)
min_x    <- mu - max_dev
max_x    <- mu + max_dev
max_dev  <- max(abs(min(sample_df[["mean_x"]]) - mu), max(sample_df[["mean_x"]]) - mu) # (母平均を中心に配置用)
min_mean <- mu - max_dev
max_mean <- mu + max_dev
max_sd   <- max(sample_df[["sd_x"]])

# ラベル用の文字列を作成
variable_label <- expression(list(X[i] %~% N(mu, sigma), (i == list(1, 2, ..., n))))
mean_label     <- expression(bar(X) == frac(1, n) ~ sum(X[i], i==1, n))
var_label      <- expression(S^2 == frac(1, n) ~ sum({}, i==1, n) * (X[i] - bar(X))^2)
sd_label       <- expression(S == sqrt(S^2))

# グラフの書き出し
for(i in 1:frame_num) {
  
  # i回目のデータ数を設定
  tmp_n <- i * n_per_frame
  
  # n個のサンプルを抽出
  tmp_sample_df <- sample_df |> 
    dplyr::filter(n <= tmp_n) # n番目までのサンプルを抽出
  
  # n個の標本統計量を抽出
  tmp_param_df <- tmp_sample_df |> 
    dplyr::filter(n == tmp_n) |> # n個時のデータを抽出
    dplyr::mutate(
      x_left  = mu - sd_x, 
      x_right = mu + sd_x, 
      v       = - group_id / group_num, 
      label = paste0(
        "list(", 
        "bar(X) == ", round(mean_x, digits = 2), ", ", 
        "S^2 == ", round(var_x, digits = 2), ", ", 
        "S == ", round(sd_x, digits = 2), 
        ")"
      )
    )
  
  # 母数ラベル用の文字列を作成
  param_label <- paste0(
    "list(", 
    "mu == ", mu, ", ", 
    "sigma^2 == ", round(sigma^2, digits = 2), ", ", 
    "sigma == ", sigma, ", ", 
    "n == ", tmp_n, 
    ")"
  )
  
  # サンプルのヒストグラムを作図
  d <- 0.1
  hist_graph <- ggplot() + 
    geom_histogram(data = tmp_sample_df, 
                   mapping = aes(x = x, y = ..density.., fill = factor(group_id)), 
                   position = "identity", bins = 50, 
                   alpha = 0.1) + # サンプル:(塗りつぶし)
    stat_bin(data = tmp_sample_df, 
             mapping = aes(x = x, y = ..density.., color = factor(group_id)), 
             geom = "step", position = "identity", direction = "mid", pad = TRUE, bins = 50) + # サンプル:(枠)
    geom_vline(mapping = aes(xintercept = mu), 
               color = "red", linetype = "dashed") + # 母平均
    geom_vline(data = tmp_param_df, 
               mapping = aes(xintercept = mean_x, color = factor(group_id)), 
               show.legend = FALSE) + # 標本平均
    geom_segment(mapping = aes(x = mu-sigma, y = 0, xend = mu+sigma, yend = 0), 
                 color = "red", linetype = "dashed") + # 母標準偏差
    geom_segment(data = tmp_param_df, 
                 mapping = aes(x = x_left, y = v*d, xend = x_right, yend = v*d, color = factor(group_id)), 
                 show.legend = FALSE) + # 標本標準偏差
    scale_color_hue(labels = parse(text = tmp_param_df[["label"]]), name = NULL) + 
    scale_fill_hue(guide = "none") + # (両方使うとなぜか凡例が分裂する)
    coord_cartesian(xlim = c(min_x, max_x), ylim = c(-d, max_dens)) + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha('white', 0.5))) + 
    labs(title = variable_label, 
         subtitle = parse(text = param_label), 
         x = "x", y = "density")
  
  # 標本平均の推移を作図
  mean_graph <- ggplot() + 
    geom_hline(mapping = aes(yintercept = mu), 
               color = "red", linetype = "dashed") + # 母平均
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = mean_x, color = factor(group_id))) + # 標本平均:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = mean_x, color = factor(group_id))) + # 標本平均:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(min_mean, max_mean)) + 
    theme(legend.position = "none") + 
    labs(title = mean_label, 
         x = "n", y = "mean")
  
  # 標本分散の推移を作図
  var_graph <- ggplot() + 
    geom_hline(mapping = aes(yintercept = sigma^2), 
               color = "red", linetype = "dashed") + # 母分散
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = var_x, color = factor(group_id))) + # 標本分散:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = var_x, color = factor(group_id))) + # 標本分散:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(0, max_sd^2)) + 
    theme(legend.position = "none") + 
    labs(title = var_label, 
         x = "n", y = "variance")
  
  # 標本標準偏差の推移を作図
  sd_graph <- ggplot() + 
    geom_hline(mapping = aes(yintercept = sigma), 
               color = "red", linetype = "dashed") + # 母標準偏差
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = sd_x, color = factor(group_id))) + # 標本標準偏差:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = sd_x, color = factor(group_id))) + # 標本標準偏差:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(0, max_sd)) + 
    theme(legend.position = "none") + 
    labs(title = sd_label, 
         x = "n", y = "standard deviation")
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    hist_graph, mean_graph, var_graph, sd_graph, 
    ncol = 1, heights = c(1, 0.5, 0.5, 0.5)
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 800, height = 1200, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "figure/ch4_1_3/norm.gif", delay = 0.1) -> tmp_path # gifファイル書き出し


# t分布 ---------------------------------------------------------------------

# 母数を指定:(ここではν > 2)
nu <- 4
mu    <- 0 # (固定)
sigma <- sqrt(nu / (nu - 2))

# サンプルサイズを指定
group_num   <- 10
sample_size <- 1000

# サンプルを作成して、推定量を計算
sample_df <- tidyr::expand_grid(
  group_id = 1:group_num,  # グループ番号
  n        = 1:sample_size # データ番号・累積データ数
) |> # グループごとにデータ番号を複製
  dplyr::group_by(group_id) |> # サンプルの生成・計算用
  dplyr::mutate(
    x      = rt(n = sample_size, df = nu), # サンプリング
    mean_x = cumsum(x) / n, 
    var_x  = cumsum((x - mean_x)^2) / n, 
    sd_x   = sqrt(var_x)
  ) |> 
  dplyr::ungroup()


# 一時保存フォルダを指定
dir_path <- "figure/tmp"

# フレーム数を指定
frame_num   <- 100
n_per_frame <- sample_size %/% frame_num

# グラフサイズを設定
max_dens <- 1
max_dev  <- max(abs(min(sample_df[["x"]]) - mu), max(sample_df[["x"]]) - mu) # (母平均を中心に配置用)
min_x    <- mu - max_dev
max_x    <- mu + max_dev
max_dev  <- max(abs(min(sample_df[["mean_x"]]) - mu), max(sample_df[["mean_x"]]) - mu) # (母平均を中心に配置用)
min_mean <- mu - max_dev
max_mean <- mu + max_dev
max_sd   <- max(sample_df[["sd_x"]])

# ラベル用の文字列を作成
variable_label <- expression(list(X[i] %~% t(nu), (i == list(1, 2, ..., n)), sigma^2 == frac(nu, nu-2)))
mean_label     <- expression(bar(X) == frac(1, n) ~ sum(X[i], i==1, n))
var_label      <- expression(S^2 == frac(1, n) ~ sum({}, i==1, n) * (X[i] - bar(X))^2)
sd_label       <- expression(S == sqrt(S^2))

# グラフの書き出し
for(i in 1:frame_num) {
  
  # i回目のデータ数を設定
  tmp_n <- i * n_per_frame
  
  # n個のサンプルを抽出
  tmp_sample_df <- sample_df |> 
    dplyr::filter(n <= tmp_n) # n番目までのサンプルを抽出
  
  # n個の標本統計量を抽出
  tmp_param_df <- tmp_sample_df |> 
    dplyr::filter(n == tmp_n) |> # n個時のデータを抽出
    dplyr::mutate(
      x_left  = mu - sd_x, 
      x_right = mu + sd_x, 
      v       = - group_id / group_num, 
      label = paste0(
        "list(", 
        "bar(X) == ", round(mean_x, digits = 2), ", ", 
        "S^2 == ", round(var_x, digits = 2), ", ", 
        "S == ", round(sd_x, digits = 2), 
        ")"
      )
    )
  
  # 母数ラベル用の文字列を作成
  param_label <- paste0(
    "list(", 
    "nu == ", nu, ", ", 
    "mu == ", mu, ", ", 
    "sigma^2 == ", round(sigma^2, digits = 2), ", ", 
    "sigma == ", round(sigma, digits = 2), ", ", 
    "n == ", tmp_n, 
    ")"
  )
  
  # サンプルのヒストグラムを作図
  d <- 0.1
  hist_graph <- ggplot() + 
    geom_histogram(data = tmp_sample_df, 
                   mapping = aes(x = x, y = ..density.., fill = factor(group_id)), 
                   position = "identity", bins = 50, 
                   alpha = 0.1) + # サンプル:(塗りつぶし)
    stat_bin(data = tmp_sample_df, 
             mapping = aes(x = x, y = ..density.., color = factor(group_id)), 
             geom = "step", position = "identity", direction = "mid", pad = TRUE, bins = 50) + # サンプル:(枠)
    geom_vline(mapping = aes(xintercept = mu), 
               color = "red", linetype = "dashed") + # 母平均
    geom_vline(data = tmp_param_df, 
               mapping = aes(xintercept = mean_x, color = factor(group_id)), 
               show.legend = FALSE) + # 標本平均
    geom_segment(mapping = aes(x = mu-sigma, y = 0, xend = mu+sigma, yend = 0), 
                 color = "red", linetype = "dashed") + # 母標準偏差
    geom_segment(data = tmp_param_df, 
                 mapping = aes(x = x_left, y = v*d, xend = x_right, yend = v*d, color = factor(group_id)), 
                 show.legend = FALSE) + # 標本標準偏差
    scale_color_hue(labels = parse(text = tmp_param_df[["label"]]), name = NULL) + 
    scale_fill_hue(guide = "none") + # (両方使うとなぜか凡例が分裂する)
    coord_cartesian(xlim = c(min_x, max_x), ylim = c(-d, max_dens)) + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha('white', 0.5))) + 
    labs(title = variable_label, 
         subtitle = parse(text = param_label), 
         x = "x", y = "density")
  
  # 標本平均の推移を作図
  mean_graph <- ggplot() + 
    geom_hline(mapping = aes(yintercept = mu), 
               color = "red", linetype = "dashed") + # 母平均
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = mean_x, color = factor(group_id))) + # 標本平均:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = mean_x, color = factor(group_id))) + # 標本平均:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(min_mean, max_mean)) + 
    theme(legend.position = "none") + 
    labs(title = mean_label, 
         x = "n", y = "mean")
  
  # 標本分散の推移を作図
  var_graph <- ggplot() + 
    geom_hline(mapping = aes(yintercept = sigma^2), 
               color = "red", linetype = "dashed") + # 母分散
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = var_x, color = factor(group_id))) + # 標本分散:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = var_x, color = factor(group_id))) + # 標本分散:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(0, max_sd^2)) + 
    theme(legend.position = "none") + 
    labs(title = var_label, 
         x = "n", y = "variance")
  
  # 標本標準偏差の推移を作図
  sd_graph <- ggplot() + 
    geom_hline(mapping = aes(yintercept = sigma), 
               color = "red", linetype = "dashed") + # 母標準偏差
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = sd_x, color = factor(group_id))) + # 標本標準偏差:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = sd_x, color = factor(group_id))) + # 標本標準偏差:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(0, max_sd)) + 
    theme(legend.position = "none") + 
    labs(title = sd_label, 
         x = "n", y = "standard deviation")
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    hist_graph, mean_graph, var_graph, sd_graph, 
    ncol = 1, heights = c(1, 0.5, 0.5, 0.5)
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 800, height = 1200, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "figure/ch4_1_3/t.gif", delay = 0.1) -> tmp_path # gifファイル書き出し


# コーシー分布 ---------------------------------------------------------------------

# 母数を指定:(固定)
nu    <- 1
mu    <- NaN
sigma <- NaN

# サンプルサイズを指定
group_num   <- 10
sample_size <- 1000

# サンプルを作成して、推定量を計算
sample_df <- tidyr::expand_grid(
  group_id = 1:group_num,  # グループ番号
  n        = 1:sample_size # データ番号・累積データ数
) |> # グループごとにデータ番号を複製
  dplyr::group_by(group_id) |> # サンプルの生成・計算用
  dplyr::mutate(
    x      = rt(n = sample_size, df = nu), # サンプリング
    mean_x = cumsum(x) / n, 
    var_x  = cumsum((x - mean_x)^2) / n, 
    sd_x   = sqrt(var_x)
  ) |> 
  dplyr::ungroup()


# 一時保存フォルダを指定
dir_path <- "figure/tmp"

# フレーム数を指定
frame_num   <- 100
n_per_frame <- sample_size %/% frame_num

# グラフサイズを設定
max_dens <- 0.01
max_dev  <- max(abs(min(sample_df[["x"]])), max(sample_df[["x"]])) # (0を中心に配置用)
min_x    <- -max_dev
max_x    <- max_dev
max_dev  <- max(abs(min(sample_df[["mean_x"]])), max(sample_df[["mean_x"]])) # (0を中心に配置用)
min_mean <- -max_dev
max_mean <- max_dev
max_sd   <- max(sample_df[["sd_x"]])

# ラベル用の文字列を作成
variable_label <- expression(list(X[i] %~% Caucy(nu), (i == list(1, 2, ..., n))))
mean_label     <- expression(bar(X) == frac(1, n) ~ sum(X[i], i==1, n))
var_label      <- expression(S^2 == frac(1, n) ~ sum({}, i==1, n) * (X[i] - bar(X))^2)
sd_label       <- expression(S == sqrt(S^2))

# グラフの書き出し
for(i in 1:frame_num) {
  
  # i回目のデータ数を設定
  tmp_n <- i * n_per_frame
  
  # n個のサンプルを抽出
  tmp_sample_df <- sample_df |> 
    dplyr::filter(n <= tmp_n) # n番目までのサンプルを抽出
  
  # n個の標本統計量を抽出
  tmp_param_df <- tmp_sample_df |> 
    dplyr::filter(n == tmp_n) |> # n個時のデータを抽出
    dplyr::mutate(
      x_left  = -sd_x, 
      x_right = sd_x, 
      v       = - group_id / group_num, 
      label = paste0(
        "list(", 
        "bar(X) == ", round(mean_x, digits = 2), ", ", 
        "S^2 == ", round(var_x, digits = 2), ", ", 
        "S == ", round(sd_x, digits = 2), 
        ")"
      )
    )
  
  # 母数ラベル用の文字列を作成
  param_label <- paste0(
    "list(", 
    "nu == ", nu, ", ", 
    "mu == ", mu, ", ", 
    "sigma^2 == ", round(sigma^2, digits = 2), ", ", 
    "sigma == ", round(sigma, digits = 2), ", ", 
    "n == ", tmp_n, 
    ")"
  )
  
  # サンプルのヒストグラムを作図
  d <- 0.001
  hist_graph <- ggplot() + 
    geom_histogram(data = tmp_sample_df, 
                   mapping = aes(x = x, y = ..density.., fill = factor(group_id)), 
                   position = "identity", bins = 100, 
                   alpha = 0.1) + # サンプル:(塗りつぶし)
    stat_bin(data = tmp_sample_df, 
             mapping = aes(x = x, y = ..density.., color = factor(group_id)), 
             geom = "step", position = "identity", direction = "mid", pad = TRUE, bins = 100) + # サンプル:(枠)
    geom_vline(data = tmp_param_df, 
               mapping = aes(xintercept = mean_x, color = factor(group_id)), 
               show.legend = FALSE) + # 標本平均
    geom_segment(data = tmp_param_df, 
                 mapping = aes(x = x_left, y = v*d, xend = x_right, yend = v*d, color = factor(group_id)), 
                 show.legend = FALSE) + # 標本標準偏差
    scale_color_hue(labels = parse(text = tmp_param_df[["label"]]), name = NULL) + 
    scale_fill_hue(guide = "none") + # (両方使うとなぜか凡例が分裂する)
    coord_cartesian(xlim = c(min_x, max_x), ylim = c(-d, max_dens)) + 
    theme(legend.text.align = 0, 
          legend.position = c(0, 1), 
          legend.justification = c(0, 1), 
          legend.background = element_rect(fill = alpha('white', 0.5))) + 
    labs(title = variable_label, 
         subtitle = parse(text = param_label), 
         x = "x", y = "density")
  
  # 標本平均の推移を作図
  mean_graph <- ggplot() + 
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = mean_x, color = factor(group_id))) + # 標本平均:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = mean_x, color = factor(group_id))) + # 標本平均:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(min_mean, max_mean)) + 
    theme(legend.position = "none") + 
    labs(title = mean_label, 
         x = "n", y = "mean")
  
  # 標本分散の推移を作図
  var_graph <- ggplot() + 
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = var_x, color = factor(group_id))) + # 標本分散:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = var_x, color = factor(group_id))) + # 標本分散:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(0, max_sd^2)) + 
    theme(legend.position = "none") + 
    labs(title = var_label, 
         x = "n", y = "variance")
  
  # 標本標準偏差の推移を作図
  sd_graph <- ggplot() + 
    geom_line(data = tmp_sample_df, 
              mapping = aes(x = n, y = sd_x, color = factor(group_id))) + # 標本標準偏差:(推移)
    geom_point(data = tmp_param_df, 
               mapping = aes(x = n, y = sd_x, color = factor(group_id))) + # 標本標準偏差:(点)
    coord_cartesian(xlim = c(0, sample_size), ylim = c(0, max_sd)) + 
    theme(legend.position = "none") + 
    labs(title = sd_label, 
         x = "n", y = "standard deviation")
  
  # 並べて描画
  graph <- patchwork::wrap_plots(
    hist_graph, mean_graph, var_graph, sd_graph, 
    ncol = 1, heights = c(1, 0.5, 0.5, 0.5)
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 800, height = 1200, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "figure/ch4_1_3/caucy.gif", delay = 0.1) -> tmp_path # gifファイル書き出し


