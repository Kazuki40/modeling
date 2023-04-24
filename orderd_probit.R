# library -----------------------------------------------------------------

source("estimate_parameter_function.R")

# preparation -------------------------------------------------------------
# データを読み込む（data.frameの名称は"データ名"とする）
data <-
  read.csv("data_md.csv", header = TRUE, fileEncoding = "CP932")


# para_0 <-c(numeric(24), c(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5))


para_0 <- c(numeric(24),  3 * 2.220446e-16 * 0:9)
#para_0 <- result$all$par
data_length <- length(data[, 1])

judge <- 0

# function ----------------------------------------------------------------


# 目的関数の定義
oderd_func <- function(para) {
  ans <- 0
  
  b <- para[1]
  for (i in 2:24) {
    b <- c(b, para[i])
  }
  b <- t(b)
  
  k <- -10 ^ 7
  for (i in 25:34) {
    k <- c(k, para[i])
  }
  k <- c(k, 10 ^ 7)
  
  if ((k[1] < k[2]) &
      (k[2] < k[3]) &
      (k[3] < k[4]) &
      (k[4] < k[5]) &
      (k[5] < k[6]) &
      (k[6] < k[7]) &
      (k[7] < k[8]) &
      (k[8] < k[9]) &
      (k[9] < k[10]) &
      (k[10] < k[11]) &
      (k[11] < k[12])) {
    for (i in 1:data_length) {
      x <- c(
        data$V1[i],
        data$V2[i],
        data$V3[i],
        data$V4[i],
        data$V5[i],
        data$V6[i],
        data$V7[i],
        data$V8[i],
        data$V9[i],
        data$V10[i],
        data$V11[i],
        data$V12[i],
        data$V13[i],
        data$V14[i],
        data$V15[i],
        data$V16[i],
        data$V17[i],
        data$V18[i],
        data$V19[i],
        data$V20[i],
        data$V21[i],
        data$V22[i],
        data$V23[i],
        data$V24[i]
      )
      x <- t(t(x))
      
      u_i <- b %*% x
      
      if (data$y[i] == 1) {
        p_i <- pnorm(k[2] - u_i, mean = 0, sd = 1) - 0
        
      } else if (data$y[i] == 11) {
        p_i <- 1 - pnorm(k[11] - u_i, mean = 0, sd = 1)
        
      } else{
        p_i <-
          pnorm(k[data$y[i] + 1] - u_i, mean = 0, sd = 1) - pnorm(k[data$y[i]] - u_i, mean = 0, sd = 1)
        
      }
      ans <- ans + log(p_i)
      #browser()
    }
    
  } else {
    ans <- -10 ^ 7
    #browser()
  }
  
  if (ans == -Inf) {
    ans <= -10 ^ 7
  }
  
  return(ans)
}


# optim -------------------------------------------------------------------

result <- estimate_parameter_function(oderd_func, para_0)
result


# test --------------------------------------------------------------------

# VGAMパッケージをインストールする
#install.packages("VGAM")

# VGAMパッケージを読み込む
library(VGAM)

# データを作成する（ここではirisデータセットを使用する）
data <-
  read.csv("data_md.csv", header = TRUE, fileEncoding = "CP932")


# オーダープロビットモデルを推定する
opm_fit <-
  vglm(
    y ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 +
      V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24,
    data = data,
    family = cumulative(parallel = TRUE),
    link = "probit"
  )

# 推定結果を表示する
summary(opm_fit)
