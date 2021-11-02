#s3calc.R
neutral = "gray90"
makeS3data <- function(neutral) {
  N <- 12
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btnI","I",neutral)
  DF[2,] <- c("btn_R1","(123456)",neutral)
  DF[3,] <- c("btn_R2","(135)(246)",neutral)
  DF[4,] <- c("btn_R3","(14)(25)(36)",neutral)
  DF[5,] <- c("btn_R4","(153)(264)",neutral)
  DF[6,] <- c("btn_R5","(165432)",neutral)
  DF[7,] <- c("btn_r0","(26)(35)",neutral)
  DF[8,] <- c("btn_r1","(12)(36)(45)",neutral)
  DF[9,] <- c("btn_r2","(13)(46)",neutral)
  DF[10,] <- c("btn_r3","(14)(23)(56)",neutral)
  DF[11,] <- c("btn_r4","(15)(24)",neutral)
  DF[12,] <- c("btn_r5","(16)(25)(34)",neutral)
  return(DF)
}

DF <- makeS3data(neutral)
DF
