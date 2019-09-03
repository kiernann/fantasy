gaa <- read.csv("https://raw.githubusercontent.com/k5cents/GAA/master/scores.csv")
team <- gaa$kier
opp10 <- gaa$coln
opp11 <- gaa$pepe
opp12 <- gaa$kyle

w10 <- pnorm(
  mean((subset(opp10, gaa$year == 3)), na.rm = TRUE),
  mean = mean((subset(team, gaa$year == 3)), na.rm = TRUE),
  sd = sd((subset(team, gaa$year == 3)), na.rm = TRUE),
  lower.tail = FALSE)
  
w11 <- pnorm(
  mean((subset(opp11, gaa$year == 3)), na.rm = TRUE),
  mean = mean((subset(team, gaa$year == 3)), na.rm = TRUE),
  sd = sd((subset(team, gaa$year == 3)), na.rm = TRUE),
  lower.tail = FALSE)
  
w12 <- pnorm(
  mean((subset(opp12, gaa$year == 3)), na.rm = TRUE),
  mean = mean((subset(team, gaa$year == 3)), na.rm = TRUE),
  sd = sd((subset(team, gaa$year == 3)), na.rm = TRUE),
  lower.tail = FALSE)
  
undefeat <- w10 * w11 * w12
print(undefeat)