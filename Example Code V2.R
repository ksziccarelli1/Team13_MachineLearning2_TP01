# 1 Wrong syntax for nested loop

for (i in 1:10){
  for (j in 1:i){
    print("Run successfully if you see this message")
}

#2 put break in correct position
  
for (i in 1:9){ # No. of rows
  x <- c()
  for (j in 1:i){ # No. of columns
    x <- c(x, i * j) # row num * col num
    
    if (i * j > 30){ # Drop any result > 30
      break
    }
    
  }
  print(x)
}

#3 
# initialize canvas 
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), xlab = "", ylab = "", axes = F)
# draw checkboard
for (i in 1:8) {
  for (j in 1:8) {
    if (j %% 2 == 0) { # should be (i + j) %% 2 == 0
      polygon(c(i, i, i - 1, i - 1), c(j, j - 1, j - 1, j), col = "green")
    } else {
      polygon(c(i, i, i - 1, i - 1), c(j, j - 1, j - 1, j), col = "yellow")
    }
  }
}
