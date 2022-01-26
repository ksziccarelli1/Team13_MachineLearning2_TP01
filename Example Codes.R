# We can use list, vectors, data frame, etc. as iterator in a for loop
dice_list <- list(1, 2, 3, 4, 5, 6)
class(dice_list)
for (i in dice_list){
  print(i)
}

dice_vector <- c(1, 2, 3, 4, 5, 6)
class(dice_vector)
for (i in dice_vector){
  print(i)
}

dice_dataframe <- data.frame(1, 2, 3, 4, 5, 6)
class(dice_dataframe)  
for (i in dice_dataframe){
  print(i)
}


# We can also put a for loop in another for loop
plot(0, 0, type = "n", xlim = c(0, 9), ylim = c(0, 9), 
     xlab = "", ylab = "", axes = F)

for (i in 1:8) {
  for (j in 1:8) {
    if ((i + j) %% 2 == 0) {
      polygon(c(i, i, i - 1, i - 1), c(j, j - 1, j - 1, j), col = "green")
    } else {
      polygon(c(i, i, i - 1, i - 1), c(j, j - 1, j - 1, j), col = "yellow")
    }
  }
}
