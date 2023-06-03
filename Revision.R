source("data_preparation.R")

sort(train$id)
sort(test$id)

result <- c()
for (i in 1:length(train$id)) {
  for(j in 1:length(test$id)){
    result <- append(result, train$id[i] == test$id[j])
  }
}
table(result)

contador <- 0
for (i in 1:nrow(data)){
  if (data$Standard.Value[i] >= 11000){
    contador <- contador + 1
  }
}
contador
table(norm_data$label)
