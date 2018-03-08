tsteps <- 50
nfeatures <- 40
nstats <- 30

input <- layer_input(shape=list(tsteps*nfeatures*nstats), name = "input")

net <- input %>% 
            layer_reshape(target_shape = c(tsteps, nfeatures*nstats)) %>%
            layer_lstm(units = nfeatures, return_sequences = TRUE) %>%
            layer_lstm(units = nfeatures, return_sequences = TRUE) %>%
            layer_lstm(units = nfeatures) %>%
            layer_dense(units = nfeatures) %>%
            layer_dense(units = floor(nfeatures/2)) %>%
            layer_dense(units = floor(nfeatures/5))
#  

win_out <- net %>% layer_dense(2, activation = "softmax", name = "win_out")
score_out <- net %>% layer_dense(2*5, name = "score_out")
stats_out <- net %>% layer_dense(2*11, name = "stats_out")

model <- keras_model(inputs = input, outputs = list(win_out,score_out,stats_out))










