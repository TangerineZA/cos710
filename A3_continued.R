loaded_times <- readRDS("times_list.RData")
loaded_ge_list <- readRDS("ge_list.RData")

for (time in loaded_times) {
   print(time)
}