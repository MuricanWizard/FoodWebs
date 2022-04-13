library('tidyverse')
library('ggplot2')

data1 <- read_csv('pop_data_C0.3_P0.2.csv', col_names = FALSE)
View(data1)
data1 <- data1 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data1 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.2, S = 100")

data2 <- read_csv('pop_data_C0.3_P0.5.csv', col_names = FALSE)
View(data2)
data2 <- data2 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data2 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.5, S = 100")

data3 <- read_csv('pop_data_C0.3_P0.7.csv', col_names = FALSE)
View(data3)
data3 <- data3 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data3 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.7, S = 100")

data4 <- read_csv('pop_data_C0.3_P0.1.csv', col_names = FALSE)
View(data4)
data4 <- data4 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data4 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.1, S = 100")

data5 <- read_csv('pop_data_C0.3_P0.3.csv', col_names = FALSE)
View(data5)
data5 <- data5 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data5 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.3, S = 100")

data6 <- read_csv('pop_data_C0.3_P0.4.csv', col_names = FALSE)
View(data6)
data6 <- data6 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data6 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.4, S = 100")

data7 <- read_csv('pop_data_C0.3_P0.6.csv', col_names = FALSE)
View(data7)
data7 <- data7 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data7 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.6, S = 100")

data8 <- read_csv('pop_data_C0.3_P0.8.csv', col_names = FALSE)
View(data8)
data8 <- data8 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data8 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.8, S = 100")

data9 <- read_csv('pop_data_C0.3_P0.9.csv', col_names = FALSE)
View(data9)
data9 <- data9 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data9 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.9, S = 100")

data10 <- read_csv('pop_data_C0.3_P1.0.csv', col_names = FALSE)
View(data10)
data10 <- data10 %>% group_by(X1) %>% mutate(meanPop = mean(X2:X31))
data10 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanPop)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 1.0, S = 100")

data_1_merge <- data1 %>% select(meanPop) %>% mutate(p = 0.2) %>% ungroup()
data_2_merge <- data2 %>% select(meanPop) %>% mutate(p = 0.5) %>% ungroup()
data_3_merge <- data3 %>% select(meanPop) %>% mutate(p = 0.7) %>% ungroup()
data_4_merge <- data4 %>% select(meanPop) %>% mutate(p = 0.1) %>% ungroup()
data_5_merge <- data5 %>% select(meanPop) %>% mutate(p = 0.3) %>% ungroup()
data_6_merge <- data6 %>% select(meanPop) %>% mutate(p = 0.4) %>% ungroup()
data_7_merge <- data7 %>% select(meanPop) %>% mutate(p = 0.6) %>% ungroup()
data_8_merge <- data8 %>% select(meanPop) %>% mutate(p = 0.8) %>% ungroup()
data_9_merge <- data9 %>% select(meanPop) %>% mutate(p = 0.9) %>% ungroup()
data_10_merge <- data10 %>% select(meanPop) %>% mutate(p = 1.0) %>% ungroup()

#Population Assembly Graph grouped by p
merged_data <- data_1_merge %>% add_row(data_2_merge)
merged_data <- merged_data %>% add_row(data_3_merge)
merged_data <- merged_data %>% add_row(data_4_merge)
merged_data <- merged_data %>% add_row(data_5_merge)
merged_data <- merged_data %>% add_row(data_6_merge)
merged_data <- merged_data %>% add_row(data_7_merge)
merged_data <- merged_data %>% add_row(data_8_merge)
merged_data <- merged_data %>% add_row(data_9_merge)
merged_data <- merged_data %>% add_row(data_10_merge)
merged_data %>% mutate(p = p %>% as.character()) %>% group_by(p) %>% ggplot() +
  geom_line(mapping=aes(x = X1, y = meanPop, color = p)) + 
  labs(x = "Time", y = "mean population", title = "S = 100, Connectance = 0.3")

#Max population graph for each p
groups <- merged_data %>% group_by(p) %>% group_split()
p_values <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
max_pops <- c()
for(i in groups){
  max_pops <- append(max_pops, i %>% pull('meanPop') %>% max())
}
max_pop_by_p <- tibble("p" = p_values, "max_pop" = max_pops)

#Scatterplot
max_pop_by_p %>% ggplot() + geom_point(mapping = aes(x = p, y = max_pop)) +
  labs(x = "p", y = "Peak Assembly Population", title = "Peak Assembly Population against p")

#Line chart
max_pop_by_p %>% ggplot() + geom_line(mapping = aes(x = p, y = max_pop)) +
  labs(x = "p", y = "Peak Assembly Population", title = "Peak Assembly Population against p")

#Time against half population graph
half_pops <- max_pops/2
half_pops_assemblyTime <- c()
for(i in 1:length(groups)){
  tempPops <- groups[[i]] %>% pull('meanPop')
  tempIndices <- groups[[i]] %>% pull('X1')
  for(j in 1:length(tempPops)){
    if(tempPops[j] >= half_pops[i]){
      half_pops_assemblyTime <- append(half_pops_assemblyTime, tempIndices[j])
      break
    }
  }
}
half_pops_assemblyTime_by_p <- tibble("p" = p_values, "half_pops_assemblyTime" = half_pops_assemblyTime)
half_pops_assemblyTime_by_p %>% ggplot() + geom_point(mapping = aes(x = p, y = half_pops_assemblyTime)) +
  labs(x = "p", y = "Time", title = "Time to assemble half population against p")

#diversity graph
data1 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.1_p0.1.csv', col_names = FALSE)
View(data1)
data1 <- data1 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data1 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.1, P = 0.1, S = 100")

data2 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.1_p0.5.csv', col_names = FALSE)
View(data2)
data2 <- data2 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data2 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.1, P = 0.5, S = 100")

data3 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.1_p1.0.csv', col_names = FALSE)
View(data3)
data3 <- data3 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data3 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.1, P = 1.0, S = 100")

data4 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.2_p0.1.csv', col_names = FALSE)
View(data4)
data4 <- data4 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data4 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.2, P = 0.1, S = 100")

data5 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.2_p0.5.csv', col_names = FALSE)
View(data5)
data5 <- data5 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data5 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.2, P = 0.5, S = 100")

data6 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.2_p1.0.csv', col_names = FALSE)
View(data6)
data6 <- data6 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data6 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.2, P = 1.0, S = 100")

data7 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.3_p0.1.csv', col_names = FALSE)
View(data7)
data7 <- data7 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data7 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.1, S = 100")

data8 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.3_p0.5.csv', col_names = FALSE)
View(data8)
data8 <- data8 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data8 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 0.5, S = 100")

data9 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.3_p1.0.csv', col_names = FALSE)
View(data9)
data9 <- data9 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data9 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.3, P = 1.0, S = 100")

data10 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.4_p0.1.csv', col_names = FALSE)
View(data10)
data10 <- data10 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data10 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.4, P = 0.1, S = 100")

data11 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.4_p0.5.csv', col_names = FALSE)
View(data11)
data11 <- data11 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data11 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.4, P = 0.5, S = 100")

data12 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.4_p1.0.csv', col_names = FALSE)
View(data12)
data12 <- data12 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data12 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.4, P = 1.0, S = 100")

data13 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.45_p0.1.csv', col_names = FALSE)
View(data13)
data13 <- data13 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data13 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.45, P = 0.1, S = 100")

data14 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.45_p0.5.csv', col_names = FALSE)
View(data14)
data14 <- data14 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data14 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.45, P = 0.5, S = 100")

data15 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.45_p1.0.csv', col_names = FALSE)
View(data15)
data15 <- data15 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data15 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.45, P = 1.0, S = 100")

data16 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.05_p0.1.csv', col_names = FALSE)
View(data16)
data16 <- data16 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data16 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.05, P = 0.1, S = 100")

data17 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.05_p0.5.csv', col_names = FALSE)
View(data17)
data17 <- data17 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data17 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.05, P = 0.5, S = 100")

data18 <- read_csv('C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/diversityData_C0.05_p1.0.csv', col_names = FALSE)
View(data18)
data18 <- data18 %>% group_by(X1) %>% mutate(meanBeta = mean(X2:X4))
data18 %>% ggplot() + geom_line(mapping=aes(x = X1, y = meanBeta)) +
  labs(x = "Time", y = "Population", title = "Connectance = 0.05, P = 1.0, S = 100")

C_0.1_data_1_merge <- data1 %>% select(meanBeta) %>% mutate(p = 0.1) %>% ungroup()
C_0.1_data_2_merge <- data2 %>% select(meanBeta) %>% mutate(p = 0.5) %>% ungroup()
C_0.1_data_3_merge <- data3 %>% select(meanBeta) %>% mutate(p = 1.0) %>% ungroup()

C_0.2_data_4_merge <- data4 %>% select(meanBeta) %>% mutate(p = 0.1) %>% ungroup()
C_0.2_data_5_merge <- data5 %>% select(meanBeta) %>% mutate(p = 0.5) %>% ungroup()
C_0.2_data_6_merge <- data6 %>% select(meanBeta) %>% mutate(p = 1.0) %>% ungroup()

C_0.3_data_7_merge <- data7 %>% select(meanBeta) %>% mutate(p = 0.1) %>% ungroup()
C_0.3_data_8_merge <- data8 %>% select(meanBeta) %>% mutate(p = 0.5) %>% ungroup()
C_0.3_data_9_merge <- data9 %>% select(meanBeta) %>% mutate(p = 1.0) %>% ungroup()

C_0.4_data_10_merge <- data10 %>% select(meanBeta) %>% mutate(p = 0.1) %>% ungroup()
C_0.4_data_11_merge <- data11 %>% select(meanBeta) %>% mutate(p = 0.5) %>% ungroup()
C_0.4_data_12_merge <- data12 %>% select(meanBeta) %>% mutate(p = 1.0) %>% ungroup()

C_0.45_data_13_merge <- data13 %>% select(meanBeta) %>% mutate(p = 0.1) %>% ungroup()
C_0.45_data_14_merge <- data14 %>% select(meanBeta) %>% mutate(p = 0.5) %>% ungroup()
C_0.45_data_15_merge <- data15 %>% select(meanBeta) %>% mutate(p = 1.0) %>% ungroup()

C_0.05_data_16_merge <- data16 %>% select(meanBeta) %>% mutate(p = 0.1) %>% ungroup()
C_0.05_data_17_merge <- data17 %>% select(meanBeta) %>% mutate(p = 0.5) %>% ungroup()
C_0.05_data_18_merge <- data18 %>% select(meanBeta) %>% mutate(p = 1.0) %>% ungroup()

#Diversity Assembly Graph grouped by p

#C0.1
merged_data_C0.1 <- C_0.1_data_1_merge %>% add_row(C_0.1_data_2_merge)
merged_data_C0.1 <- merged_data %>% add_row(C_0.1_data_3_merge)
merged_data_C0.1 %>% mutate(p = p %>% as.character()) %>% group_by(p) %>% ggplot() +
  geom_line(mapping=aes(x = X1, y = meanBeta, color = p)) + 
  labs(x = "Time", y = "mean beta", title = "S = 100, Connectance = 0.1")

#C0.2
merged_data_C0.2 <- C_0.2_data_4_merge %>% add_row(C_0.2_data_5_merge)
merged_data_C0.2 <- merged_data %>% add_row(C_0.2_data_6_merge)
merged_data_C0.2 %>% mutate(p = p %>% as.character()) %>% group_by(p) %>% ggplot() +
  geom_line(mapping=aes(x = X1, y = meanBeta, color = p)) + 
  labs(x = "Time", y = "mean beta", title = "S = 100, Connectance = 0.2")

#C0.3
merged_data_C0.3 <- C_0.3_data_7_merge %>% add_row(C_0.3_data_8_merge)
merged_data_C0.3 <- merged_data %>% add_row(C_0.3_data_9_merge)
merged_data_C0.3 %>% mutate(p = p %>% as.character()) %>% group_by(p) %>% ggplot() +
  geom_line(mapping=aes(x = X1, y = meanBeta, color = p)) + 
  labs(x = "Time", y = "mean beta", title = "S = 100, Connectance = 0.3")

#C0.4
merged_data_C0.4 <- C_0.4_data_10_merge %>% add_row(C_0.4_data_11_merge)
merged_data_C0.4 <- merged_data %>% add_row(C_0.4_data_12_merge)
merged_data_C0.4 %>% mutate(p = p %>% as.character()) %>% group_by(p) %>% ggplot() +
  geom_line(mapping=aes(x = X1, y = meanBeta, color = p)) + 
  labs(x = "Time", y = "mean beta", title = "S = 100, Connectance = 0.4")

#C0.45
merged_data_C0.45 <- C_0.45_data_13_merge %>% add_row(C_0.45_data_14_merge)
merged_data_C0.45 <- merged_data %>% add_row(C_0.45_data_15_merge)
merged_data_C0.45 %>% mutate(p = p %>% as.character()) %>% group_by(p) %>% ggplot() +
  geom_line(mapping=aes(x = X1, y = meanBeta, color = p)) + 
  labs(x = "Time", y = "mean beta", title = "S = 100, Connectance = 0.45")

#C0.05
merged_data_C0.05 <- C_0.05_data_16_merge %>% add_row(C_0.05_data_17_merge)
merged_data_C0.05 <- merged_data %>% add_row(C_0.05_data_18_merge)
merged_data_C0.05 %>% mutate(p = p %>% as.character()) %>% group_by(p) %>% ggplot() +
  geom_line(mapping=aes(x = X1, y = meanBeta, color = p)) + 
  labs(x = "Time", y = "mean beta", title = "S = 100, Connectance = 0.05")


setwd("C:/Users/Akhil/Documents/Notes Bio Research/modifiedData0226/")
#Population plots
c_vec <- c(0.05,0.1,0.2,0.3, 0.4, 0.45)

par(mfrow = c(2,3))
for (i in 1:length(c_vec)){
  c_val <- c_vec[i]
  
  data1 <- read.csv(sprintf('pop_data_C%g_p0.1_iteration1.csv', c_val), h = F)
  data2 <- read.csv(sprintf('pop_data_C%g_p0.5_iteration1.csv', c_val), h = F)
  data3 <- read.csv(sprintf('pop_data_C%g_p1_iteration1.csv', c_val), h = F)
  
  plot(rowMeans(data1[,2:31]), type = 'l', lwd = 2, col = 1 , ylab = 'Number of species', xlab = 'time', ylim = c(0,100))
  points(rowMeans(data2[,2:31]), type = 'l', lwd = 2, col = 2 )
  points(rowMeans(data3[,2:31]), type = 'l', lwd = 2, col = 4  )
  abline(h = 1, lty = 2)
  
  legend('bottomright', legend = c('p=0.1', 'p=0.5', 'p=1.0'), lwd = 2, col = c(1,2,4), box.lwd = 0)
  legend('top', legend = paste("C = ", c_val), box.lwd = 0)
  
}

#Time to half population plots
c_vec <- c(0.05,0.1,0.2,0.3, 0.4, 0.45)

par(mfrow = c(2,3))
for (i in 1:length(c_vec)){
  c_val <- c_vec[i]
  
  data1 <- read.csv(sprintf('pop_data_C%g_p0.1_iteration1.csv', c_val), h = F)
  data2 <- read.csv(sprintf('pop_data_C%g_p0.5_iteration1.csv', c_val), h = F)
  data3 <- read.csv(sprintf('pop_data_C%g_p1_iteration1.csv', c_val), h = F)
  
  plot(rowMeans(data1[,2:31]), type = 'l', lwd = 2, col = 1 , ylab = 'Population', xlab = 'time', ylim = c(0,100))
  points(rowMeans(data2[,2:31]), type = 'l', lwd = 2, col = 2 )
  points(rowMeans(data3[,2:31]), type = 'l', lwd = 2, col = 4  )
  abline(h = 1, lty = 2)
  
  legend('right', legend = c('p=0.1', 'p=0.5', 'p=1.0'), lwd = 2, col = c(1,2,4), box.lwd = 0)
  legend('top', legend = paste("C = ", c_val), box.lwd = 0)
  
}

#Diversity plots
#Gamma plots
setwd("C:/Users/Akhil/Documents/Notes Bio Research/diversityData0308/")
c_vec <- c(0.05,0.1,0.2,0.3,0.4,0.45)

par(mfrow = c(2,3))
for (i in 1:length(c_vec)){
  c_val <- c_vec[i]
  
  data1 <- read.csv(sprintf('GammadiversityData_C%g_p0.1.csv', c_val), h = F)
  data2 <- read.csv(sprintf('GammadiversityData_C%g_p0.5.csv', c_val), h = F)
  data3 <- read.csv(sprintf('GammadiversityData_C%g_p1.0.csv', c_val), h = F)
  
  plot(rowMeans(data1[,2:4]), type = 'l', lwd = 2, col = 1 , ylab = 'Gamma', xlab = 'time', ylim = c(0,100))
  points(rowMeans(data2[,2:4]), type = 'l', lwd = 2, col = 2 )
  points(rowMeans(data3[,2:4]), type = 'l', lwd = 2, col = 4  )
  abline(h = 1, lty = 2)
  
  legend('right', legend = c('p=0.1', 'p=0.5', 'p=1.0'), lwd = 2, col = c(1,2,4), box.lwd = 0)
  legend('top', legend = paste("C = ", c_val), box.lwd = 0)
  
}

#Diversity plots
#Beta plots
setwd("C:/Users/Akhil/Documents/Notes Bio Research/diversityData0303/")
c_vec <- c(0.05,0.1,0.2,0.3,0.4,0.45)

par(mfrow = c(2,3))
for (i in 1:length(c_vec)){
  c_val <- c_vec[i]
  
  data1 <- read.csv(sprintf('diversityData_C%g_p0.1.csv', c_val), h = F)
  data2 <- read.csv(sprintf('diversityData_C%g_p0.5.csv', c_val), h = F)
  data3 <- read.csv(sprintf('diversityData_C%g_p1.0.csv', c_val), h = F)
  
  plot(rowMeans(data1[,2:4]), type = 'l', lwd = 2, col = 1 , ylab = 'Beta', xlab = 'time', ylim = c(0,10))
  points(rowMeans(data2[,2:4]), type = 'l', lwd = 2, col = 2 )
  points(rowMeans(data3[,2:4]), type = 'l', lwd = 2, col = 4  )
  abline(h = 1, lty = 2)
  
  legend('right', legend = c('p=0.1', 'p=0.5', 'p=1.0'), lwd = 2, col = c(1,2,4), box.lwd = 0)
  legend('top', legend = paste("C = ", c_val), box.lwd = 0)
  
}
