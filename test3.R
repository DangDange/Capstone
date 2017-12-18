Raw_data <- read.csv("realcrimedata.csv", sep = ",", header = TRUE)
Raw_data_Instance_Size <- nrow(Raw_data)

# Raw_data <- transform(Raw_data,
#                       y2 = ifelse( CrimeNum > 80000,"1",
#                                    ifelse(CrimeNum >= 60000 & CrimeNum < 80000, "2",
#                                           ifelse(CrimeNum >= 20000 & CrimeNum < 60000, "3",
#                                                  ifelse(CrimeNum >= 10000 & CrimeNum < 20000, "4",
#                                                         ifelse(CrimeNum >= 5000 & CrimeNum < 10000, "5",
#                                                                ifelse(CrimeNum >= 4000 & CrimeNum < 5000, "6",
#                                                                       ifelse(CrimeNum >= 3000 & CrimeNum < 4000, "7",
#                                                                              ifelse(CrimeNum >= 2000 & CrimeNum < 3000, "8",
#                                                                                     ifelse(CrimeNum >= 1000 & CrimeNum < 2000, "9", "10"))))))))));
# 
# 
# 
# 
# Raw_data <- transform(Raw_data,
#                       y2 = ifelse( CrimeNum > 80000,"1",
#                                    ifelse(CrimeNum >= 60000 & CrimeNum < 80000, "2",
#                                           ifelse(CrimeNum >= 20000 & CrimeNum < 60000, "3",
#                                                  ifelse(CrimeNum >= 10000 & CrimeNum < 20000, "4",
#                                                         ifelse(CrimeNum >= 5000 & CrimeNum < 10000, "5",
#                                                                ifelse(CrimeNum >= 4000 & CrimeNum < 5000, "6",
#                                                                       ifelse(CrimeNum >= 3000 & CrimeNum < 4000, "7",
#                                                                              ifelse(CrimeNum >= 2000 & CrimeNum < 3000, "8",
#                                                                                     ifelse(CrimeNum >= 1000 & CrimeNum < 2000, "9", "10"))))))))));
# 
# 
# 
# 
# 
# 

Raw_data <- transform(Raw_data,
                      y2 = ifelse( CrimeNum > 20000,"1",
                                   ifelse(CrimeNum >= 5000 & CrimeNum < 20000, "2",
                                        ifelse(CrimeNum >= 3500 & CrimeNum < 5000, "3","4"))));
                                          






Raw_data$y2 <- as.factor(Raw_data$y2)

Raw_data_n <- cbind(Raw_data[,2],Raw_data[,3])
Numeric_y <- Raw_data$y2



Training_data_Instance_Size<-nrow(Raw_data_n)

folds1 <- Raw_data_n[ 0:(Training_data_Instance_Size*0.2)+1 ,] 
folds2 <- Raw_data_n[ (Training_data_Instance_Size*0.2):(Training_data_Instance_Size*0.4) ,] 
folds3 <- Raw_data_n[ (Training_data_Instance_Size*0.4):(Training_data_Instance_Size*0.6) ,] 
folds4 <- Raw_data_n[ (Training_data_Instance_Size*0.6):(Training_data_Instance_Size*0.8) ,] 
folds5 <- Raw_data_n[ (Training_data_Instance_Size*0.8):(Training_data_Instance_Size*1.0) ,] 
test1 <-rbind(folds2,folds3,folds4,folds5)
test2 <-rbind(folds1,folds3,folds4,folds5)
test3 <-rbind(folds1,folds2,folds4,folds5)
test4 <-rbind(folds1,folds2,folds3,folds5)
test5 <-rbind(folds2,folds3,folds4,folds1)

folds1_y <- Numeric_y[ 0:(Training_data_Instance_Size*0.2)+1] 
folds2_y <- Numeric_y[ (Training_data_Instance_Size*0.2):(Training_data_Instance_Size*0.4)] 
folds3_y <- Numeric_y[ (Training_data_Instance_Size*0.4):(Training_data_Instance_Size*0.6)] 
folds4_y <- Numeric_y[ (Training_data_Instance_Size*0.6):(Training_data_Instance_Size*0.8)] 
folds5_y <- Numeric_y[ (Training_data_Instance_Size*0.8):(Training_data_Instance_Size*1.0)] 

test1_y <-rbind(folds2_y,folds3_y,folds4_y,folds5_y)

test2_y <-rbind(folds1_y,folds3_y,folds4_y,folds5_y)
test3_y <-rbind(folds1_y,folds2_y,folds4_y,folds5_y)
test4_y <-rbind(folds1_y,folds2_y,folds3_y,folds5_y)
test5_y <-rbind(folds2_y,folds3_y,folds4_y,folds5_y)




library(class)
acc<-matrix(1:20, nrow=20)
 


for (i in 1:20){
  predict1 <- knn(train = test1 ,test = folds1, test1_y, k=i)
  new_predict1<-factor(predict1, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  new_folds1_y<-factor(folds1_y, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  acc1 <- sum(new_predict1==new_folds1_y)/length(folds1_y)

  predict2 <- knn(train = test2 ,test = folds2, test2_y, k=i)
  new_predict2<-factor(predict2, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  new_folds2_y<-factor(folds2_y, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  acc2 <- sum(new_predict2==new_folds2_y)/length(folds2_y)

  predict3 <- knn(train = test3 ,test = folds3, test3_y, k=i)
  new_predict3<-factor(predict3, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  new_folds3_y<-factor(folds3_y, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  acc3 <- sum(new_predict3==new_folds3_y)/length(folds3_y)


  predict4 <- knn(train = test4 ,test = folds4, test4_y, k=i)
  new_predict4<-factor(predict4, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  new_folds4_y<-factor(folds4_y, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  acc4 <- sum(new_predict4==new_folds4_y)/length(folds4_y)

  predict5 <- knn(train = test5 ,test = folds5, test5_y, k=i)
  new_predict5<-factor(predict5, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  new_folds5_y<-factor(folds5_y, order = FALSE, level = c(1,2,3,4,5,6,7,8,9,10))
  acc5 <- sum(new_predict5==new_folds5_y)/length(folds5_y)

  acc[i]<-(acc1+acc2+acc3+acc4+acc5)/5

}


index<-0
min1<-1
for(i in 1:20){
  if(min1>1-acc[i])
  {
    min1<-1-acc[i]
    index<-i
  }
}

 args<-commandArgs(TRUE)
 a<- as.double(args[1])
 b<- as.double(args[2])



test_n1<-matrix(c(a,b),nrow=1, ncol=2)
# test_n1<-matrix(c(37.594,127.988),nrow=1, ncol=2)
predict <- knn(train = Raw_data_n ,test =test_n1, Numeric_y, k=index)
print(toString(predict[1]))

