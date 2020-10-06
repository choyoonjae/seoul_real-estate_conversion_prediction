#####See line 45#####

######################라이브러리######################
{
  library(caret)
  library(reshape)
  library(nnet)
  library(tree)
  library(e1071)
  library(class)
  library(gbm)
}
######################################################


######################함수선언########################
{
  minmaxscale <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  
  rank2group <- function (y, k=4){
    count=length(y)
    z=rank(y,ties.method="min")
    return(floor((z-1)/(count/k))+1)
  }
}
######################################################

###################데이터불러오기#####################
{
  setwd("C:/Users/CHO/Desktop/윤재/서울연구원") 
  bdata <- read.csv("dataset2.csv")
}
######################################################

#TODO LIST : 1. NA omit(변수 내에서 NA 비율 몇일 때 변수제거, NA포함 케이스 제거 or 평균 or 최빈값)
#            2. Factor형 변수 encoding             
######################################################

#############factor형 변수 index 선택#################
variable_name <- colnames(bdata)
variable_index <- c(1:length(colnames(bdata)))
(btable <- cbind(variable_name,variable_index))

factor_address <- c(1,2,3,7) ##factor형 변수의 index 입력
input_factor_data <- subset(bdata,
                            select = factor_address)

for(q in 1:length(factor_address)){ ## factor로 변수형 변환
  input_factor_data[[q]] <- as.numeric(factor(input_factor_data[[q]]))
  input_factor_data[[q]]<- as.factor(input_factor_data[[q]])
  print(q)
}
######################################################

#factor형 변수 제외한 input data, target data 나누기##
{
  target_name <- vector()
  target_name <- "buldyn"
  
  col_list <- vector()
  col_list <- c(1:length(colnames(bdata)))
  target_address <- vector()
  target_address <- grep(target_name,colnames(bdata),fixed = T)
  sep_address <- vector()
  sep_address <- grep('Sep',colnames(bdata),fixed = T)
  remove_list <- vector()
  remove_list <- c(target_address,sep_address,factor_address)
  col_list <- setdiff(col_list,remove_list)
  
  input_data <- data.frame()
  input_data <- subset(bdata,
                       select = col_list)
  
  target_data <- data.frame()
  target_data <- subset(bdata,
                        select=target_address)
}
######################################################

#############target 5:5 주소값 생성###################
{
  target_string <- vector()
  target_string <- as.character(target_data$buldyn)
  target_address <- vector()
  target_address <- grep('1',target_string,fixed = T)
  nontarget_address <- vector()
  nontarget_address <- vector()
  nontarget_address <- grep('0',target_string,fixed = T)
  nontarget_address <- sample(nontarget_address,length(target_address))
}
######################################################

#################CASE_ID for dataset 생성#############
CASE_ID <- c(target_address,nontarget_address)
######################################################

##############factor_dataset 생성#####################
{
  data_1 <- data.frame()
  data_2 <- data.frame()
  data_1 <- input_factor_data[target_address,]
  data_2 <- input_factor_data[nontarget_address,]
  input_factor_data <- rbind(data_1,data_2)
}
######################################################

##############taget_dataset 생성######################
{
  data_1 <- data.frame()
  data_2 <- data.frame()
  
  data_1 <- target_data[target_address,]
  data_2 <- target_data[nontarget_address,]
  target_data <- c(data_1, data_2)
}
######################################################

##############raw input data 생성#####################
{
  raw_input_data <- vector()
  for(i in 1:length(colnames(input_data))){
    basket <- vector()
    basket <- input_data[[i]]
    raw_input_data <- cbind(raw_input_data,basket)
  }
  raw_input_colname <- paste(colnames(input_data),'_raw',sep="")
  colnames(raw_input_data) <- raw_input_colname
  raw_input_data <- as.data.frame(raw_input_data)
  data_1 <- data.frame()
  data_2 <- data.frame()
  data_1 <- raw_input_data[target_address,]
  data_2 <- raw_input_data[nontarget_address,]
  raw_input_data <- rbind(data_1,data_2)
  raw_input_data <- cbind(raw_input_data,input_factor_data)
  
}
######################################################

#############Minmax dataset 생성######################
{
  minmax_data <- vector()
  for(i in 1:length(colnames(input_data))){
    basket <- vector()
    basket <- input_data[[i]]
    basket <- minmaxscale(basket)
    minmax_data <- cbind(minmax_data,basket)
  }
  minmax_colname <- paste(colnames(input_data),'_minmax',sep="")
  colnames(minmax_data) <- minmax_colname
  minmax_data <- as.data.frame(minmax_data)
  data_1 <- data.frame()
  data_2 <- data.frame()
  data_1 <- minmax_data[target_address,]
  data_2 <- minmax_data[nontarget_address,]
  minmax_data <- rbind(data_1,data_2)
  minmax_data <- cbind(minmax_data,input_factor_data)
  
}
######################################################

#############Z-score dataset 생성#####################
{
  norm_data <- vector()
  for(i in 1:length(colnames(input_data))){
    basket <- vector()
    basket <- input_data[[i]]
    basket <- scale(basket)
    norm_data <- cbind(norm_data,basket)
  }
  norm_colname <- paste(colnames(input_data),'_norm',sep="")
  colnames(norm_data) <- norm_colname
  norm_data <- as.data.frame(norm_data)
  data_1 <- data.frame()
  data_2 <- data.frame()
  data_1 <- norm_data[target_address,]
  data_2 <- norm_data[nontarget_address,]
  norm_data <- rbind(data_1,data_2)
  norm_data <- cbind(norm_data,input_factor_data)
}
######################################################

############Quantile dataset 생성#####################
{
  quan_data <- vector()
  for(i in 1:length(colnames(input_data))){
    basket <- vector()
    basket <- input_data[[i]]
    basket <- rank2group(basket, 4)
    quan_data <- cbind(quan_data,basket)
  }
  quan_colname <- paste(colnames(input_data),'_quan',sep="")
  colnames(quan_data) <- quan_colname
  quan_data <- as.data.frame(quan_data)
  data_1 <- data.frame()
  data_2 <- data.frame()
  data_1 <- quan_data[target_address,]
  data_2 <- quan_data[nontarget_address,]
  quan_data <- rbind(data_1,data_2)
  quan_data <- cbind(quan_data,input_factor_data)
}
######################################################

###변환방법별 input variable 1개&target prediction###
{
  NN_norm_acc <- vector()
  NN_quan_acc <- vector()
  NN_minmax_acc <- vector()
  NN_raw_input_acc <- vector()
  
  a <- vector()
  a <- length(colnames(input_data))
  for(i in 1:a){
    norm <- vector()
    quan <- vector()
    minmax <- vector()
    raw_input <- vector()
    
    norm <- norm_data[[i]]
    quan <- quan_data[[i]]
    minmax <- minmax_data[[i]]
    raw_input <- raw_input_data[[i]]
    
    norm_basket <- data.frame()
    quan_basket <- data.frame()
    minmax_basket <- data.frame()
    raw_input_basket <- data.frame()
    
    raw_input_basket <- cbind(raw_input,target_data)
    raw_input_basket <- as.data.frame(raw_input_basket)
    col_name <- vector()
    col_name <- c(colnames(raw_input_data[i]),'target')
    colnames(raw_input_basket) <- col_name
    
    norm_basket <- cbind(norm,target_data)
    norm_basket <- as.data.frame(norm_basket)
    col_name <- vector()
    col_name <- c(colnames(norm_data[i]),'target')
    colnames(norm_basket) <- col_name
    
    quan_basket <- cbind(quan,target_data)
    quan_basket <- as.data.frame(quan_basket)
    colnames(quan_basket[1]) <- colnames(quan_data[i])
    col_name <- vector()
    col_name <- c(colnames(quan_data[i]),'target')
    colnames(quan_basket) <- col_name
    
    
    minmax_basket <- cbind(minmax,target_data)
    minmax_basket <- as.data.frame(minmax_basket)
    colnames(minmax_basket[1]) <- colnames(minmax_data[i])
    col_name <- vector()
    col_name <- c(colnames(minmax_data[i]),'target')
    colnames(minmax_basket) <- col_name
    
    ##raw ANN
    set.seed(7777)
    intrain <- vector()
    intrain <- createDataPartition(y= as.factor(raw_input_basket$target),p=0.7,list = F)
    train <- data.frame()
    test <- data.frame()
    train <- raw_input_basket[intrain, ]
    test <- raw_input_basket[-intrain, ]
    model.nnet <- nnet(as.factor(target)~. ,data=train, size=10, softmax = FALSE, maxit = 2000,  decay= 5e-04)
    predicted <- vector()
    actual <- vector()
    predicted <- predict(model.nnet, test, type = 'class')
    actual <- test$target
    accuracy <- vector()
    accuracy <- mean(actual == predicted)
    NN_raw_input_acc <- c(NN_raw_input_acc,accuracy)
    
    ##norm ANN
    set.seed(7777)
    intrain <- vector()
    intrain <- createDataPartition(y= as.factor(norm_basket$target),p=0.7,list = F)
    train <- data.frame()
    test <- data.frame()
    train <- norm_basket[intrain, ]
    test <- norm_basket[-intrain, ]
    model.nnet <- nnet(as.factor(target)~. ,data=train, size=10, softmax = FALSE, maxit = 2000,  decay= 5e-04)
    predicted <- vector()
    actual <- vector()
    predicted <- predict(model.nnet, test, type = 'class')
    actual <- test$target
    accuracy <- vector()
    accuracy <- mean(actual == predicted)
    NN_norm_acc <- c(NN_norm_acc,accuracy)
    
    ##quan ANN
    set.seed(7777)
    intrain <- vector()
    intrain <- createDataPartition(y= as.factor(quan_basket$target),p=0.7,list = F)
    train <- data.frame()
    test <- data.frame()
    train <- quan_basket[intrain, ]
    test <- quan_basket[-intrain, ]
    model.nnet <- nnet(as.factor(target)~. ,data=train, size=20, softmax = FALSE, maxit = 2000,  decay= 5e-04)
    predicted <- vector()
    actual <- vector()
    predicted <- predict(model.nnet, test, type = 'class')
    actual <- test$target
    accuracy <- vector()
    accuracy <- mean(actual == predicted)
    NN_quan_acc <- c(NN_quan_acc,accuracy)
    
    ##minmax ANN
    set.seed(7777)
    intrain <- vector()
    intrain <- createDataPartition(y= as.factor(minmax_basket$target),p=0.7,list = F)
    train <- data.frame()
    test <- data.frame()
    train <- minmax_basket[intrain, ]
    test <- minmax_basket[-intrain, ]
    model.nnet <- nnet(as.factor(target)~. ,data=train, size=20, softmax = FALSE, maxit = 2000,  decay= 5e-04)
    predicted <- vector()
    actual <- vector()
    predicted <- predict(model.nnet, test, type = 'class')
    actual <- test$target
    accuracy <- vector()
    accuracy <- mean(actual == predicted)
    NN_minmax_acc <- c(NN_minmax_acc,accuracy)
    
    # 
    print(i)
  }
}
######################################################

##################REPORT 생성#########################
{
  b <- colnames(input_data)
  
  report <- data.frame()
  report <- cbind(NN_norm_acc,NN_quan_acc,NN_minmax_acc,NN_raw_input_acc)
  row.names(report) <- b
  report <- as.data.frame(report)
  
  type_list <- c('NN_norm','NN_quan','NN_minmax','NN_raw_input')
  result <- vector()
  
  for(i in 1:length(report$NN_norm_acc)){
    ranking <- vector()
    ranking <- rank(-report[i,])
    ranking <- as.integer(ranking)
    index <- vector()
    index <- grep(1, ranking, fixed = T)
    
    model_basket <- vector()
    model_basket <- type_list[index]
    model_basket <- paste(model_basket,collapse = ', ')
    
    result <- c(result,model_basket)
  }
  
  report <- cbind(report,result)
}
write.csv(report, paste('featureselection_report_',Sys.Date(),'.csv',sep = ""))
#############final data set 생성########################
{
  mergebasket <- vector()
  mergebasket[1:length(input_factor_data)] <- 0
  
  for(j in 1:length(result)){
    if(result[j] == "NN_norm"){
      mergebasket <- data.frame(mergebasket, norm_data[j])
    }else if(result[j] == "NN_quan"){
      mergebasket <- data.frame(mergebasket, quan_data[j])
    }else if(result[j] == "NN_minmax"){
      mergebasket <- data.frame(mergebasket, minmax_data[j])
    }else{
      mergebasket <- data.frame(mergebasket, raw_input_data[j])
    }
  }
  mergebasket[[1]] <- NULL
  mergebasket <- cbind(mergebasket, input_factor_data, target_data)
}
######################################################

# ##############factor형 변수들 dummy화#################
# 
# dummybasket <- vector()
# dummybasket[1:length(input_factor_data)] <- 0
# for(i in 1:length(factor_address)){
#   basket <- vector()
#   basket <- input_factor_data[[i]]
#   test <- model.matrix( ~ input_factor_data[[i]] -1)
#   test_frame <- as.data.frame(test)
#   dummybasket <- cbind(dummybasket,test_frame)
# }
# dummybasket[[1]] <- NULL
# head(dummybasket)
# colnames(dummybasket)<-(1:length(dummybasket[1,]))
# ttest1 <- cbind(mergebasket,input_factor_data,target_data)
# ttest2 <- cbind(mergebasket, dummybasket, target_data)
# mergebasket <- ttest2
# ######################################################

##############Final dataset 생성&초기화###############
case_merge <- data.frame()
case_merge <- cbind(CASE_ID,mergebasket)
write.csv(case_merge, paste('finaldata_',Sys.Date(),'.csv',sep = ""))
######################################################
{
  rm(list = ls())
  mergebasket <- read.csv("ttest0405_2.csv", stringsAsFactors = T) #395에서 설정한 파일명과 동일하게 변경
  mergebasket <-na.omit(mergebasket)
  mergebasket[[1]] <- NULL
}
######################################################

#######################train/test set 구분##############################
set.seed(7777)
intrain <- vector()
intrain <- createDataPartition(y= as.factor(mergebasket$target_data),p=0.7,list = F)
train <- data.frame()
test <- data.frame()
case_basket <- vector()
case_basket <- mergebasket$CASE_ID
mergebasket$CASE_ID <- NULL
train <- mergebasket[intrain, ]
test <- mergebasket[-intrain, ]
test_case <- case_basket[-intrain]
########################################################################

######################Neural network####################################
{
  model.nnet <- nnet(as.factor(target_data)~.,data=train, size=20, softmax = FALSE, maxit = 10000, rang=0.1,  decay= 5e-04, MaxNWts = 2000)
  NNprediction <- vector()
  actual <- vector()
  NNprediction <- predict(model.nnet, test, type = 'class')
  actual <- test$target_data
  accuracy <- vector()
  NN_accuracy <- mean(actual == NNprediction)
}
########################################################################

#######################Decision Tree_deviance###########################
{
  treemod1<-tree(as.factor(target_data)~.,data=train,split = "deviance")
  treeprediction1 <- predict(treemod1, test, type = "class")
  actual <- vector()
  actual <- test$target_data
  DT_deviance_accuracy <- mean(treeprediction1 == actual)
}
########################################################################

#######################Decision Tree_gini##############################
{
  treemod2<-tree(as.factor(target_data)~.,data=train,split = "gini")
  treeprediction2 <- predict(treemod2, test, type = "class")
  actual <- vector()
  actual <- test$target_data
  DT_gini_accuracy <- mean(treeprediction2 == actual)
}
########################################################################

########################Gradient boosting###############################
{
  GBmodel <- gbm(target_data~.,distribution = "bernoulli", data = train, n.trees = 10000, interaction.depth=4, shrinkage = 0.01)
  GBprediction <- predict(GBmodel, newdata = test, n.trees=500, type = "response")
  GBprediction <- round(GBprediction)
  actual <- vector()
  actual <- test$target_data
  GB_accuracy <- mean(GBprediction == actual)
}
########################################################################

######################Support vector machine############################
{
  svm_model <- svm(as.factor(target_data) ~., data=train)
  svmprediction <- predict(svm_model, newdata = test)
  actual <- vector()
  actual <- test$target_data
  svm_accuracy <- mean(svmprediction == actual)
}
########################################################################

####################Logistic regression################################
{
  LRmodel <- glm(target_data ~., data=train, family=binomial)
  LRprediction <- predict(LRmodel, newdata=test, type="response")
  LRprediction <- round(LRprediction)
  LR_accuracy <- mean(LRprediction == test$target_data)
}
########################################################################

# ####################Logistic regression_dummy################################
# {
#   mergebasket3 <- vector()
#   mergebasket3 <- mergebasket
#   mergebasket3$sig_cd <- as.numeric(factor(mergebasket3$sig_cd, level=c("강남구","강서구","강동구","강북구","관악구",
#                                                                         "광진구","구로구","금천구","노원구","도봉구",
#                                                                         "동대문구","동작구","마포구","서대문구","서초구",
#                                                                         "성동구","성북구","송파구","양천구","영등포구","용산구",
#                                                                         "은평구","종로구","중구","중랑구")))
#   
#   GY_basket <- factor(mergebasket3$GY_gubun,levels=0:4)
#   GY_dummy <- model.matrix( ~ GY_basket - 1)
#   sig_basket <- factor(mergebasket3$sig_cd,levels=1:25)
#   sig_dummy <- model.matrix( ~ sig_basket - 1)
#   geo_basket <- factor(mergebasket3$geo_form,levels=0:9)
#   geo_dummy <- model.matrix( ~ geo_basket - 1)
#   mergebasket3 <- cbind(mergebasket3,GY_dummy,sig_dummy,geo_dummy)
#   intrain <- vector()
#   intrain <- createDataPartition(y= as.factor(mergebasket3$target_data),p=0.7,list = F)
#   train <- data.frame()
#   test <- data.frame()
#   train <- mergebasket3[intrain, ]
#   test <- mergebasket3[-intrain, ]
#   
#   LRmodel <- glm(target_data ~., data=train, family=binomial)
#   LRprediction <- predict(LRmodel, newdata=test, type="response")
#   LRprediction <- round(LRprediction)
#   LR_accuracy2 <- mean(LRprediction == test$target_data)
# }
# ########################################################################
# 
######################사례기반추론(Case-based reasoning)##################
{
mergebasket2 <- mergebasket
train <- mergebasket2[intrain, ]
test <- mergebasket2[-intrain, ]
knn_prediction <- vector()
cl = train[,23]
knn_prediction = knn(train = train[,1:22],test = test[,1:22], cl,k=round(sqrt(length(mergebasket[[1]]))))
knn_accuracy <- mean(knn_prediction == test$target_data)
}
#########################################################################

#######################finaltable생성####################################
{
finaltable <- data.frame()
finaltable <- test
finaltable <- cbind(test_case,finaltable,NNprediction,treeprediction1,treeprediction2,GBprediction,svmprediction,LRprediction,knn_prediction)
write.csv(finaltable, paste('finaltable_',Sys.Date(),'.csv',sep = ""))
}
#########################################################################

######################final_accuracy&model save###############################
{
final_accuracy_table <- vector()
final_accuracy_table <- cbind(NN_accuracy,DT_deviance_accuracy,DT_gini_accuracy,GB_accuracy,svm_accuracy,LR_accuracy,knn_accuracy)
write.csv(final_accuracy_table, paste('final_accuracy_table_',Sys.Date(),'.csv',sep = ""))
save(model.nnet, file=paste(getwd(),'/NNmodel.RData',sep=""))
save(treemod1, file=paste(getwd(),'/DT_deviance.RData',sep=""))
save(treemod2, file=paste(getwd(),'/DT_gini.RData',sep=""))
save(GBmodel, file=paste(getwd(),'/GBmodel.RData',sep=""))
save(svm_model, file=paste(getwd(),'/SVMmodel.RData',sep=""))
save(LRmodel, file=paste(getwd(),'/LRmodel.RData',sep=""))
}


