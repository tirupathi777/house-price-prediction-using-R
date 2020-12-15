setwd("C:/Users/hp/Documents/test")


htr=read.csv("housing_train.csv", stringsAsFactors = F)
hts=read.csv("housing_test.csv",stringsAsFactors = F)


hts$Price=NA

hts$Data="Test"
htr$Data="Train"

hall=rbind(htr,hts)

glimpse(hall)

n=table(hall$Suburb)
dim(n)
n=round(tapply(hall$Price,hall$Suburb,mean,na.rm=T))
sort(n)
n=round(tapply(hall$Price,hall$Postcode,mean,na.rm=T))

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

n=table(hall$SellerG)
dim(n)
n
lapply(hall,function(x) sum(is.na(x)))

for(col in names(hall)){
  if(sum(is.na(hall[,col]))>0 & (col %in% c("Bedroom2","Bathroom",
                                               "Car","Landsize","BuildingArea","YearBuilt"))){
    hall[is.na(hall[,col]),col]=round(mean(hall[,col],na.rm=T))
  }
}

hall=CreateDummies(hall ,"Type",1000)
hall=CreateDummies(hall ,"Method",50)

glimpse(hall)

htr=hall %>% filter(Data=='Train') %>% select(-Data)
hts=hall %>% filter(Data=='Test') %>% select(-Data,-Price)

glimpse(hall)

set.seed(2)
s=sample(1:nrow(htr),.75*nrow(htr))
htr1=htr[s,]
htr2=htr[-s,]

library(gbm)
library(cvTools)
library(dplyr)
library(car)


param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)

myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  
  params=my_params[i,]
  
  k=cvTuning(gbm,Price~.-Suburb-Address-SellerG-CouncilArea,
             data =htr1,
             tuning =params,
             args = list(distribution="gaussian"),
             folds = cvFolds(nrow(htr1), K=10, type = "random"),
             seed =2,
             predictArgs = list(n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    
    myerror=score.this
    print(myerror)
    
    best_params=params
  }
  
  print('DONE')
  
}

Score_rf =212467/myerror
Score_rf

myerror

best_params

best_params=data.frame(interaction.depth=7,
                       n.trees=700,
                       shrinkage=0.1,
                       n.minobsnode=2)

bs.gbm.final=gbm(Price~.-Suburb-Address-SellerG-CouncilArea,data=htr1,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "gaussian")

bs.gbm.final

test.pred=predict(bs.gbm.final,newdata=htr2,
                  n.trees = best_params$n.trees)

plot(test.pred,htr2$Price)
errors=htr2$Price-test.pred

RMSE_gbm=errors**2 %>% mean() %>% sqrt()
RMSE_gbm
Score_gbm =212467/RMSE_gbm
Score_gbm


for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  params=my_params[i,]
  k=cvTuning(gbm,Price~.-Suburb-Address-SellerG-CouncilArea,
             data =htr,
             tuning =params,
             args = list(distribution="gaussian"),
             folds = cvFolds(nrow(htr), K=10, type = "random"),
             seed =2,
             predictArgs = list(n.trees=params$n.trees)
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    myerror=score.this
    print(myerror)
    best_params=params
  }
  
  print('DONE')
}

Score_rf =212467/myerror
Score_rf

myerror
best_params

best_params=data.frame(interaction.depth=7,
                       n.trees=700,
                       shrinkage=0.1,
                       n.minobsnode=2)

bs.gbm.final=gbm(Price~.-Suburb-Address-SellerG-CouncilArea,data=htr,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "gaussian")
bs.gbm.final

test.pred=predict(bs.gbm.final,newdata=hts,
                  n.trees = best_params$n.trees)

write.table(test.pred,file ="tirupathi rao_gbm_P1_part2.csv",
            row.names = F,col.names="Price")
