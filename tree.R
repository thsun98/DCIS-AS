library(data.table)
library(ggplot2)
library(viridis)
library(ranger)
library(partykit)
library(rpart)
library(rpart.plot)
library(treeClust)
library(sperrorest)
#dat2 <- fread("MRIReadData2_Rita.csv")
#dat2 <- fread("Rita64patients_changeQ.csv")
#dat2 <- fread("Rita73patients.csv")
dat2 <- fread("Rita73patients_trial.csv")
#dat2 <- fread("Rita63patientstryit.csv")
#dat2 <- fread("AS-Reads_Rita_N63.csv")
#dat2 <- fread("Heather_FinalReads_63patients_NETfinal.csv")

#dat2 <- fread("Rita63patients_forpaper.csv")
#dat2<- fread("Heather73patients.csv")
missing_num <- unlist(dat2[,lapply(.SD,function(x){
  sum(is.na(x)|x=="")
})])

Cols_2_model <- names(missing_num[missing_num<=30])
train_dat <- dat2[,Cols_2_model, with = FALSE]
train_dat[,`Study ID`:=NULL]
set.seed(42)
#alpha     <- 1# percentage of training set
#inTrain   <- sample(1:nrow(train_dat), floor(alpha * nrow(train_dat)))
train.set <- train_dat[,.SD] #added as.factor
#test.set  <- train_dat[-inTrain,.SD] #added as.factor
#test.X <- test.set
#test.Y <- test.X[,`Progression to Invasive Cancer`]
#test.X[,`Progression to Invasive Cancer`:=NULL]
train.X<- train.set
#train.X[,`Progression to Invasive Cancer`:=NULL]
#train.set[,sapply(.SD,class)]
train.set[,(colnames(train.set)):=lapply(.SD,as.factor)]
train.set[,sapply(.SD,class)]
rpart.tree <- rpart(as.factor(`Progression to Invasive Cancer`) ~ ., data=train.set,
                    method="class",model=TRUE,
                    control=rpart.control(maxdepth=5,
                                          minbucket=4,
                                          cp         = 0.01))
rpart.predict.leaves(rpart.tree,newdata=train.X,type="where")
dat2[,leaf_node:=rpart.tree$where]
fwrite(dat2,"alldat_Ritaleafnodeid.csv")
rparty.tree <- as.party(rpart.tree)
rparty.tree
plot(rparty.tree,box.palette="auto")

print(rparty.tree)
printcp(rpart.tree)
#plotcp(rpart.tree)



#pred.tree <- predict(object=rpart.tree, newdata=test.X,
 #     type = c("prob")
#   )
#pred.tree <- as.data.frame(pred.tree)
#setDT(pred.tree)
#pred.tree[,actual:=test.Y]


