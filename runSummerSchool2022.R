library(data.table)
library(h2o)
library(dplyr)

## read points from Vaihingen benchmark 400K points lidar 
## (http://www2.isprs.org/commissions/comm3/wg4/tests.html)
## 
## 
#path2script <- rstudioapi::getSourceEditorContext()$path
#setwd(dirname(path2script))
ref<-fread("Vaihingen3D_EVAL_WITH_REF.pts")
## no column names!
names(ref)
## re-assign column names 
names(ref)<-c("x","y","z", "int","return","returns", "class")
## show classes and frequencies
table(ref$class)
## class names
cl<-c('Powerline','Low vegetation','Impervious surfaces','Car','Fence/Hedge','Roof','Facade','Shrub','Tree')
print(cl)
##  RDS file with 23 features, e.g. 23 columns with "geometric features" 
##  extracted from cloud compare with 1 m neighbour distance
features<-readRDS("features.rds")
## add my features (they are in the same order so we only need to bind horizontally)
all.data<-cbind(ref, features)
## faccio diventare colonna "classi" da tipo "intero" ad una colonna di tipo  "fattoriale"
## 
all.data$class<-as.factor(all.data$class)
 
### new with H2o --------

library(h2o)
h2o.init()
df <- h2o::as.h2o(all.data)
splits <- h2o.splitFrame(data = df,
                         ratios = c(0.5)  # 50/50 training/testing
                          )
train <- splits[[1]]
test <- splits[[2]]


rf <- h2o.randomForest(y = "class",
                       training_frame = train,
                       model_id = "our.rf",
                       seed = 1234)

rf_perf1 <- h2o.performance(model = rf, newdata = test)
print(rf_perf1)

h2o.varimp_plot(rf)

df.pred <- h2o.predict(rf, newdata = df)
df.pred.df<-as.data.frame(df.pred)
head(df.pred.df)
df.pred.df.prob <- apply(df.pred.df[,-1], 1, function(x){round(max(x),3) })

final.df <- data.frame(ref[,c("x", "y", "z" ,"class")], class.pred=as.integer(df.pred.df$predict), 
                       prob=df.pred.df.prob)
write.csv(final.df,"classified.txt", row.names = F)
zip("classified.zip", "classified.txt")
