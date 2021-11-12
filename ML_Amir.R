library(mice)

vars_first_timepoint_new<-vars_first_timepoint

initialize_data <- mice(vars_first_timepoint, maxit = 0)
outlist4 <- as.character(initialize_data$loggedEvents[, "out"])
vars_first_timepoint_new <-vars_first_timepoint_new[, !names(vars_first_timepoint_new) %in% outlist4]

vars_first_timepoint_new_full<- mice(vars_first_timepoint_new, m=1, maxit = 20, method = 'cart')
vars_first_timepoint_new_full_imputed<-complete(vars_first_timepoint_new_full,1)


set.seed(123)

smp_size <- floor(0.7 * nrow(vars_first_timepoint_new_full_imputed))
train_ind <- sample(seq_len(nrow(vars_first_timepoint_new_full_imputed)), size = smp_size)


train <- vars_first_timepoint_new_full_imputed[train_ind, ]
test <- vars_first_timepoint_new_full_imputed[-train_ind, ]

##RF
install.packages("randomForest")
library(randomForest)
library(pROC)

train_up= subset(train, select = -c(Subject.ID)) 
train_up$Binary.Outcome<-factor(train_up$Binary.Outcome)
rf = randomForest(Binary.Outcome ~ ., data=train_up, ntree=500, mtry=3, importance=TRUE)

pred_clinical_train<-predict(rf,test,type="prob")
pred_clinical_test<-predict(rf_classifier_clinical,test[,1:38],type="prob")

g1 <- roc(test$Binary.Outcome,pred_clinical_train[,1])
auc(g1)
ci.auc(g1)

importance(rf)
varImp(rf,type=2)

varImpPlot(rf,type=2)

##SVM



library(e1071)
svmfit = svm(Binary.Outcome~.,data=train,probability = TRUE)
pred<-predict(svmfit, test, probability=TRUE)
pred_prob<-(attr(pred, "probabilities"))

g4 <- roc(test$Binary.Outcome,as.numeric(pred_prob[,2]))
auc(g4)
ci.auc(g4)


## LR
train$Binary.Outcome<-factor(train$Binary.Outcome)
regressor = glm(formula = Binary.Outcome ~ ., data=train,family=binomial)
pred<-predict(regressor, test)

g6 <- roc(test$Binary.Outcome,as.numeric(pred))
auc(g6)
ci.auc(g6)

#CNN
library(tensorflow)
library(keras)


train_mat_new<-train_mat[,-5]
train_mat_new<-cbind(train_mat_new,train_mat[,5])
test_mat_new<-test_mat[,-5]
test_mat_new<-cbind(test_mat_new,test_mat[,5])


x_train<-array_reshape(train_mat_new[,1:30], c(dim(train_mat_new[,1:30]), 1))
x_test = array_reshape(test_mat_new[,1:30], c(dim(test_mat_new[,1:30]), 1))
y_train = to_categorical(train_mat_new[,31], 3)
y_test = to_categorical(test_mat_new[,31], 3)

model <- keras_model_sequential() 
model %>% 
  layer_conv_1d(filters=5, kernel_size=10,  activation = "relu",  input_shape=c(30, 1)) %>%
  #layer_global_max_pooling_1d() %>%
  layer_max_pooling_1d(pool_size = 4) %>%
  layer_flatten() %>% 
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 50, batch_size = 30, 
  
)
eval_model<-model %>% evaluate(x_test, y_test)

yhat_keras_class_vec <- predict_classes(object = model, x = (x_test)) %>%
  as.vector()

yhat_keras_class_vec <- predict_proba(object = model, x = (x_test)) %>%
  as.vector()


cm<-table(vars_ukbiobank_test_mat[,39],yhat_keras_class_vec)
print(sum(diag(cm))/sum(cm))

gCNN <- roc(test$Binary.Outcome,yhat_keras_class_vec[1:603])
auc(gCNN)
ci.auc(gCNN)

#NN
library(deepnet)

nn <- nn.train(train_mat_new[,1:30], as.matrix(train_mat_new[,31]), hidden = c(3),activationfun = "sigm", learningrate = 0.006,momentum = 0.0002, 
               learningrate_scale = 0.00001, output = "sigm", numepochs = 20)



yy = nn.predict(nn,as.matrix(test_mat_new[,1:30]))

g7 <- roc(test_mat_new[,31],as.numeric(yy))
auc(g7)
ci.auc(g7)

###Lasso

library(caret)


cv_output <- cv.glmnet(train_mat_new[,1:30], train_mat_new[,31],
                       alpha =1, lambda = lambda_seq, type.measure = "auc", thresh = 1e-3, maxit = 1e3,
                       nfolds = 5, family="binomial")



# identifying best lambda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(train_mat_new[,1:30], train_mat_new[,31], alpha = 1, lambda = best_lam, type.measure = "auc", thresh = 1e-3, maxit = 1e3,
                     nfolds = 5, family="binomial")
pred <- predict(lasso_best, s = best_lam, newx = test_mat_new[,1:30],type = 'response')

gRidge <- roc(test_mat_new[,31],as.numeric(pred))


plot(g1, col="blue",asp=F,legacy.axes = TRUE) # plot ROC curve
lines(g2,col="coral")
lines(gRidge,col="yellow")
lines(g4,col="green")
lines(g6,col="brown")

legend(0.70, 0.31, legend=c("Random Forest (AUC=0.702, 95% C.I.: [0.655-0.749])", "SVM (AUC=0.639, 95% C.I.: [0.591-0.686])","Logistic Regression (AUC=0.626, 95% C.I.: [0.577-0.673])", "Lasso (AUC=0.623, 95% C.I.: [0.575-0.671])", "Ridge (AUC=0.621, 95% C.I.: [0.573-0.669])"),
       col=c("blue", "green", "brown", "coral","yellow"), lty=1, cex=0.8
       , text.font=4)

##NB
library(caret)
train_up= subset(train, select = -c(Subject.ID)) 

nbfit<-caret::train(train_up[,1:30],train_up[,4],'nb',trControl=trainControl(method='cv',number=5))


X <- varImp(nbfit)
plot(X)

for (i in 1:30)
{
print(class(train_up[,i]))
}

###validation plot



a <- data.frame(Method=c('RF', 'SVM', 'Logistic Regression', 'Lasso Regression', 'Ridge Regression','Temporal Convolutional Network','RNN','LSTM','Weighted LSTM'),
                Mean = c(0.683,0.649,0.636,0.633,0.631,0.715,0.745,0.769,0.805),
                Lower_lim= c(0.635,0.601,0.587,0.585,0.583,0.652,0.722,0.738,0.785),
                Upper_lim=c(0.729,0.696,0.683,0.681,0.679,0.750,0.759,0.785,0.822),
                Methods=c('RF', 'SVM', 'Logistic Regression', 'Lasso Regression', 'Ridge Regression','Temporal Convolutional Network','RNN','LSTM','Weighted LSTM'))

#('taxoNN_corr', 'CNN_basic', 'CNN_shuffle', 'RF','Lasso Regression', 'Ridge Regression','NB','GBC','SVM'))

level_order <- c('RF', 'SVM', 'Logistic Regression', 'Lasso Regression', 'Ridge Regression','Temporal Convolutional Network','RNN','LSTM','Weighted LSTM')             
library(ggplot2)

p<-ggplot(a,                ### The data frame to use.
          aes(x = factor(Method, levels=level_order),
              y = Mean,
              color = level_order)) +
  geom_errorbar(aes(ymin = Lower_lim,
                    ymax = Upper_lim),
                width = 0.2, 
                size  = 0.9) +
  geom_point(shape = 5, 
             size  = 1.5) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  
  theme(aspect.ratio = 1)+
  ylim(0.5,0.9) +
  labs(y="AUC", x = "")
#p + theme(legend.position = "none")
z<-p+coord_flip()
z + theme(legend.position = "none")



#### HC and non HC patients
vars_HC<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/HC.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")
vars_NHC<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/NHC.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

vars_HC<-vars_NHC
vars_HC[ vars_HC == "#N/A" ] <- NA

vars_HC <- as.data.frame(sapply((vars_HC), as.numeric))

library(tidyr)
library(dplyr)

all_na <- function(x) any(!is.na(x))
vars_HC_new<-vars_HC %>% select_if(all_na)

library(mice)
vars_first_timepoint_new<-vars_HC_new
initialize_data <- mice(vars_first_timepoint_new, maxit = 0)
outlist4 <- as.character(initialize_data$loggedEvents[, "out"])
vars_first_timepoint_new <-vars_first_timepoint_new[, !names(vars_first_timepoint_new) %in% outlist4]

vars_first_timepoint_new_full<- mice(vars_first_timepoint_new, m=1, maxit = 20, method = 'cart')
vars_first_timepoint_new_full_imputed<-complete(vars_first_timepoint_new_full,1)


set.seed(123)



smp_size <- floor(0.7 * nrow(vars_first_timepoint_new_full_imputed))
train_ind <- sample(seq_len(nrow(vars_first_timepoint_new_full_imputed)), size = smp_size)


train <- vars_first_timepoint_new_full_imputed[train_ind, ]
test <- vars_first_timepoint_new_full_imputed[-train_ind, ]



##RF
install.packages("randomForest")
library(randomForest)
library(pROC)
library(caret)


train$Binary.Outcome<-factor(train$Binary.Outcome)
rf = randomForest(Binary.Outcome ~ ., data=train, ntree=500, mtry=5, importance=TRUE)

pred_clinical_train<-predict(rf,test,type="prob")
pred_clinical_test<-predict(rf_classifier_clinical,vars_ukbiobank_test[,1:38],type="prob")

g1 <- roc(test$Binary.Outcome,pred_clinical_train[,2])
auc(g1)
ci.auc(g1)

importance(rf)
varImp(rf,type=2)

##error bar


errorbar<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/errorbar.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

errorbar$Method <- factor(errorbar$Method)

library(ggplot2)
# Default bar plot
p<- ggplot(errorbar, aes(x=dose, y=Performance, fill=forcats::fct_rev(Method))) + 
  scale_fill_manual(values=colorsAmir)+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) 

colorsAmir<-c("#e34a33",
  "#fee8c8",
  "#fdbb84"
  
  )


print(p)
# Finished bar plot
p+labs(title="Tooth length per dose", x="Dose (mg)", y = "Length")+
  theme_classic() +
  scale_fill_manual(values=c('#999999','#E69F00'))


####

f4<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/f2.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

f4$ID<-seq(1, 541)



library(ggplot2)
sp<-ggplot(f4, aes(x=ID, y=prob, group = predicted,col=factor(predicted)))  +       # Thicker line
  geom_point(aes(shape=factor(predicted)),   # Shape depends on cond
             size = 2) 

sp + theme_bw()+scale_x_continuous(breaks=seq(0, 600, by = 50),limits = c(0, 550)) + scale_colour_manual(values=cbp1)



cbp1 <- c("#7AD7F0", "#FF0000",  "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sp + 

##
  ##Pie charts
  
  df <- data.frame(
    group = c("Male", "Female", "Child"),
    value = c(25, 25, 50)
  )
head(df)
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)
pie