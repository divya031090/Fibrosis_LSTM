vars_first_timepoint<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/data_one.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

vars_first_timepoint<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/newdataoneset.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

vars_first_timepoint<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/data_lastupdated.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

vars_first_timepoint_up<-vars_first_timepoint[1:1604,]

vars_first_timepoint<-vars_first_timepoint_up
  
  
vars_first_timepoint$recepient.gender<-factor(vars_first_timepoint$recepient.gender)
vars_first_timepoint$transplant.primary.indication<-factor(vars_first_timepoint$transplant.primary.indication)
vars_first_timepoint$BMI.30.Y.N.<-factor(vars_first_timepoint$BMI.30.Y.N.)
vars_first_timepoint$diabetes.mellitus..Y.N.<-factor(vars_first_timepoint$diabetes.mellitus..Y.N.)
vars_first_timepoint$hypertension..Y.N.<-factor(vars_first_timepoint$hypertension..Y.N.)
vars_first_timepoint$dyslipidemia..Y.N.<-factor(vars_first_timepoint$dyslipidemia..Y.N.)
vars_first_timepoint$donor.type<-factor(vars_first_timepoint$donor.type)

vars_first_timepoint$HBV.HCV<-factor(vars_first_timepoint$HBV.HCV)

vars_first_timepoint[ vars_first_timepoint == "#N/A" ] <- NA

vars_first_timepoint <- as.data.frame(sapply((vars_first_timepoint), as.numeric))

library(tidyr)
library(dplyr)

all_na <- function(x) any(!is.na(x))
vars_first_timepoint_new<-vars_first_timepoint %>% select_if(all_na)

vars_first_timepoint_new = subset(vars_first_timepoint_new, select = -c(Fibrosis,Fibrosis.Outcome) )

vars_first_timepoint_cases<-(vars_first_timepoint[which(vars_first_timepoint$Binary.Outcome==1),])
vars_first_timepoint_controls<-vars_first_timepoint[which(vars_first_timepoint$Binary.Outcome==0),]



missing_terms<-as.data.frame(sapply(vars_first_timepoint, function(x) sum(is.na(x))))
missing_terms_perc<-as.data.frame(sapply(vars_first_timepoint, function(x) (sum(is.na(x)))*100)/2019)

##last timepoint
vars_last_timepoint<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/data_last.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

vars_last_timepoint[ vars_last_timepoint == "#N/A" ] <- NA

vars_last_timepoint <- as.data.frame(sapply((vars_last_timepoint), as.numeric))

vars_first_timepoint<-vars_last_timepoint


#vars_first_timepoint_cases<-vars_ukbiobank[which(vars_ukbiobank$Cardiovascular.Issues..Outcome.==1),]
#vars_ukbiobank_controls<-vars_ukbiobank[which(vars_ukbiobank$Cardiovascular.Issues..Outcome.==0),]


missing_terms<-as.data.frame(sapply(vars_first_timepoint, function(x) sum(is.na(x))))
missing_terms_perc<-as.data.frame(sapply(vars_first_timepoint, function(x) (sum(is.na(x)))*100)/2163)


#summarise
IQR<-cbind(colnames(vars_first_timepoint),summary(vars_first_timepoint)[3,],summary(vars_first_timepoint)[2,],summary(vars_first_timepoint)[5,])



IQR<-cbind(colnames(vars_first_timepoint_cases),summary(vars_first_timepoint_cases)[3,],summary(vars_first_timepoint_cases)[2,],summary(vars_first_timepoint_cases)[5,])

IQR<-cbind(colnames(vars_first_timepoint_controls),summary(vars_first_timepoint_controls)[3,],summary(vars_first_timepoint_controls)[2,],summary(vars_first_timepoint_controls)[5,])

write.table(IQR, file = "/Users/divvi/Documents/Dr. Bhat/IQR_controls.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = FALSE, row.names = FALSE)

#p value
iter<-c(7,
        9,
        13,
        15,
        17,
        19,
        21,
        22,
        24,
        26,
        28,
        30,
        31,
        
        34,
        36,
        37,
        39,
        41,
        43,
        45,
        46,
        47,
       
        51,
        52)
for (i in iter)
{
print(wilcox.test(vars_first_timepoint[,i]~vars_first_timepoint$Binary.Outcome,data=vars_first_timepoint)$p.value)
}

#chisq

vars_first_timepoint$recepient.gender<-factor(vars_first_timepoint$recepient.gender)
table(vars_first_timepoint$recepient.gender)

vars_first_timepoint$HBV.HCV <-factor(vars_first_timepoint$HBV.HCV)
table(vars_first_timepoint$HBV.HCV)

vars_first_timepoint$diabetes.mellitus..Y.N.<-factor(vars_first_timepoint$diabetes.mellitus..Y.N.)
table(vars_first_timepoint$diabetes.mellitus..Y.N.)

vars_first_timepoint$hypertension..Y.N.<-factor(vars_first_timepoint$hypertension..Y.N.)
table(vars_first_timepoint$hypertension..Y.N.)

vars_first_timepoint$dyslipidemia..Y.N.<-factor(vars_first_timepoint$dyslipidemia..Y.N.)
table(vars_first_timepoint$dyslipidemia..Y.N.)

vars_first_timepoint$Binary.Fibrosis.Outcome<-factor(vars_first_timepoint$Binary.Fibrosis.Outcome)

#cases

vars_first_timepoint_cases$recepient.gender<-factor(vars_first_timepoint_cases$recepient.gender)
table(vars_first_timepoint_cases$recepient.gender)

vars_first_timepoint_cases$HBV.HCV <-factor(vars_first_timepoint_cases$HBV.HCV)
table(vars_first_timepoint_cases$HBV.HCV)

vars_first_timepoint_cases$diabetes.mellitus..Y.N.<-factor(vars_first_timepoint_cases$diabetes.mellitus..Y.N.)
table(vars_first_timepoint_cases$diabetes.mellitus..Y.N.)

vars_first_timepoint_cases$hypertension..Y.N.<-factor(vars_first_timepoint_cases$hypertension..Y.N.)
table(vars_first_timepoint_cases$hypertension..Y.N.)

vars_first_timepoint_cases$dyslipidemia..Y.N.<-factor(vars_first_timepoint_cases$dyslipidemia..Y.N.)
table(vars_first_timepoint_cases$dyslipidemia..Y.N.)

#controls

vars_first_timepoint_controls$recepient.gender<-factor(vars_first_timepoint_controls$recepient.gender)
table(vars_first_timepoint_controls$recepient.gender)

vars_first_timepoint_controls$HBV.HCV <-factor(vars_first_timepoint_controls$HBV.HCV)
table(vars_first_timepoint_controls$HBV.HCV)

vars_first_timepoint_controls$diabetes.mellitus..Y.N.<-factor(vars_first_timepoint_controls$diabetes.mellitus..Y.N.)
table(vars_first_timepoint_controls$diabetes.mellitus..Y.N.)

vars_first_timepoint_controls$hypertension..Y.N.<-factor(vars_first_timepoint_controls$hypertension..Y.N.)
table(vars_first_timepoint_controls$hypertension..Y.N.)

vars_first_timepoint_controls$dyslipidemia..Y.N.<-factor(vars_first_timepoint_controls$dyslipidemia..Y.N.)
table(vars_first_timepoint_controls$dyslipidemia..Y.N.)

chisq.test(vars_first_timepoint$recepient.gender,vars_first_timepoint$Binary.Outcome)
chisq.test(vars_first_timepoint$diabetes.mellitus..Y.N.,vars_first_timepoint$Binary.Outcome)
chisq.test(vars_first_timepoint$HBV.HCV,vars_first_timepoint$Binary.Outcome)
chisq.test(vars_first_timepoint$hypertension..Y.N.,vars_first_timepoint$Binary.Outcome)
chisq.test(vars_first_timepoint$dyslipidemia..Y.N.,vars_first_timepoint$Binary.Outcome)


#logistic regression
vars_ukbiobank$Sex<-factor(vars_ukbiobank$Sex)
vars_ukbiobank$Diabetes<-factor(vars_ukbiobank$Diabetes)
pvalues_logit<-NULL
logit_value<-NULL
for (i in 2:64)
{
  fit<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank[,i],data=vars_ukbiobank,family=binomial(logit))
  p_val<-coef(summary(fit))[,4]
  pvalues_logit<-rbind(pvalues_logit,p_val)
  logit_value_col<-as.data.frame(exp(cbind(coef(fit), confint(fit)))  )
  logit_value<-rbind(logit_value,logit_value_col)
  
}
pvalues_logit<-cbind(pvalues_logit,colnames(vars_ukbiobank)[2:64])
logit_value<-cbind(logit_value,colnames(vars_ukbiobank)[2:64])


###MVA
vars_ukbiobank$BMI_categorical<-factor(vars_ukbiobank$BMI_categorical)
fit2<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank$Age+vars_ukbiobank$Systolic_Blood_Pressure+vars_ukbiobank$LDL+vars_ukbiobank$Waist.circumference+vars_ukbiobank$BMI_categorical,data=vars_ukbiobank,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit2), confint(fit2)))  )

fit3<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank$Age+vars_ukbiobank$Systolic_Blood_Pressure+vars_ukbiobank$LDL+vars_ukbiobank$Waist.circumference+vars_ukbiobank$BMI+vars_ukbiobank$Glycated.haemoglobin..HbA1c.,data=vars_ukbiobank,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit3), confint(fit3)))  )

fit4<-glm(vars_ukbiobank$Cardiovascular.Issues..Outcome.~vars_ukbiobank$Age+vars_ukbiobank$Systolic_Blood_Pressure+vars_ukbiobank$LDL+vars_ukbiobank$Waist.circumference+vars_ukbiobank$BMI+vars_ukbiobank$Glycated.haemoglobin..HbA1c.+vars_ukbiobank$Visceral.adipose.tissue.volume..VAT.+vars_ukbiobank$Cholesterol+vars_ukbiobank$Urate,data=vars_ukbiobank,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit4), confint(fit4)))  )


#sample training and testing data
gendata_ID<-read.csv("/Users/divvi/Documents/Dr. Bhat/overlaped_ID.csv",header =TRUE, stringsAsFactors = FALSE,sep="\t")
smp_size <- floor(0.7 * nrow(vars_first_timepoint))

set.seed(123)
train_ind <- sample(seq_len(nrow(vars_first_timepoint)), size = smp_size)


train <- vars_first_timepoint[train_ind, ]
test <- vars_first_timepoint[-train_ind, ]



## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(gendata_ID)), size = smp_size)


train <- gendata_ID[train_ind, ]
test <- gendata_ID[-train_ind, ]
colnames(train[1])<-"eid"

train<-read.csv("/Users/divvi/Documents/Dr. Bhat/training.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")
test<-read.csv("/Users/divvi/Documents/Dr. Bhat/test_set.csv",header =TRUE, stringsAsFactors = FALSE,sep=",")

vars_ukbiobank_train<-subset(vars_ukbiobank, eid %in% train$eid)
vars_ukbiobank_test<-subset(vars_ukbiobank, eid %in% test$eid)

fit3<-glm(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.~vars_ukbiobank_train$Age+vars_ukbiobank_train$Systolic_Blood_Pressure+vars_ukbiobank_train$LDL+vars_ukbiobank_train$BMI+vars_ukbiobank_train$Glycated.haemoglobin..HbA1c..1,data=vars_ukbiobank_train,family=binomial(logit))
as.data.frame(exp(cbind(coef(fit3), confint(fit3)))  )


fit<-glm(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.~vars_ukbiobank_train$BMI,data=vars_ukbiobank_train,family=binomial(logit))
coef(summary(fit))[,4]

VIF_uk<-as.data.frame(vif(glm(vars_ukbiobank_train$Cardiovascular.Issues..Outcome.~vars_ukbiobank_train$Age+vars_ukbiobank_train$Systolic_Blood_Pressure+vars_ukbiobank_train$LDL+vars_ukbiobank_train$BMI+vars_ukbiobank_train$Glycated.haemoglobin..HbA1c..1,data=vars_ukbiobank_train,family=binomial(logit))
))


###data difference 

vars_diff<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/data_difference.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

vars_diff[ vars_diff == "#N/A" ] <- NA

vars_diff <- as.data.frame(sapply((vars_diff), as.numeric))

IQR<-cbind(colnames(vars_diff),summary(vars_diff)[3,],summary(vars_diff)[2,],summary(vars_diff)[5,])


###LR
smp_size <- floor(0.7 * nrow(vars_first_timepoint))
 set.seed(123)
 train_ind <- sample(seq_len(nrow(vars_first_timepoint)), size = smp_size)
 train <- vars_first_timepoint[train_ind, ]
 test <- vars_first_timepoint[-train_ind, ]
 
 data_train<-subset(vars_first_timepoint, Subject.ID %in% train$Subject.ID)
 data_test<-subset(vars_first_timepoint, Subject.ID %in% test$Subject.ID)
 


regressor = glm(formula = Binary.Outcome ~ ., data=data_train_up)
pred<-predict(regressor, vars_ukbiobank_test[,1:41])


###

#Hepatitis and non hepatitis group

vars_HC<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/HC.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")
vars_NHC<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/NHC.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")



###
scores_up<-read.csv("/Users/divvi/Documents/Dr. Bhat/Amir project/New data files/new_8thMarch/scores.csv",header = TRUE, stringsAsFactors = FALSE,sep=",")

regressor = glm(formula = Outcome ~ as.numeric(FIB4), data=scores_up_up,family=binomial)
pred<-predict(regressor, scores_up_up)

scores_up_up<-as.data.frame(scores_up[1:1500,])
predic<-ifelse(pred > 0.5,1,0)

g6 <- roc(scores_up_up$Outcome,as.numeric(pred))
auc(g6)
ci.auc(g6)



#### Further Logistic regression of the important variables
keep<-c("Recepient.age",
     
     "donor.age",
     "creat",
     "AST" ,
     "ALT" ,
     "bilt" ,
     "platelets"   ,
     "WBC",
     "weight.kg..at.tx",
     "TACROLIMUS",
     "inr",  
     "M_CYCLO",
     "ALP"   ,
     "Meld.at.Tx"    ,
     "Na",
     "Binary.Outcome")

"ALP",
"BMI.at.tx",
"inr",
"HGB",
"ALB",

"Meld.at.Tx",
"Na",

"donor.type",

new_vars<-vars_first_timepoint_new_full_imputed[keep]



new_vars$Binary.Outcome<-factor(new_vars$Binary.Outcome)

new_vars$donor.type<-factor(new_vars$donor.type)

new_vars$transplant.primary.indication<-factor(new_vars$transplant.primary.indication)



library(glmnet)
library(gee)
regressor = glm(formula = Binary.Outcome ~ ., data=new_vars,family = "binomial")

OR_amir<-as.data.frame(exp(cbind(coef(regressor), confint(regressor)))  )

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(regressor,show.values = TRUE, value.offset = .3)


library(broom)
library(ggplot2)
model_output <- tidy(regressor)
out_conf <- tidy(regressor, conf.int = TRUE)

library(forestmangr)
lm_model_out <- round_df(out_conf, digits=2)
lm_model_out <- lm_model_out[-1,] #remove the intercept 

# Now plot them
ggplot(lm_model_out, aes(x=reorder(term, estimate), y=estimate)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                width = 0.2,size  = 1,
                position = "dodge", color="turquoise4") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_point() + coord_flip() 

### RF

library(randomForest)
library(pROC)


newvars_noNA <- new_vars[complete.cases(new_vars),]

rf = randomForest(Binary.Outcome ~ ., data=newvars_noNA, ntree=500, mtry=3, importance=TRUE)

pred_clinical_train<-predict(rf,test,type="prob")
pred_clinical_test<-predict(rf_classifier_clinical,test[,1:38],type="prob")

g1 <- roc(test$Binary.Outcome,pred_clinical_train[,1])
auc(g1)
ci.auc(g1)

importance(rf)
varImp(rf,type=2)

varImpPlot(rf,type=1)

### use gee


data(warpbreaks)
## marginal analysis of random effects model for wool
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="exchangeable"))
## test for serial correlation in blocks
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="AR-M", Mv=1))
if(require(MASS)) {
  data(OME)
  ## not fully appropriate link for these data.
  (fm <- gee(cbind(Correct, Trials-Correct) ~ Loud + Age + OME, id = ID,
             data = OME, family = binomial, corstr = "exchangeable"))
  summary(fm)
}



##OR plot



OR_amir<-OR_amir[2:11,]

OR_amir$OddsRatio<-c(0.9796,
0.9882,
1.0072,
0.9982,
1.0194,
1.0012,
0.9984,
0.9993,
0.9906,
1.005)


OR_amir$ciLow<-c(0.9708,
                 0.9871,
                 1.0014,
                 0.9971,
                 1.0036,
                 1.0004,
                 0.9976,
                 0.9978,
                 0.9653,
                 0.9993)

OR_amir$ciHigh<-c(0.9884,
                  0.9893,
                  1.013,
                  0.9993,
                  1.0354,
                  1.0025,
                  0.9992,
                  1.0007,
                  1.0154,
                  1.0106)

OR_amir$OddsRatio<-OR_amir$V1
OR_amir$ciLow<-OR_amir$`2.5 %`
OR_amir$ciHigh<-OR_amir$`97.5 %`
OR_amir$Covariate<-rownames(OR_amir)

ggplot(OR_amir,aes(y=Covariate, x=OddsRatio, label=Covariate)) +
  geom_point(size=1, shape=19) +
  geom_errorbarh(aes(xmin=ciLow, xmax=ciHigh)) +
 
  geom_vline(xintercept=1, linetype='longdash') 


###gee plot

library(jstable)
library(geepack)
data(dietox)
dietox$Cu <- as.factor(dietox$Cu)
mf2 <- formula(Weight ~ Cu * Time + I(Time^2) + I(Time^3))
gee2 <- ordgee(mf2, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
anova(gee2)
