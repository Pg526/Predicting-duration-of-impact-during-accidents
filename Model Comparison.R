#Model Comparison

Model_1 =Model2Q
Model_2 =Model3H
Model_3 =Model4D
Model_4 =Model5F
Model_5 =Model_minBIC_26
Model_6 =Model_minBIC_27
Model_7 =Model_minBIC_28
Model_8 =Model_minCP_49J
Model_9 =Model_min1BIC_32
Model_10 =Model_min1BIC_33
Model_11 =Model_min1BIC_34
Model_12 = Model_minCP1_43A
Model_13 =Model_minCP1_44B

##------------------------------------------
#r -square, adjusted R square, BIC, AIC
#glance

#M1 = as.data.frame(glance(Model_1))
#M2 = as.data.frame(glance(Model_2))
# M3 = as.data.frame(glance(Model_3))
# M4 = as.data.frame(glance(Model_4))
# M5 = as.data.frame(glance(Model_5))
# M6 = as.data.frame(glance(Model_6))
# M7 = as.data.frame(glance(Model_7))
# M8 = as.data.frame(glance(Model_8))
# M9 = as.data.frame(glance(Model_12))
# M10 = as.data.frame(glance(Model_10))
# M11 = as.data.frame(glance(Model_11))
# M12 = as.data.frame(glance(Model_12))
# M13 = as.data.frame(glance(Model_13))

# VIF and cooks distance
# P1 = c(max(vif(Model1)), max(cooks.distance(Model_1)))
# P2 = c(max(vif(Model2)), max(cooks.distance(Model_2)))
# P3 = c(max(vif(Model3)), max(cooks.distance(Model_3)))
# P4 = c(max(vif(Model4)), max(cooks.distance(Model_4)))
# P5 = c(max(vif(Model5)), max(cooks.distance(Model_5)))
# P6 = c(max(vif(Model6)), max(cooks.distance(Model_6)))
# P7 = c(max(vif(Model7)), max(cooks.distance(Model_7)))
# P8 = c(max(vif(Model8)), max(cooks.distance(Model_8)))
# P9 = c(max(vif(Model9)), max(cooks.distance(Model_12)))
# P10 = c(max(vif(Model8)), max(cooks.distance(Model_10)))
# P11 = c(max(vif(Model8)), max(cooks.distance(Model_11)))
# P12 = c(max(vif(Model8)), max(cooks.distance(Model_12)))
# P13 = c(max(vif(Model8)), max(cooks.distance(Model_13)))


results = rbind(M1,M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13)
results
#params = rbind(P1,P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)
#params



##------------------------------------------
#residual plots


#Model_1
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_1$residuals, main = "Model 1")
qqline(Model_1$residuals, col =4)

plot(Model_1$residuals ~ Model_1$fitted.values, main = "Model 1")
abline(h=0, col='blue')
par(plotgrid)

#Model_2
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_2$residuals)
qqline(Model_2$residuals, col=4)

plot(Model_2$residuals ~ Model_2$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_3
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_3$residuals)
qqline(Model_3$residuals, col =4)

plot(Model_3$residuals ~ Model_3$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_4
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_4$residuals)
qqline(Model_4$residuals, col =4)

plot(Model_4$residuals ~ Model_4$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model5
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_5$residuals)
qqline(Model_5$residuals, col =4)

plot(Model_5$residuals ~ Model_5$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_6
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_6$residuals)
qqline(Model_6$residuals, col =4)

plot(Model_6$residuals ~ Model_6$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_7
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_7$residuals)
qqline(Model_7$residuals, col =4)

plot(Model_7$residuals ~ Model_7$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_8
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_8$residuals)
qqline(Model_8$residuals, col =4)

plot(Model_8$residuals ~ Model_8$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_9
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_9$residuals)
qqline(Model_9$residuals, col =4)

plot(Model_9$residuals ~ Model_4$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_10
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_10$residuals)
qqline(Model_10$residuals, col =4)

plot(Model_10$residuals ~ Model_10$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_11
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_11$residuals)
qqline(Model_11$residuals, col =4)

plot(Model_11$residuals ~ Model_11$fitted.values)
abline(h=0, col='blue')
par(plotgrid)


#Model_12
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_12$residuals)
qqline(Model_12$residuals, col =4)

plot(Model_12$residuals ~ Model_12$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

#Model_13
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_13$residuals)
qqline(Model_13$residuals, col =4)

plot(Model_13$residuals ~ Model_13$fitted.values)
abline(h=0, col='blue')
par(plotgrid)

##------------------------------------------
#press staticstics
library(dplyr)
library(leaps)
library(MASS)
library(car)
library(caret)
library(MPV)
library(broom)


press = rbind(
  PRESS(Model_1),
  PRESS(Model_2),
  PRESS(Model_3),
  PRESS(Model_4),
  PRESS(Model_5),
  PRESS(Model_6),
  PRESS(Model_7),
  PRESS(Model_8),
  PRESS(Model_9),
  PRESS(Model_10),
  PRESS(Model_11),
  PRESS(Model_12),
  PRESS(Model_13))
press
##------------------------------------------
#Sum of Square Prediction Error

Pred1<-predict(Model_1,test_PA_new);sum((Pred1-test_PA_new[,"y"])^2)
Pred2<-predict(Model_2,test_PA_new);sum((Pred2-test_PA_new[,"y"])^2)
Pred3<-predict(Model_3,test_PA_new);sum((Pred3-test_PA_new[,"y"])^2)
Pred4<-predict(Model_4,test_PA_new);sum((Pred4-test_PA_new[,"y"])^2)
Pred5<-predict(Model_5,test_PA_new);sum((Pred5-test_PA_new[,"y"])^2)
Pred6<-predict(Model_6,test_PA_new);sum((Pred6-test_PA_new[,"y"])^2)
Pred7<-predict(Model_7,test_PA_new);sum((Pred7-test_PA_new[,"y"])^2)
Pred8<-predict(Model_8,test_PA_new);sum((Pred8-test_PA_new[,"y"])^2)
Pred9<-predict(Model_9,test_PA_new);sum((Pred9-test_PA_new[,"y"])^2)
Pred10<-predict(Model_10,test_PA_new);sum((Pred10-test_PA_new[,"y"])^2)
Pred11<-predict(Model_11,test_PA_new);sum((Pred11-test_PA_new[,"y"])^2)
Pred12<-predict(Model_12,test_PA_new);sum((Pred12-test_PA_new[,"y"])^2)
Pred13<-predict(Model_13,test_PA_new);sum((Pred13-test_PA_new[,"y"])^2)

predr2 = rbind(Model_1 = sum((Pred1-test_PA_new[,"y"])^2),
               Model_2 = sum((Pred2-test_PA_new[,"y"])^2),
               Model_3 = sum((Pred3-test_PA_new[,"y"])^2),
               Model_4 = sum((Pred4-test_PA_new[,"y"])^2),
               Model_5 = sum((Pred5-test_PA_new[,"y"])^2),
               Model_6 = sum((Pred6-test_PA_new[,"y"])^2),
               Model_7 = sum((Pred7-test_PA_new[,"y"])^2),
               Model_8 = sum((Pred8-test_PA_new[,"y"])^2),
               Model_9 =sum((Pred9-test_PA_new[,"y"])^2),
               Model_10 =sum((Pred10-test_PA_new[,"y"])^2),
               Model_11 =sum((Pred11-test_PA_new[,"y"])^2),
               Model_12 =sum((Pred12-test_PA_new[,"y"])^2),
               Model_13 =sum((Pred13-test_PA_new[,"y"])^2))
predr2

#------------------------------------------------
#Kfold cross validation
set.seed(144)

c<-trainControl(method="cv",number=10)

summary(Model_1)
#Dataset: Accident_PA : 
#Model2Q
Kfold_Model1 = train(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
                       x37+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
                       x58+x59+x60+x61+x62+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                       x78+x79+x80+x81+x82+x83+x84+x85+x86, Accident_PA,  trControl=c, method = "lm" )
summary(Model3H)#Model 2
Kfold_Model2 = train(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                       x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                       x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + x72 + 
                       x35 + x71 + x75 + x78 + x70 + x77 + x41 + x80 + x60 + x54, Accident_PA, trControl=c,method="lm")
summary(Model4D)#Model3
Kfold_Model3 = train( y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                        x17 + x19 + x25 + x29 + x33 + x35 + x39 + x41 + x43 + x44 + 
                        x45 + x48 + x49 + x50 + x52 + x53 + x54 + x55 + x56 + x57 + 
                        x58 + x60 + x62 + x66 + x70 + x71 + x72 + x75 + x77 + x78 + 
                        x79 + x80 + x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model5F) #Model4
Kfold_Model4 = train( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                        x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                        x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + x72 + 
                        x35 + x71 + x75 + x78 + x70 + x77 + x41 + x80 + x60 + x54, Accident_PA, trControl=c,method="lm")

summary(Model_minBIC_26) #Model5
Kfold_Model5 = train(y ~ x2 + x3 + x4 + x10 + x12 + x13 + x15 + x17 + 
                       x19 + x25 + x29 + x33 + x39 + x43 + x44 + x45 + x47 + x48 + 
                       x49 + x57 + x79 + x82 + x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model_minBIC_27) #Model6
Kfold_Model6 = train(y ~ x2 + x3 + x4 + x10 + x12 + x13 + x15 + x17 + 
                       x19 + x25 + x29 + x33 + x39 + x43 + x44 + x45 + x47 + x48 + 
                       x49 + x57 + x72 + x79 + x82 + x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model_minBIC_28) #Model7
Kfold_Model7 = train(y ~ x2 + x3 + x4 + x10 + x12 + x13 + x15 + x17 + 
                       x19 + x25 + x29 + x33 + x35 + x39 + x43 + x44 + x45 + x47 + 
                       x48 + x49 + x57 + x72 + x79 + x82 + x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model_minCP_49J) #Model8
Kfold_Model8 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                       x17 + x19 + x25 + x29 + x33 + x35 + x39 + x41 + x43 + x44 + 
                       x45 + x47 + x48 + x49 + x50 + x52 + x54 + x55 + x57 + x58 + 
                       x67 + x68 + x72 + x73 + x76 + x79 + x81 + x83 + x84 + x85 + 
                       x86, Accident_PA, trControl=c,method="lm")

summary(Model_min1BIC_32) #Model9
Kfold_Model9 = train(y ~ x2 + x3 + x4 + x10 + x12 + x13 + x15 + x17 + 
                       x19 + x25 + x29 + x33 + x39 + x43 + x44 + x45 + x48 + x49 + 
                       x50 + x52 + x53 + x54 + x55 + x57 + x58 + x62 + x79 + x82 + 
                       x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model_min1BIC_33) #Model10
Kfold_Model10 = train(y ~ x2 + x3 + x4 + x10 + x12 + x13 + x15 + x17 + 
                        x19 + x25 + x29 + x33 + x39 + x43 + x44 + x45 + x48 + x49 + 
                        x50 + x52 + x53 + x54 + x55 + x57 + x58 + x62 + x72 + x79 + 
                        x82 + x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model_min1BIC_34) #Model11
Kfold_Model11 = train(y ~ x2 + x3 + x4 + x10 + x12 + x13 + x15 + x17 + 
                        x19 + x25 + x29 + x33 + x35 + x39 + x43 + x44 + x45 + x48 + 
                        x49 + x50 + x52 + x53 + x54 + x55 + x57 + x58 + x62 + x72 + 
                        x79 + x82 + x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model_minCP1_43A) #Model12
Kfold_Model12 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                        x17 + x19 + x25 + x29 + x33 + x35 + x39 + x41 + x43 + x44 + 
                        x45 + x48 + x49 + x50 + x52 + x53 + x54 + x55 + x56 + x57 + 
                        x58 + x60 + x62 + x67 + x68 + x72 + x73 + x76 + x79 + x82 + 
                        x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")

summary(Model_minCP1_44B) #Model13
Kfold_Model13 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                        x17 + x19 + x25 + x29 + x33 + x35 + x39 + x41 + x43 + x44 + 
                        x45 + x48 + x49 + x50 + x52 + x53 + x54 + x55 + x56 + x57 + 
                        x58 + x60 + x62 + x67 + x68 + x72 + x73 + x76 + x79 + x82 + 
                        x83 + x84 + x85 + x86, Accident_PA, trControl=c,method="lm")


rbind(Model_1 = Kfold_Model1$results,  
      Model_2 = Kfold_Model2$results,
      Model_3 = Kfold_Model3$results,
      Model_4 = Kfold_Model4$results,
      Model_5 = Kfold_Model5$results, 
      Model_6 = Kfold_Model6$results, 
      Model_7 = Kfold_Model7$results, 
      Model_8 = Kfold_Model8$results,
      Model_9 = Kfold_Model8$results,
      Model_10 = Kfold_Model8$results,
      Model_11 = Kfold_Model8$results,
      Model_12 = Kfold_Model8$results,
      Model_13 = Kfold_Model8$results)

#Prediction R square

PRM1 <- 1- PRESS(Model_1)/ sum(anova(Model_1)$`Sum Sq`)
PRM2 <- 1- PRESS(Model_2)/ sum(anova(Model_2)$`Sum Sq`)
PRM3 <- 1- PRESS(Model_3)/ sum(anova(Model_3)$`Sum Sq`)
PRM4 <- 1- PRESS(Model_4)/ sum(anova(Model_4)$`Sum Sq`)
PRM5 <- 1- PRESS(Model_5)/ sum(anova(Model_5)$`Sum Sq`)
PRM6 <- 1- PRESS(Model_6)/ sum(anova(Model_6)$`Sum Sq`)
PRM7 <- 1- PRESS(Model_7)/ sum(anova(Model_7)$`Sum Sq`)
PRM8 <- 1- PRESS(Model_8)/ sum(anova(Model_8)$`Sum Sq`)
PRM9 <- 1- PRESS(Model_9)/ sum(anova(Model_9)$`Sum Sq`)
PRM10 <- 1- PRESS(Model_10)/ sum(anova(Model_10)$`Sum Sq`)
PRM11 <- 1- PRESS(Model_11)/ sum(anova(Model_11)$`Sum Sq`)
PRM12 <- 1- PRESS(Model_12)/ sum(anova(Model_12)$`Sum Sq`)
PRM13 <- 1- PRESS(Model_13)/ sum(anova(Model_13)$`Sum Sq`)

rbind(PRM1, PRM2, PRM3, PRM4, PRM5, PRM6, PRM7, PRM8, PRM9, PRM10,
      PRM11, PRM12, PRM13)


comp1<- cbind(summary(Model_1)$df[1],summary(Model_1)$r.squared,summary(Model_1)$adj.r.squared,summary(Model_1)$fstatistic[1],
              AIC(Model_1),BIC(Model_1), PRESS(Model_1))
comp2<- cbind(summary(Model_2)$df[1],summary(Model_2)$r.squared,summary(Model_2)$adj.r.squared,summary(Model_2)$fstatistic[1],
              AIC(Model_2),BIC(Model_2), PRESS(Model_2))
comp3 <- cbind(summary(Model_3)$df[1],summary(Model_3)$r.squared,summary(Model_3)$adj.r.squared,summary(Model_3)$fstatistic[1],
               AIC(Model_3),BIC(Model_3), PRESS(Model_3))
comp4 <-cbind(summary(Model_4)$df[1],summary(Model_4)$r.squared,summary(Model_4)$adj.r.squared,summary(Model_4)$fstatistic[1],
              AIC(Model_4),BIC(Model_4), PRESS(Model_4))
comp5 <- cbind(summary(Model_5)$df[1],summary(Model_5)$r.squared,summary(Model_5)$adj.r.squared,summary(Model_5)$fstatistic[1],
               AIC(Model_5),BIC(Model_5), PRESS(Model_5))
comp6 <- cbind(summary(Model_6)$df[1],summary(Model_6)$r.squared,summary(Model_6)$adj.r.squared,summary(Model_6)$fstatistic[1],
               AIC(Model_6),BIC(Model_6), PRESS(Model_6))
comp7<- cbind(summary(Model_7)$df[1],summary(Model_7)$r.squared,summary(Model_7)$adj.r.squared,summary(Model_7)$fstatistic[1],
              AIC(Model_7),BIC(Model_7), PRESS(Model_7))
comp8<- cbind(summary(Model_8)$df[1],summary(Model_8)$r.squared,summary(Model_8)$adj.r.squared,summary(Model_8)$fstatistic[1],
              AIC(Model_8),BIC(Model_8), PRESS(Model_8))
comp9<- cbind(summary(Model_9)$df[1],summary(Model_9)$r.squared,summary(Model_9)$adj.r.squared,summary(Model_9)$fstatistic[1],
              AIC(Model_9),BIC(Model_9), PRESS(Model_9))
comp10 <- cbind(summary(Model_10)$df[1],summary(Model_10)$r.squared,summary(Model_10)$adj.r.squared,summary(Model_10)$fstatistic[1],
                AIC(Model_10),BIC(Model_10), PRESS(Model_10))
comp11<- cbind(summary(Model_11)$df[1],summary(Model_11)$r.squared,summary(Model_11)$adj.r.squared,summary(Model_11)$fstatistic[1],
               AIC(Model_11),BIC(Model_11), PRESS(Model_11))
comp12<- cbind(summary(Model_12)$df[1],summary(Model_12)$r.squared,summary(Model_12)$adj.r.squared,summary(Model_12)$fstatistic[1],
               AIC(Model_12),BIC(Model_12), PRESS(Model_12))
comp13<- cbind(summary(Model_13)$df[1],summary(Model_13)$r.squared,summary(Model_13)$adj.r.squared,summary(Model_13)$fstatistic[1],
               AIC(Model_13),BIC(Model_13), PRESS(Model_13))

rbind(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comp9, comp10, comp11, comp12, comp13)

table(Model_9$coefficients)
Model_min1BIC_32$coefficients
Model_9$coefficients
summary(Model_9)
anova(Model_9)
