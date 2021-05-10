library(tidyverse)
BASEBALL<- read_csv("game_logs.csv")
#Replacing zero values for attendance
 BASEBALL<-BASEBALL %>% drop_na()
double_header <- data.frame()
 for(i in 2:nrow(BASEBALL))  {
    if(BASEBALL[i-1,"park_id"]== BASEBALL[i,"park_id"]) {
       double_header <- rbind(double_header, BASEBALL[i-1,])
       double_header <- rbind(double_header, BASEBALL[i,])
    }
 }
attach(BASEBALL)
BASEBALL <- BASEBALL %>% select(-c(ends_with("_triple_plays"),ends_with("_putouts"),
                                   ends_with("_team_earned_runs"),ends_with("_rbi"),ends_with("_at_bats")
                                   , ends_with("_game_number"),day_of_week,park_id,ends_with("_name")
                                  ))
double_header<- double_header %>% select(-c(ends_with("_triple_plays"),ends_with("_putouts"),
                                               ends_with("_team_earned_runs"),ends_with("_rbi"),ends_with("_at_bats")
                                               , ends_with("_game_number"),day_of_week,park_id,ends_with("_name")
                                                ))
BASEBALL <- BASEBALL %>% select(-c(acquisition_info))
double_header <- double_header %>% select(-c(acquisition_info))
BASEBALL <- BASEBALL %>% mutate(v_league=as.factor(v_league),h_league=as.factor(h_league),day_night=as.factor(day_night))
# Changing Date Format
BASEBALL <-BASEBALL %>% mutate(date=trunc(BASEBALL$date*10^(-4)))
double_header <-double_header %>% mutate(date=trunc(double_header$date*10^(-4)))

names(BASEBALL)[names(BASEBALL)=="date"]<-"year"
names(double_header)[names(double_header)=="date"]<-"year"

#Point Diff
BASEBALL<- BASEBALL %>% mutate(score_differences = h_score-v_score)
double_header <- double_header %>% mutate(score_differences = h_score-v_score)


#Blow out games
Blowout_games <- BASEBALL %>% filter(abs(v_score-h_score)>=5)

#games from 1961
BASEBALL1960s <- BASEBALL %>% filter(between(year,1961,1969))
BASEBALL1980s <- BASEBALL %>% filter(between(year,1980,1989))
BASEBALL2000s <- BASEBALL %>% filter(between(year,2000,2009))
BASEBALL2010s <- BASEBALL %>% filter(between(year,2010,2016))


#something
Leagues <- BASEBALL %>%group_by(h_league,year)%>%summarise(meanruns=mean(h_score))
ggplot(Leagues,aes(year,meanruns))+geom_point(aes(color=h_league))

ggplot(Leagues,aes(year,meanruns))+geom_boxplot(aes(fill=h_league))
#mean_runs
BASEBALLsummary <- BASEBALL %>% group_by(year) %>% summarise(mean_homeruns = mean(h_homeruns+v_homeruns),
                                                             mean_vistorruns = mean(v_homeruns),
                                                             mean_attendance=mean(attendance))
BASEBALLsummary

# Plot of mean_runs
ggplot(BASEBALLsummary,aes(year,mean_runs))+geom_line()

# Double Header


ggplot(BASEBALLsummary,aes(year,mean_homeruns))+geom_line()
ggplot(BASEBALLsummary,aes(year,mean_vistorruns))+geom_line()
ggplot(BASEBALLsummary,aes(year,total_runs))+geom_line()
ggplot(BASEBALLsummary,aes(year,mean_attendance))+geom_line()


lapply(BASEBALLsummary,min)
lapply(BASEBALLsummary,max)

Blowouts1960s <- Blowout_games %>% filter(between(year,1961,1969))
Blowouts1980s <- Blowout_games %>% filter(between(year,1980,1989))
Blowouts2000s <- Blowout_games %>% filter(between(year,2000,2009))
Blowouts2010s  <- Blowout_games %>% filter(between(year,2010,2016))



#Models from 1961-1969
library(leaps)
#Subset Selection
BASEBALL1960s <- BASEBALL1960s %>% select(-ends_with("_league"),-ends_with("_score"))
BASEBALL_subsets_selection <-regsubsets(score_differences~.,BASEBALL1960s)
BASEBALL_subsets_selectionsum <-summary(BASEBALL_subsets_selection)
which.min(BASEBALL_subsets_selectionsum$bic)
which.min(BASEBALL_subsets_selectionsum$cp)
coef(BASEBALL_subsets_selection,8)


#Test set, training sets
BASEBALL1960s.data <- BASEBALL1960s %>% select(ends_with("_hits"),ends_with("_walks"),ends_with("_left_on_base"),
                                          ends_with("_double_plays"),score_differences)
BASEBALL1960s.train <- BASEBALL1960s.data[-sample(1:nrow(BASEBALL1960s.data),1472),]
BASEBALL1960s.test <- BASEBALL1960s.data[sample(1:nrow(BASEBALL1960s.data),1472),]

BASEBALL1960s.lm <- glm(score_differences~.,data = BASEBALL1960s.train)
BASEBALL1960s.lm.predict <- predict(BASEBALL1960s.lm,newdata=BASEBALL1960s.test)
MSE1960s1 = mean((BASEBALL1960s.test$score_differences - BASEBALL1960s.lm.predict)^2)
summary(BASEBALL1960s.lm)

#FORWARD SELECTION 1960s
attach(BASEBALL1960s)
BASEBALL1960s_forward_selection<-regsubsets(score_differences~.,BASEBALL1960s,method = "forward")
BASEBALL1960s_forward_selectionsum <-summary(BASEBALL1960s_forward_selection)
which.min(BASEBALL1960s_forward_selectionsum$bic)
which.min(BASEBALL1960s_forward_selectionsum$cp)
coef(BASEBALL1960s_forward_selection,8)

BASEBALL1960s.data1 <- BASEBALL1960s %>% select(ends_with("_hits"),ends_with("_pitchers_used"),
                                                c(v_double_plays,h_left_on_base,h_walks,v_homeruns,score_differences))

BASEBALL1960s.train2 <- BASEBALL1960s.data1[-sample(1:nrow(BASEBALL1960s.data1),1472),]
BASEBALL1960s.test2 <- BASEBALL1960s.data1[sample(1:nrow(BASEBALL1960s.data1),1472),]

BASEBALL1960s.lm.forward. <-glm(score_differences~.,data=BASEBALL1960s.train2)
BASEBALL1960s.lm.forward.predict <-predict(BASEBALL1960s.lm.forward.,newdata = BASEBALL1960s.test2)
MSE1960s2 = mean((BASEBALL1960s.test2$score_differences - BASEBALL1960s.lm.forward.predict)^2)

#BACKWARD ELMINATION/ This is the same model as the best subsets model
BASEBALL1960s_backward_elmin <- regsubsets(score_differences~.,BASEBALL1960s,method = "backward")
BASEBALL1960s_backward_elminsum <- summary(BASEBALL1960s_backward_elmin)
which.min(BASEBALL_subsets_selectionsum$bic)
which.min(BASEBALL_subsets_selectionsum$cp)
coef(BASEBALL1960s_backward_elmin,8)
coef(BASEBALL_subsets_selection,8)


# CROSS VALIDATION 1960s
library(boot)

CVsubsetsel1960s <-cv.glm(data = BASEBALL1960s.train,BASEBALL1960s.lm,K=5)
CVsubsetsel1960s.k10 <-cv.glm(data = BASEBALL1960s.train,BASEBALL1960s.lm,K=10)
CVsubsetsel1960s$delta
CVsubsetsel1960s.k10$delta
CVforwardsel1960s <- cv.glm(data=BASEBALL1960s.train2,BASEBALL1960s.lm.forward.,K=5)
CVforwardsel1960s.k10 <- cv.glm(data=BASEBALL1960s.train2,BASEBALL1960s.lm.forward.,K=10)
CVforwardsel1960s$delta
CVforwardsel1960s.k10$delta

# The best model is the subset selection for the 1960s era

#Model fitting for 1980S

#BEST SUBSETS
BASEBALL1980s_subsetsel <-regsubsets(score_differences~.,BASEBALL1980s)
BASEBALL1980s_subsetselsum <- summary(BASEBALL1980s_subsetsel)
which.min(BASEBALL1980s_subsetselsum$cp)
coef(BASEBALL1980s_subsetsel,5)
coef(BASEBALL1980s_subsetsel,9)

#training and test sets

BASEBALL1980s_subsetsel.train <-BASEBALL1980s[-sample(1:nrow(BASEBALL1980s),2034),]
BASEBALL1980s_subsetsel.test <- BASEBALL1980s[sample(1:nrow(BASEBALL1980s),2034),]

#BIC MODEL 
BASEBALL1980s.subsetselbic <- glm(score_differences~year+v_league+h_score+length_outs+v_errors,
                                 data = BASEBALL1980s_subsetsel.train)
summary(BASEBALL1980s.subsetselbic)
BASEBALL1980s.subsetselbic.predict <- predict(BASEBALL1980s.subsetselbic,newdata = BASEBALL1980s_subsetsel.test)
MSE1980s1 <- mean((BASEBALL1980s_subsetsel.test$score_differences - BASEBALL1980s.subsetselbic.predict)^2)

#CP MODEL
BASEBALL1980s.subsetselcp <- glm(score_differences~year+h_score+length_outs+day_night+
                                     v_triples+v_strikeouts+v_pitchers_used+h_left_on_base+
                                     h_errors,data = BASEBALL1980s_subsetsel.train )
summary(BASEBALL1980s.subsetselcp)
BASEBALL1980s.subsetselcp.predict <- predict(BASEBALL1980s.subsetselcp,newdata = BASEBALL1980s_subsetsel.test)
MSE1980s1.1 <- mean((BASEBALL1980s_subsetsel.test$score_differences - BASEBALL1980s.subsetselcp.predict)^2)


# FORWARD SELECTION 1980s
BASEBALL1980s.data <-BASEBALL1980s %>% select(-ends_with("_score"))
BASEBALL1980s_forwardsel <-regsubsets(score_differences~.,BASEBALL1980s.data,method = "forward")
BASEBALL1980s_forwardselsum <- summary(BASEBALL1980s_forwardsel)
which.min(BASEBALL1980s_forwardselsum$bic)
which.min(BASEBALL1980s_forwardselsum$cp)
coef(BASEBALL1980s_forwardsel,9)

BASEBALL1980s_forwardsel.train <-BASEBALL1980s[-sample(1:nrow(BASEBALL1980s.data),2034),]
BASEBALL1980s_.forwardsel.test <- BASEBALL1980s[sample(1:nrow(BASEBALL1980s.data),2034),]

BASEBALL1980s.fwd.lm <- glm(score_differences~v_doubles+v_strikeouts+v_pitchers_used+
                                h_hits+h_doubles+h_walks+h_strikeouts+h_pitchers_used+
                                h_league,data = BASEBALL1980s_forwardsel.train)
summary(BASEBALL1980s.fwd.lm)
BASEBALL1980s.fwd.lm.predict <- predict(BASEBALL1980s.fwd.lm,newdata = BASEBALL1980s_.forwardsel.test)
MSE1980s2 <- mean((BASEBALL1980s_subsetsel.test$score_differences - BASEBALL1980s.fwd.lm.predict)^2)

# BACKWARD ELMINATION
BASEBALL1980s_backwardelim <-regsubsets(score_differences~.,BASEBALL1980s,method = "backward")
BASEBALL1980s_backwardelimsum <- summary(BASEBALL1980s_backwardelim)
which.min(BASEBALL1980s_backwardelimsum$bic)
which.min(BASEBALL1980s_backwardelimsum$cp)
coef(BASEBALL1980s_backwardelim,5)
coef(BASEBALL1980s_backwardelim,9)

BASEBALL1980s_backwardelmin.train <-BASEBALL1980s[-sample(1:nrow(BASEBALL1980s),2034),]
BASEBALL1980s_backwardelmin.test <- BASEBALL1980s[sample(1:nrow(BASEBALL1980s),2034),]

BASEBALL1980s.lm.bwd <- glm(score_differences~year+v_league+h_score+length_outs+v_hits+v_errors+
                               h_triples+h_walks+h_pitchers_used,data=BASEBALL1980s_backwardelmin.train)
                                
summary(BASEBALL1980s.lm.bwd)
BASEBALL1980s.lm.bwd.predict <- predict(BASEBALL1980s.lm.bwd,newdata = BASEBALL1980s_backwardelmin.test)
MSE1980s3<- mean((BASEBALL1980s_backwardelmin.test$score_differences - BASEBALL1980s.lm.bwd.predict)^2)


#CROSS VALIDATION 1980s

  ## SUBSET SELECTION
    ####5 folds
CVsubsetselmod1 <- cv.glm(data=BASEBALL1980s_subsetsel.train,BASEBALL1980s.subsetselbic,K=5)
CVsubsetselmod2 <- cv.glm(data=BASEBALL1980s_subsetsel.train,BASEBALL1980s.subsetselcp,K=5)

CVsubsetselmod1$delta
CVsubsetselmod2$delta
    ### 10 folds
CVsubsetselmod1.10.1980sfolds <- cv.glm(data=BASEBALL1980s_subsetsel.train,BASEBALL1980s.subsetselbic,K=10)
CVsubsetselmod2.10.1980sfolds <- cv.glm(data=BASEBALL1980s_subsetsel.train,BASEBALL1980s.subsetselcp,K=10)
CVsubsetselmod1.10.1980sfolds$delta
CVsubsetselmod2.10.1980sfolds$delta

  ## FORWARD SELECTION
    ### 5 folds
CVforwardselmod1980s5folds <- cv.glm(data=BASEBALL1980s_forwardsel.train,BASEBALL1980s.fwd.lm,K=5)
CVforwardselmod1980s5folds$delta
    ### 10 folds
CVforwardselmod1980s10folds <- cv.glm(data=BASEBALL1980s_forwardsel.train,BASEBALL1980s.fwd.lm,K=10)
CVforwardselmod1980s10folds$delta

  ## BACKWARD SELECTION
   ### 5 folds
CVbackwardelmin1980s5folds <- cv.glm(data=BASEBALL1980s_backwardelmin.train,BASEBALL1980s.lm.bwd,K=5)
CVbackwardelmin1980s5folds$delta
   ### 10 folds
CVbackwardelmin1980s10folds <- cv.glm(data=BASEBALL1980s_backwardelmin.train,BASEBALL1980s.lm.bwd,K=10)
CVbackwardelmin1980s10folds$delta

# The best model is the 1980s era is backward elmination

# Model fitting for 2000s

#Best Subsets
BASEBALL2000s.data <- BASEBALL2000s %>% select(-ends_with("_score"))
subset_selection2000s <- regsubsets(score_differences~.,data=BASEBALL2000s.data)
subset_selection2000s.sum <- summary(subset_selection2000s)
which.min(subset_selection2000s.sum$bic)
which.min(subset_selection2000s.sum$cp)
coef(subset_selection2000s,8)

BASEBALL2000s.data_train <- BASEBALL2000s.data[-sample(1:nrow(BASEBALL2000s.data),2430),]
BASEBALL2000s.data_test <-  BASEBALL2000s.data[sample(1:nrow(BASEBALL2000s.data),2430),]

BASEBALL2000s_subsetsel <- glm(score_differences~ v_hits+v_walks+v_left_on_base+v_double_plays+
                                   h_hits+h_walks+h_left_on_base+h_double_plays,data=BASEBALL2000s.data_train)
summary(BASEBALL2000s_subsetsel)
BASEBALL2000s_subsetsel.predict <- predict(BASEBALL2000s_subsetsel,newdata = BASEBALL2000s.data_test)
MSE2000s1 <- mean((BASEBALL2000s.data_test$score_differences - BASEBALL2000s_subsetsel.predict)^2)

#FORWARD SELECTION
forward_selection2000s <- regsubsets(score_differences~.,data=BASEBALL2000s.data,method = "forward")
forward_selection2000s.sum <-summary(forward_selection2000s)
which.min(forward_selection2000s.sum$bic)
which.min(forward_selection2000s.sum$cp)
coef(forward_selection2000s,8)

BASEBALL2000s_forwardsel <- glm(score_differences~v_hits+v_homeruns+v_walks+
                                  v_left_on_base+h_hits+h_homeruns+h_walks+h_left_on_base,
                                data = BASEBALL2000s.data_train)
BASEBALL2000s_forwardsel.predict <- predict(BASEBALL2000s_forwardsel,newdata = BASEBALL2000s.data_test)
MSE2000s2 <- mean((BASEBALL2000s.data_test$score_differences - BASEBALL2000s_forwardsel.predict)^2)

#BACKWARD ELIMINATION
backward_elimination2000s <- regsubsets(score_differences~.,data = BASEBALL2000s.data,method = "backward")
backward_elimination2000s.sum <- summary(backward_elimination2000s)
which.min(backward_elimination2000s.sum$bic)
which.min(backward_elimination2000s.sum$cp)
coef(backward_elimination2000s,8)

# Same Model as best subsets

# CROSS VALIDATION 2000s
 ##BEST SUBSETS
  ### 5 folds
CVsubsetsel2000s.k5 <- cv.glm(BASEBALL2000s.data_train,BASEBALL2000s_subsetsel,K=5)
CVsubsetsel2000s.k5$delta
  ### 10 folds
CVsubsetsel2000s.k10 <- cv.glm(BASEBALL2000s.data_train,BASEBALL2000s_subsetsel,K=10)
CVsubsetsel2000s.k10$delta
  ### 5 folds
CVforwardsel2000s.k5 <- cv.glm(BASEBALL2000s.data_train,BASEBALL2000s_forwardsel,K=5)
CVforwardsel2000s.k5$delta
  ### 10 folds
CVforwardsel2000s.k10 <- cv.glm(BASEBALL2000s.data_train,BASEBALL2000s_forwardsel,K=10)
CVforwardsel2000s.k10$delta

# THIS MODEL IS THE BEST SUBSET SELECTION for 2000s

# Model fitting for 2010s

#Best subset selection 2010s
BASEBALL2010s.data <- BASEBALL2010s %>% select(-ends_with("_score"))

BASEBALL2010s.data_train <- BASEBALL2010s.data[-sample(1:nrow(BASEBALL2010s.data),1701),]
BASEBALL2010s.data_test <-  BASEBALL2010s.data[sample(1:nrow(BASEBALL2010s.data),1701),]

subset_selection2010s <- regsubsets(score_differences~.,data = BASEBALL2010s.data)
subset_selection2010s.sum <- summary(subset_selection2010s)
which.min(subset_selection2010s.sum$bic)
which.min(subset_selection2010s.sum$cp)
coef(subset_selection2010s,8)

BASEBALL2010s_subsetsel <-glm(score_differences~ v_hits+v_walks+v_left_on_base+v_double_plays+
                                h_hits+h_walks+h_left_on_base+h_double_plays,data=BASEBALL2010s.data_train)
summary(BASEBALL2010s_subsetsel)
BASEBALL2010s_subsetsel.predict <- predict(BASEBALL2010s_subsetsel,newdata = BASEBALL2010s.data_test)
MSE2010s1 <- mean((BASEBALL2010s.data_test$score_differences - BASEBALL2010s_subsetsel.predict)^2)

#FORWARD SELECTION
forward_selection2010s <- regsubsets(score_differences~.,data = BASEBALL2010s.data,method="forward")
forward_selection2010s.sum <- summary(forward_selection2010s)
which.min(forward_selection2010s.sum$cp)
which.min(forward_selection2010s.sum$cp)
coef(forward_selection2010s,8)

BASEBALL2010s_forwardsel <- glm(score_differences~v_hits+v_homeruns+v_walks+v_left_on_base+h_hits+
                                h_homeruns+h_walks+h_double_plays,
                                data = BASEBALL2010s.data_train)
summary(BASEBALL2010s_forwardsel)
BASEBALL2010s_forwardsel.predict <- predict(BASEBALL2010s_forwardsel,newdata = BASEBALL2010s.data_test )
MSE2010s2 <- mean((BASEBALL2010s.data_test$score_differences - BASEBALL2010s_forwardsel.predict)^2)

# BACKWARD ELIMINATION
backward_elimination2010s <- regsubsets(score_differences~., data = BASEBALL2010s.data,method = "backward")
backward_elimination2010s.sum <- summary(backward_elimination2010s)
which.min(backward_elimination2010s.sum$bic)
which.min(backward_elimination2010s.sum$cp)
coef(subset_selection2010s,8)

# SAME MODEL AS IN SUBSET SELECTION

# CROSS VALIDATION 2010s

### 5 folds
CVsubsetsel2010s.k5 <- cv.glm(BASEBALL2010s.data_train,BASEBALL2010s_subsetsel,K=5)
CVsubsetsel2010s.k5$delta
### 10 folds
CVsubsetsel2010s.k10 <- cv.glm(BASEBALL2010s.data_train,BASEBALL2010s_subsetsel,K=10)

CVsubsetsel2010s.k10$delta
### 5 folds
CVforwardsel2010s.k5 <- cv.glm(BASEBALL2010s.data_train,BASEBALL2010s_forwardsel,K=5)
CVforwardsel2010s.k5$delta
### 10 folds
CVforwardsel2010s.k10 <- cv.glm(BASEBALL2010s.data_train,BASEBALL2010s_forwardsel,K=10)
CVforwardsel2010s.k10$delta

# THE BEST MODEL FOR THE 2010s is the best subset selection

# What happens in Blow Out Games

#Blow outs 1960s

Blowouts1960s.bestmodel <- glm(score_differences~v_hits+h_hits+
                                 v_walks+h_walks+v_left_on_base+h_left_on_base+
                                 v_double_plays+h_double_plays,
                               data = Blowouts1960s)
summary(Blowouts1960s.bestmodel)
Blowouts1960s.bestmodel.predict <- predict(Blowouts1960s.bestmodel,data=Blowouts1960s)
Blowout_MSE1960s <- mean((Blowouts1960s$score_differences - Blowouts1960s.bestmodel.predict)^2)

# Blowouts 1980s

Blowouts1980s.bestmodel<- glm( score_differences~year+v_league+h_score+length_outs+v_hits+v_errors+ h_triples+ h_walks
                               + h_pitchers_used,data=Blowouts1980s)
summary(Blowouts1980s.bestmodel)
blowouts1980s.bestmodel.predict <- predict(Blowouts1980s.bestmodel,data=Blowouts1980s)
Blowout_MSE1980s <- mean((Blowouts1980s$score_differences - blowouts1980s.bestmodel.predict)^2)

# Blowouts 2000s
Blowouts2000s.bestmodel<- glm( score_differences~v_hits + v_walks + v_left_on_base + v_double_plays+ 
                                 h_hits + h_walks
                               + h_left_on_base+h_double_plays
                               ,data=Blowouts2000s)
summary(Blowouts2000s.bestmodel)
blowouts2000s.bestmodel.predict <- predict(Blowouts2000s.bestmodel,data=Blowouts2000s)
Blowout_MSE2000s <- mean((Blowouts2000s$score_differences - blowouts2000s.bestmodel.predict)^2)

#2010s
Blowouts2010s.bestmodel<- glm(score_differences~v_hits+v_walks+v_left_on_base
                              +v_double_plays+h_hits+h_walks+
                              +h_left_on_base + h_double_plays
                              ,data=Blowouts2010s)
summary(Blowouts2010s.bestmodel)
blowouts2010s.bestmodel.predict <- predict(Blowouts2010s.bestmodel,data=Blowouts2010s)
Blowout_MSE2010s <- mean((Blowouts2010s$score_differences - blowouts2010s.bestmodel.predict)^2)

# Win probabilitys

#Win Prob for 1960s

prob = 0;
for(i in 1:length(BASEBALL1960s.lm.predict)){
  if(BASEBALL1960s.lm.predict[i]==abs(BASEBALL1960s.lm.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(BASEBALL1960s.lm.predict)

#win probability 1980s

prob = 0;
for(i in 1:length(BASEBALL1980s.lm.bwd.predict)){
  if(BASEBALL1980s.lm.bwd.predict[i]==abs(BASEBALL1980s.lm.bwd.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(BASEBALL1980s.lm.bwd.predict)

#Win prob 2000s

prob = 0;
for(i in 1:length(BASEBALL2000s_subsetsel.predict)){
  if(BASEBALL2000s_subsetsel.predict[i]==abs(BASEBALL2000s_subsetsel.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(BASEBALL2000s_subsetsel.predict)

#win prob 2010s

prob = 0;
for(i in 1:length(BASEBALL2010s_subsetsel.predict)){
  if(BASEBALL2010s_subsetsel.predict[i]==abs(BASEBALL2010s_subsetsel.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(BASEBALL2010s_subsetsel.predict)


probs <- c(0.530,0.525,0.559,0.507)
mean(probs)


# BLOWOUTS 1960s
prob = 0;
for(i in 1:length(Blowouts1960s.bestmodel.predict)){
  if(Blowouts1960s.bestmodel.predict[i]==abs(Blowouts1960s.bestmodel.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(Blowouts1960s.bestmodel.predict)


#Blowout1980s
prob = 0;
for(i in 1:length(blowouts1980s.bestmodel.predict)){
  if(blowouts1980s.bestmodel.predict[i]==abs(blowouts1980s.bestmodel.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(blowouts1980s.bestmodel.predict)


#blowouts2000s

prob = 0;
for(i in 1:length(blowouts2000s.bestmodel.predict)){
  if(blowouts2000s.bestmodel.predict[i]==abs(blowouts2000s.bestmodel.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(blowouts2000s.bestmodel.predict)

# blowouts2010s
prob = 0;
for(i in 1:length(blowouts2010s.bestmodel.predict)){
  if(blowouts2010s.bestmodel.predict[i]==abs(blowouts2010s.bestmodel.predict[i])){
    prob=prob+1;
  }
}
prob= prob/length(blowouts2010s.bestmodel.predict)

probs = c(0.524,0.516,0.509,0.517)
mean(probs)
 


