# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(GGally)
library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(car)

raw_inp_df <- read.csv('online_shoppers_intention.csv')


#####Data Pre-Processing#########


###Find Missing Values#####
sum(is.na(raw_inp_df))
sapply(raw_inp_df, function(x) sum(is.na(x)))
raw_inp_df <- na.omit(raw_inp_df)


inp_df <- raw_inp_df
# inp_df$profitability <- ifelse(inp_df$Revenue==TRUE,"Yes","No")
# inp_df$weekend_indicator <- ifelse(inp_df$Weekend==TRUE,"weekend","weekday")


#Change Month Names
old <- c("Feb" , "Mar"  ,"May"  ,"Oct",  "June", "Jul"  ,"Aug",  "Nov",  "Sep",  "Dec" )
new <- c("February" , "March"  ,"May"  ,"October",  "June", "July"  ,"August",  "November",  "September",  "December" )
inp_df$Month <- factor(inp_df$Month, old, new)
inp_df$Month <- factor(inp_df$Month,levels = month.name)


#####Convert To Factor Levels For Remaining Variable#######
inp_df <- inp_df %>% 
  mutate(OperatingSystems = as.factor(OperatingSystems),
         Browser = as.factor(Browser),
         Region = as.factor(Region),
         TrafficType = as.factor(TrafficType),
         VisitorType = as.factor(VisitorType),
         Weekend = as.factor(Weekend),
         Revenue = as.factor(Revenue)
  )




######Revenue Wise Count########
prof_df <- ggplot(inp_df, aes(x=Revenue))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  geom_text(aes(label = ..count..), stat = "Count",colour = "white", size = 3,
            vjust = 1.5, position = position_dodge(.9))+
  theme_minimal()
prof_df



######Visitor Wise Count########
vis_df <- ggplot(inp_df, aes(x=VisitorType))+
  geom_bar(stat="count", width=0.65, fill="#00CC99")+
  geom_text(aes(label = ..count..), stat = "Count",colour = "white", size = 3,
            vjust = 1.5, position = position_dodge(.9))+
  scale_fill_brewer(palette = "Pastel1")
vis_df

######Month Wise Visitors Count########
monthly_visits_df <- inp_df%>%group_by(Month,VisitorType)%>%summarise(total=n())
vis_mnth_df <-ggplot(monthly_visits_df, aes( y=total, x=factor(Month), fill = VisitorType)) + 
  geom_col(position = position_dodge()) +
  #scale_fill_manual(values = c("#3db5ff", "#0099f9")) +
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#ffffff")
vis_mnth_df



######Month Wise Count########
vis_df <- ggplot(inp_df, aes(x=factor(Month, levels = month.name)))+
  geom_bar(stat="count", width=0.65, fill="#006666")+
  geom_text(aes(label = ..count..), stat = "Count",colour = "white", size = 3,
            vjust = 1.5, position = position_dodge(.9))+
  scale_fill_brewer(palette = "Pastel1")
vis_df


######Month Wise Revenue Count########
monthly_prof_df <- inp_df%>%group_by(Month,Revenue)%>%summarise(total=n())
prof_mnth_df <-ggplot(monthly_prof_df, aes( y=total, x=factor(Month), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  #scale_fill_manual(values = c("#3db5ff", "#0099f9")) +
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#ffffff")
prof_mnth_df
 

######Browser Wise Revenue Count########
browser_prof_df <- inp_df%>%group_by(Browser,Revenue)%>%summarise(total=n())
prof_brwser_df <-ggplot(browser_prof_df, aes( y=total, x=factor(Browser), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  #scale_fill_manual(values = c("#3db5ff", "#0099f9")) +
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#ffffff")
prof_brwser_df


######OS Wise Revenue Count########
os_prof_df <- inp_df%>%group_by(OperatingSystems,Revenue)%>%summarise(total=n())
prof_os_df <-ggplot(os_prof_df, aes( y=total, x=factor(OperatingSystems), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  #scale_fill_manual(values = c("#3db5ff", "#0099f9")) +
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#ffffff")
prof_os_df


######Region Wise Revenue Count########
region_prof_df <- inp_df%>%group_by(Region,Revenue)%>%summarise(total=n())
prof_region_df <-ggplot(region_prof_df, aes( y=total, x=factor(Region), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  #scale_fill_manual(values = c("#3db5ff", "#0099f9")) +
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#ffffff")
prof_region_df



######Traffic Wise Revenue Count########
traffic_prof_df <- inp_df%>%group_by(TrafficType,Revenue)%>%summarise(total=n())
prof_traffic_df <-ggplot(traffic_prof_df, aes( y=total, x=factor(TrafficType), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("#F0E442", "#0072B2")) +
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#000000")
prof_traffic_df


######Weekend-Weekday Wise Revenue Count########
wknd_prof_df <- inp_df%>%group_by(Weekend,Revenue)%>%summarise(total=n())
prof_wknd_df <-ggplot(wknd_prof_df, aes( y=total, x=factor(Weekend), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette="Spectral")
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#000000")
prof_wknd_df




##### Monthly Weekend-Weekday Traffic
mnthly_wknd_wekday_prof_df <- inp_df%>%group_by(Month,Revenue)%>%summarise(total=n())

mnthly_wknd_wekday_prof_grph <- 
  ggplot(mnthly_wknd_wekday_prof_df ,aes(x=Month, y=total, group=Revenue , fill = Revenue)) +
  geom_line(position = position_dodge(width = 0.9)) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Monthly Weekend Weekday Traffic") +
  geom_text(aes(label = total), position = position_dodge(0.9), vjust = 2, size = 2.5, color = "#000000")+
  ylab("Customers Who Purchased The Products")+
  scale_fill_brewer(palette="Set1")
mnthly_wknd_wekday_prof_grph


par(mfcol=c(4,3))
hist(osi$Administrative, freq=FALSE, main="Histogram of Administrative")
curve(dnorm(x, mean=mean(inp_df$Administrative), sd=sd(inp_df$Administrative)), add = TRUE, col='darkblue', lwd=2) 




######################################################
##################MODELLING###########################
######################################################



###Recode Values For Columns Containinig Too Many Factors#####
#####Copy processed DataFrame to other dataframe###### 
ml_inp_df <- inp_df
ml_inp_df$TrafficType <- recode(ml_inp_df$TrafficType, "1=1; 2=2; 3=3; 4=4; else=5")
ml_inp_df$Browser <- recode(ml_inp_df$Browser, "1=1; 2=2; else=1")

###Coralation Map#####
corr_map <- ggcorr(inp_df[, 1:10], method=c("everything", "pearson"), label=TRUE, hjust = .90, size = 3, layout.exp = 2)
corr_map


# partition data
set.seed(2)

# ##normal sampling##
# train.index <- sample(c(1:dim(ml_inp_df)[1]), dim(ml_inp_df)[1]*0.7)
# 
# ##Over sampling##
# train.index <- sample(c(1:dim(ml_inp_df)[1]), dim(ml_inp_df)[1], prob = ifelse(ml_inp_df$Revenue>0, 0.8, 0.01))
# #s <- sample(row.names(housing.df), 5, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))

#####Sampling Through Scaling#######
train.index <- createDataPartition(ml_inp_df$Revenue, p=0.65, list = FALSE, times = 1)


train_df <- ml_inp_df[train.index, ]
valid_df <- ml_inp_df[-train.index, ]

#The proportions of the T and F are equal
prop.table(table(train_df$Revenue)) 
prop.table(table(valid_df$Revenue))


preprocess <- preProcess(train_df, method = c("center", "scale"))
trainprocessed <- predict(preprocess, train_df)
validprocessed <- predict(preprocess, valid_df)



########################################
#########Decision Tree##################

default_dt <- rpart(Revenue ~ ., data = trainprocessed, parms = list(split = 'information'), method = "class")
prp(default_dt, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(default_dt$frame$var[default_dt$frame$var == "<leaf>"])


prp(default_dt, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(default_dt$frame$var == "<leaf>", 'gray', 'white'))


# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
inp_df_pred_train <- predict(default_dt,trainprocessed,type = "class")
# generate confusion matrix for training data
confusionMatrix(inp_df_pred_train, as.factor(trainprocessed$Revenue))

### repeat the code for the validation set
inp_df_pred_valid <- predict(default_dt,validprocessed,type = "class")
confusionMatrix(inp_df_pred_valid, as.factor(validprocessed$Revenue))


# Check actual and predicted records for decision tree####
library(pROC)
check_perfor_dt <- data.frame(actual = validprocessed$Revenue, predicted = as.numeric(inp_df_pred_valid))

r2 <- roc(check_perfor_dt$actual, check_perfor_dt$predicted)
plot.roc(r2)
# compute auc
auc(r2)




########Logistic Regression###################
##############################################

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
options(scipen=999)
logit_reg <- glm(Revenue ~ ., data = trainprocessed, family = "binomial") 
summary(logit_reg)
vif(logit_reg)

#### Table 10.3

# use predict() with type = "response" to compute predicted probabilities. 
logit_reg_pred <- predict(logit_reg, validprocessed[, -18], type = "response")


# 
# full.logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
# empty.logit.reg  <- glm(Personal.Loan ~ 1,data = train.df, family= "binomial")
# summary(empty.logit.reg)
# 
# backwards = step(full.logit.reg)
# summary(backwards)
# 
# back2 <- glm(Personal.Loan ~ Income + Education + CD.Account,data = train.df, family= "binomial")
# summary(back2)
# 
# forwards = step(empty.logit.reg,scope=list(lower=formula(empty.logit.reg),upper=formula(full.logit.reg)), direction="forward",trace=0)
# formula(forwards)
# 
# stepwise = step(empty.logit.reg,scope=list(lower=formula(empty.logit.reg),upper=formula(full.logit.reg)), direction="both",trace=0)
# formula(stepwise)



# Check actual and predicted records
logit_reg_pred <- ifelse(logit_reg_pred > 0.5, "TRUE", "FALSE")

###Performance Dataframe containing actual vs predicted values####
check_perfor <- data.frame(actual = validprocessed$Revenue, predicted = logit_reg_pred)
check_perfor$predicted = as.factor(check_perfor$predicted)

########Validation Metrics#######
table(logit_reg_pred, validprocessed$Revenue)
mean(logit_reg_pred == validprocessed$Revenue)

####Confusion Matrix#####
confusionMatrix(check_perfor$actual, check_perfor$predicted)


#### ROC Curve For Logistic Regression######
library(pROC)
check_perfor <- check_perfor%>%
  mutate(actual_mod = ifelse(actual==TRUE,1,0),
         predicted_mod = ifelse(predicted==TRUE,1,0))
r1 <- roc(check_perfor$actual_mod, check_perfor$predicted_mod)
plot.roc(r1)
# compute auc
auc(r1)






###########Random Forest###############
#######################################


library(randomForest)
## random forest
rf <- randomForest(as.factor(Revenue) ~ ., data = trainprocessed, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)


## confusion matrix
rf_pred <- predict(rf, validprocessed)
confusionMatrix(rf_pred, as.factor(validprocessed$Revenue))




##############################################
#############BOOSTING#########################
##############################################

library(adabag)

trainprocessed$Revenue <- as.factor(trainprocessed$Revenue)

set.seed(1)
boost <- boosting(Revenue ~ ., data = trainprocessed)
pred <- predict(boost, validprocessed)
confusionMatrix(as.factor(pred$class), as.factor(validprocessed$Revenue))



#####Insights and Suggestions#######

# 
#The significant importance of PageValue comprehends that the customers
#who will check out different products and its recommendations. 
#
#Hence a good amount of improvement on recommendation engines and bundle packages 
#would bring in more conversions for the website. 
#This includes more products exploiting the long tail effect in e-commerce could drive more revenue.
# 
#Some pointers to improve the conversion rate 
# 
#Minimalist and attractive UI Pages To retain more users on the website pages
#Being informative to the users about product information and their prices
#Bringing more users on the website through inorganic promotions, coupons and ads
#The bounce rate of a website can be reduced by implementing faster refresh rate and
#  creating attractive landing page which has highly good deals on products and offers exclusively for the visitors
#Also creating the personalized emails for existing members abd introducing customer loyalty programs would help in bringing more retention.
#
#
