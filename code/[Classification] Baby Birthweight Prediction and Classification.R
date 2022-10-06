############################
# Installation of packages #
############################
#install.packages("readr")
#install.packages("caret")
#install.packages("class")
#install.packages("nnet")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("reshape2")



################
# Call library #
################
library(readr)
library(tidyverse)
library(ggplot2)
library(caret)
library(class)
library(nnet)



#########################
# Set Working Directory #
#########################
setwd("")
getwd()



###################################
# Import data: Baby's birthweight #
###################################

birthweight = readr::read_csv(file="2008_births.csv")
str(birthweight)
dim(birthweight)
nrow(birthweight)
ncol(birthweight)
colnames(birthweight)
head(birthweight)


###################################
# Preprocessing and Data cleaning #
###################################

# Target variable: Baby Birthweight - BPound and BOUNCE (to be combined)
# Data reduction: Remove unrelevant features in original dataset 
#                 Features about post birth are removed
#                 Column about birth details such as date are removed
# Coding categorical data
# Impute missing data

keep_col = c('SEX', 'MARITAL','FAGE', 'GAINED', 'VISITS', 'MAGE', 
            'FEDUC', 'MEDUC', 'TOTALP', 'BDEAD', 'TERMS',  
            #'RACEMOM', 'RACEDAD', 'HISPMOM', 'HISPDAD', 'LOUTCOME',
            'PLURAL','WEEKS','CIGNUM', 'DRINKNUM', 'ANEMIA', 'BPOUND','BOUNCE',
            'CARDIAC', 'ACLUNG', 'DIABETES', 'HERPES', 'HYDRAM', 
            'HEMOGLOB', 'HYPERCH', 'HYPERPR', 'ECLAMP', 'CERVIX',
            'PINFANT','PRETERM','RENAL','RHSEN','UTERINE')

quantitative_col =c('FAGE', 'GAINED', 'VISITS', 'MAGE', 'BWEIGHT','PLURAL',
                    'FEDUC', 'MEDUC', 'TOTALP', 'BDEAD', 'TERMS', 
                    #'RACEMOM', 'RACEDAD', 'HISPMOM', 'HISPDAD', 'BPOUND','BOUNCE',
                    'WEEKS','CIGNUM', 'DRINKNUM')

# qualitative_tf_col =c('ANEMIA', 'CARDIAC', 'ACLUNG', 'DIABETES', 'HERPES', 'HYDRAM', 
#                      'HEMOGLOB', 'HYPERCH', 'HYPERPR', 'ECLAMP', 'CERVIX', 'PINFANT',
#                      'PRETERM','RENAL','RHSEN','UTERINE')
# 
# qualitative_bin_col =c('SEX', 'MARITAL')

qualitative_col =c('SEX', 'MARITAL','ANEMIA',
                  'CARDIAC', 'ACLUNG', 'DIABETES', 'HERPES', 'HYDRAM', 
                  'HEMOGLOB', 'HYPERCH', 'HYPERPR', 'ECLAMP', 'CERVIX',
                  'PINFANT','PRETERM','RENAL','RHSEN','UTERINE')


# Check missing value
# Missing values are recorded as 9, 98 or 99 across different columns

birthweight_clean = birthweight %>%  
      select(all_of(keep_col)) %>%
      filter_at(all_of(qualitative_col), all_vars(. != 9)) %>%
      filter(!rowSums(.==99)) %>%
      filter(!rowSums(.==98))

# Check no more odd max value for all attributes      
summary(birthweight_clean)


# Transfomrmation of attribute
birthweight_clean = 
  birthweight_clean %>% 
  mutate(BWEIGHT=BPOUND+(0.0625*BOUNCE)) %>%
  mutate(BWEIGHT_COND=case_when(
                      BWEIGHT <5.5 ~ 0,
                      BWEIGHT >=5.5 & BWEIGHT <8.8 ~ 1,
                      BWEIGHT >=8.8 ~ 2)
         )%>%
  select(-c("BPOUND","BOUNCE")) 
  


# Dummy coding nominal features
# Convert categorical features to factor
birthweight_clean = 
  birthweight_clean %>% 
  
  mutate(SEX_dummy_Boy = ifelse(SEX==1,1,0),
         SEX_dummy_Girl = ifelse(SEX==2,1,0),
         STATUS_dummy_Married=ifelse(MARITAL==1,1,0),
         STATUS_dummy_Unmarried=ifelse(MARITAL==2,1,0),
         BWEIGHT_dummy_under=ifelse(BWEIGHT_COND==0,1,0),
         BWEIGHT_dummy_normal=ifelse(BWEIGHT_COND==1,1,0),
         BWEIGHT_dummy_over=ifelse(BWEIGHT_COND==2,1,0)
      ) %>% 

  mutate_at(vars(BWEIGHT_COND ),factor,levels = c(0,1,2),labels=c("Underweight","Normal","Overweight")) %>%
  mutate_at(vars(BWEIGHT_COND ),relevel,ref="Underweight")
  
 # mutate_at(qualitative_tf_col,factor,levels = c(0,1),labels=c("Yes","No")) %>%
 #  mutate_at(qualitative_tf_col,relevel,ref="Yes") %>%
  
 # mutate_at(vars(contains("dummy")),factor,levels = c(0,1),labels=c("Yes","No")) %>%
 #  mutate_at(vars(contains("dummy")),relevel,ref="Yes") %>%
  
 # mutate_at(vars(SEX),factor,levels = c(1,2),labels=c("Boy","Girl")) %>%
 # mutate_at(vars(SEX),relevel,ref="Boy") %>%
  
 # mutate_at(vars(MARITAL),factor,levels = c(1,2),labels=c("Married","Unmarried")) %>%
 # mutate_at(vars(MARITAL),relevel,ref="Married")



summary(birthweight_clean)
str(birthweight_clean)
all_vars = colnames(birthweight_clean)
tot_vars = length(all_vars)
dim(birthweight_clean)
sum(is.na(birthweight_clean))



#############################
# Exploratory Data Analysis #
#############################
#install.packages("gridExtra")
library(gridExtra)

grid.arrange(plot1, plot2,ncol=2, nrow = 1)

plot1=birthweight_clean %>% 
        ggplot(aes(x=BWEIGHT)) + 
        geom_histogram(fill="#9f2042")  +
        xlab("Birthweight (pounds)") +
        scale_x_continuous(breaks=seq(0,10,0.5))
        ylab("Frequency") +
        ggtitle("Histogram of Birthweight")

plot2=birthweight_clean %>% 
        ggplot(aes(x=BWEIGHT_COND)) + 
        geom_bar(fill=c("#211103","#9f2042","#1F1D1D")) +
        xlab("Classes") +
        ylab("Frequency") +
        ggtitle("Birthweight classes")

plot_density = function(cond,true_lab,false_lab,title){
  birthweight_clean %>% 
    mutate(Status=ifelse(cond,true_lab,false_lab)) %>%
    ggplot(aes(x=BWEIGHT,color=Status)) +
    geom_density(size=1) +
    ylab("Density") +
    xlab("Birthweight") +
    ggtitle(title) +
    scale_color_manual(values=c("#211103","#9f2042")) 
}

# Smoker vs Non-Smoker
plot3 = plot_density(birthweight_clean$CIGNUM>0,
                     "Smoker","Non-smoker",
                     "Status of smoking")

plot4 = plot_density(birthweight_clean$DRINKNUM>1,
                     "Drink","Not drinking",
                     "Status of taking/ not taking alcohol")  



# has/had previus preterm/small infant
plot5 = plot_density(birthweight_clean$PRETERM==1,
                     "Yes","No",
                     "Has/had previus preterm/small infant") 

# had/had previous infant 4000+ grams
plot6 = plot_density(birthweight_clean$PINFANT==1,
                     "Yes","No",
                     "Has/had previus infant with >5.5 pounds")  

# Mother has/had incompetent cervix
plot7 = plot_density(birthweight_clean$CERVIX==1,
                     "Yes","No",
                     "Has/had incompetent Cervix")  


# Mother has/had diabetes
plot8 = plot_density(birthweight_clean$DIABETES==1,
                     "Yes","No",
                     "Has/had diabetes")

# Mother has/had uterine bleeding
plot9 = plot_density(birthweight_clean$UTERINE==1,
                     "Yes","No",
                     "Has/had uterine bleeding")

# Mother has/had Oligohydramnios
plot10 = plot_density(birthweight_clean$HYDRAM==1,
                     "Yes","No",
                     "Has/had Oligohydramnios")

# Mother has/had chronic hypertension
plot11 = plot_density(birthweight_clean$HYPERCH==1,
                     "Yes","No",
                     "Has/had chronic hypertension")

# Mother has/had pregnancy hypertension
plot12 = plot_density(birthweight_clean$HYPERPR==1,
                      "Yes","No",
                      "Has/had pregnancy hypertension")

# Early delivery vs Normal delivery
plot13 = plot_density(birthweight_clean$WEEKS<37,
                     "Early delivery","Normal delivery",
                     "Early delivery vs Normal delivery")


# Early delivery vs Normal delivery
plot14 = plot_density(birthweight_clean$BDEAD>0,
                     "Yes","Never/No",
                     "Has/had children born alive now dead")


grid.arrange(plot3,plot4,plot5,plot6,plot7,plot8,ncol=2)
grid.arrange(plot9,plot10,plot11,plot12,plot13,plot14,ncol=2)


# Correlation

library(reshape2)
corr_matrix <- round(cor(birthweight_clean[,quantitative_col]),2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(corr_matrix){
  corr_matrix[lower.tri(corr_matrix)]<- NA
  return(corr_matrix)
}

upper_tri <- get_upper_tri(corr_matrix)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#211103", high = "#9f2042", mid = "white", 
                       midpoint = 0, limit =c(min(melt(corr_matrix)$value),max(melt(corr_matrix)$value)), space = "Lab", 
                       name="Pearson\nCorrelation") +

  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  xlab("")+ylab("")+ggtitle("Correlation Heat Map")+
  coord_fixed()


#############################
# Modelling: Classification #
#############################

#--------------#
# Model 1: kNN #
#--------------#

# Data preparation for kNN
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
  
birthweight_class= birthweight_clean %>%
  mutate_at(quantitative_col,normalize) %>%
  select(-contains("BWEIGHT_dummy"),-c("SEX","MARITAL","BWEIGHT"))

str(birthweight_class)


# Train/Test split for classification
# Use nrow to get the number of rows
(N <- nrow(birthweight_class))

# Calculate how many rows 75% of N should be and print it
(round(0.75*N))

# Create the vector of N uniform random variables: split
set.seed(123)
split <- runif(N)

# Use gp to create the training set: mpg_train (75% of data) and mpg_test (25% of data)
birthweight_class_train <- birthweight_class[split<0.75,]
birthweight_class_test <- birthweight_class[split>=0.75,]

# Use nrow() to examine mpg_train and mpg_test
nrow(birthweight_class_train)
nrow(birthweight_class_test)


# Train knn model
# Use kNN to identify the identify the class of weight
target_index = grep("BWEIGHT_COND", colnames(birthweight_class))
target_index

COND = birthweight_class_train$BWEIGHT_COND


# When k=279 (square root of total observations)
KNN_pred_279 <- knn(train = birthweight_class_train[-target_index], 
                test = birthweight_class_test[-target_index], 
                cl = COND,
                k=279)

KNN_pred_100 <- knn(train = birthweight_class_train[-target_index], 
                    test = birthweight_class_test[-target_index], 
                    cl = COND,
                    k=100)

KNN_pred_65 <- knn(train = birthweight_class_train[-target_index], 
                   test = birthweight_class_test[-target_index], 
                   cl = COND,
                   k=65)


# Accuracy
COND_actual <- birthweight_class_test$BWEIGHT_COND
k_279=mean(COND_actual==KNN_pred_279)
k_100=mean(COND_actual==KNN_pred_100)
k_65=mean(COND_actual==KNN_pred_65)


# Create a confusion matrix of the predicted versus actual values
confusionMatrix(COND_actual,KNN_pred_279)
confusionMatrix(COND_actual,KNN_pred_100)
confusionMatrix(COND_actual,KNN_pred_65)


k_result = rbind(k_65,k_100,k_279)
k_result


#--------------------------------------------#
# Model 2: Logistic Regression (Multinomial) #
#--------------------------------------------#

# Fit the model
fmla=as.formula(BWEIGHT_COND ~ .)

log_reg_model = nnet::multinom(fmla, data = birthweight_class_train)
  
# Summarize the model
summary(log_reg_model)

# Make predictions
predict_log_reg <- log_reg_model  %>% predict(birthweight_class_test[-target_index])

# Model accuracy
COND_actual <- birthweight_class_test$BWEIGHT_COND
logreg = mean(COND_actual==predict_log_reg)
confusionMatrix(COND_actual,predict_log_reg)




