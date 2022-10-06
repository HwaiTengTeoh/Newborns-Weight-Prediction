# Newborns Weight Prediction and Classification
*Note: This repository shares the pairwork project in fulfillment of the Programming class for Master of Data Science coursework.*

## Work Summary
Birth weight is the weight of baby taken just after he or she is born, and a baby birth weight is a strong indicator of maternal and new-born health and nutrition. According to World Health Organization (WHO), low birth weight is defined as the birthweight less than 2,500 grams or 5.5 pounds regardless of gestational age. The normal weight range for new-born baby should exceed 2,500 grams (5.5 pounds) but less than 4,000 grams (8.8 pounds). Low birth weight is an outcome that has been of concern because infant mortality rates and birth defect rates are very high for low birth weight babies. Low birth weight can occur in premature babies who are delivered at a gestational age less than 37 weeks of pregnancy (the normal length of a pregnancy is 40 weeks) or in babies who are born at the regular time but are under weight. Baby with greater than the normal weight range is considered large and would increase risk and difficulties during delivery. It would be so important for the doctor to be able to predict the birth weight instead of solely relying on ultrasound results, so they can take different measures in advance and minimize the risk during delivery. Though ultrasound can help for such prediction, but data scientist could also detect this in advance with given data.

## Objectives
The main goals of this project are intuitive. We are dealing with different tasks by taking birthweight and birthweight class as the target variables. In the first task, we aim to predict the baby's birth weight given information about the mother's historical health, habits, gestational period, and age. In the second task, we aim to classify whether a baby is underweight, overweight, or has normal weight using the same features. Thus, regression and classification models are built in this work. 

## Data Sources
- The Dataset used is shared at the Kaggle site: https://www.kaggle.com/competitions/csci-ml-s19-pa1/data
- The data was collected in 2008 with 133422 rows with 125 columns. 
- For **regression task**, the target variables for our study is the birth weight of infant, **BWEIGHT** which can be represented from columns namely **BPOUND** and **BOUNCE** which denote the birthweight in pounds and ounce separately.
- For **classification task**, since the target variable is recorded in measurable metrics, we assigned the birthweight categories according to the defined categories by WHO to create a new column **BWEIGHT_COND** to record the groupings of baby birth weight. 
- Babies with **low birth weight** are those with less than 5.5 pounds. Large babies with a birthweight exceeding 8.8 pounds are categorized as **overweight**. Besides that, the baby is in the **normal birth weight** range.

## RPubs 
The code scripts are translated as R Markdown documents in RStudio and shared on RPubs:\
https://rpubs.com/s2003493/Birthweight-Analysis?fbclid=IwAR3yUR0axet3pAG5IQtNkzFXmZxbV8oicCm-BDOKw70XxdaTGfhJEmUTNNU
