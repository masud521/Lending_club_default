---
title: "default rate on loans"
Author: "Masood Khan"
output: html_notebook
---

```{r}
library(readr) # data input
library(readxl)
library(tidyverse) # data wrangling
library(dplyr) # data manipulation
library(stringr) # string manipulation
library(ggplot2) # visualization
library(ggthemes) # visualization
library(lubridate) # date and time
library(purrr) # data manipulation

```

Introduction- This is a Exploratory Data Analysis of Loan Data set. The dataset contains information about loan issued through 2015-2018. The data also required cleaning and data preparation processes such has removing irrelevant and redundant variables, columns having lots of NA values.

A business problem has been identified and they are concerned about the default rate on their loans. They want to understand who is likely to default and who they should lend to in the future. Hence, our target variable is Loan Status.

Load the data available for analysis. The dataset is about loans peer to peer lending and the profile of customers.

```{r}

loan <- read_csv("loan.csv", na = "")


```

```{r}
loan_dict <- read_xlsx("LCDataDictionary.xlsx")

```


```{r}
head(loan)

```


Summary and glimpse of data

```{r}
glimpse(loan)

```


Observation:

There are 2260668  observations
145 variables with many columns having lots of NA values
Target variable will be Loan status as it has information about loan paid, current and charged off(default) and other status


check for any NAs 

```{r}
loan %>%
  summarise_all(funs(sum(is.na(.))))
  

```

Percentage of missing values columns

```{r}

loan_missing_values <-  loan %>%
                        summarise_all(funs(sum(is.na(.))/n())) %>%
                        gather(key = "variables", value = "NAs_percentage")

```

I am visualising the percentage of missing values in each column so we can have a better understanding of missing values



```{r}
loan_missing_values %>%
  arrange(desc(NAs_percentage)) %>%
  slice(1:50) %>%
  ggplot() +
  geom_col(aes( x = variables, y = NAs_percentage), fill = "red", bins = 30)+
  coord_flip()

```


```{r}

ggsave("NAplot1.png", height = 30, width = 30, units = "in")

```

Observation:

There are many variables have more tha 25% NA values and to get meaninful information from data I am keeping columns which has missing values 15% or less
 
```{r}

loan_cleaned <- loan[, which(colMeans(is.na(loan)) > 0.15)]

```
 

I have removed redundant variables which are not relevant to default loans and those varaibles which are not deciding factors regarding loan approval 

1. id, member_id, url, desc,  column can be removed as they have unique values for the purpose of loan identification only.

2.total_pymnt, total_pymnt_inv, total_rec_prncp etc. are basically customer payment behaiviour parameters

Data selction

The dataset contains of information of age, annual income, grade of employee, home ownership that affect the probability of default of the borrower. The columns I have used are namely:


verification_status : Indicates if income was verified by LC, not verified, or if the income source was verified
loan_status : Variable with multiple levels (Charged off, Current, Default, Fully Paid …)
loan_amnt : Total amount of loan taken
int_rate : Loan interset rate
grade : Grade of employment
purpose : A category provided by the borrower for the loan request.
emp_length : Duration of employment
home_ownership : Type of ownership of house
annual_inc : Total annual income
term : 36-month or 60-month period

```{r}

loan_cleaned <- loan %>%
                select(verification_status, loan_status, loan_amnt, int_rate, grade, purpose, emp_length, home_ownership, annual_inc, term)

head(loan_cleaned)
  
  

```

Data corrections:

Handling Categorical variables-There are some Columns that are categorical variables but are represented as characters. We need to convert them into factors

```{r}

loan_cleaned$verification_status <- as.factor(loan_cleaned$verification_status)
loan_cleaned$loan_status <- as.factor(loan_cleaned$loan_status)
loan_cleaned$grade <- as.factor(loan_cleaned$grade)
loan_cleaned$emp_length <- as.factor(loan_cleaned$emp_length)
loan_cleaned$home_ownership <- as.factor(loan_cleaned$home_ownership)
loan_cleaned$term <- as.factor(loan_cleaned$term)


```


```{r}

head(loan_cleaned)

```

Exploratory Data Analysis

Now we will further analyse with selected data

```{r}

loan_status_perc <-
loan_cleaned %>% group_by(loan_status) %>% 
summarise(count = n()) %>%
mutate(percentage = count/sum(count))

loan_status_perc

```



```{r}

loan_status_perc %>%
ggplot(aes(x = reorder(loan_status, percentage), y = percentage, colour = loan_status, fill = loan_status)) +
geom_col() + 
coord_flip()+ 
theme(legend.position = "none") +
labs(x = "Loan_Status", y = "Percent %")

```


```{r}

ggsave("loan_status_per.png", height = 10, width = 10, units = "in")

```




2nd graph with total loan counts


```{r}
loan_cleaned %>%
       count(loan_status) %>%
        ggplot() +
        aes(x = reorder(loan_status, n), y = n, fill = loan_status, colour = loan_status) + 
        geom_col() + 
        coord_flip() + 
        theme(legend.position = "none") +
        labs(x = "Loan Status" , y = "Count")

```



```{r}
ggsave("loan_status_count.png", height = 10, width = 10, units = "in")

```



The varibale Loan_status has 9 categories

To answer business question about default rates I have only considered the data from Fully paid and Charged off (default) category. 

We want to convert this variable to binary (1 for default and 0 for non-default) but we have 10 different levels. Loans with status Current, Late payments, In grace period need to be removed. Therefore, we create a new variable called loan_outcome where

loan_outcome -> 1 if loan_status = ‘Charged Off’ or ‘Default’ loan_outcome -> 0 if loan_status = ‘Fully Paid’


```{r}
loan_status <-
  
loan_cleaned %>%
  filter(loan_status == "Fully Paid" | loan_status == "Charged Off") %>%
  mutate(binary_loan_status = as.numeric(ifelse(loan_status == "Charged Off", 1, 0)))

tail(loan_status)

```

```{r}
sapply(loan_status , function(x) sum(is.na(x)))

loan_status <- loan_status %>%
        filter(!is.na(annual_inc) , 
               !(home_ownership %in% c('NONE' , 'ANY')) , 
               emp_length != 'n/a')

```




Univariate analysis on Categorical variables

For credit risk modelling It is known that the better the grade the lowest the interest rate.

LendingClub also assigned a grade (A–G) to each note that reflects LendingClub’s assessment of the credit risk of the corresponding loan.Let visualise the grade and corresponding interest rate with boxplot  


```{r}
loan_status %>%
  ggplot(aes(x = grade, y = int_rate, fill = grade)) +
  geom_boxplot() +
  theme_igray() +
  theme(legend.position = "none")+
  labs( x = "Grade", y = "Interest rate")
  
```

```{r}

ggsave("grade_int_rate.png", height = 10, width = 10, units = "in")

```

Percentage of the loan assigned according to grade

Grade B and C are assigned to more than 50% of the loans.

```{r}

loan_status %>%
  group_by(grade) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  ggplot() +
  aes(x = reorder(grade, -percentage), y = percentage, fill = grade, colour = grade) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none") +
  labs(x = "Grade", y = "Percent %")
  

```

```{r}
ggsave("grade_perc.png", height = 10, width = 10, units = "in")

```

TERM: 
This is the number of payments on the loan. There are two terms (36 months and 60 months) on which loans are given.
Around 75% of the loans are given on 36 months term


```{r}

loan_status %>%
  group_by(term) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  ggplot() +
  aes(x = term, y = percentage, colour = term, fill = term) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Term", y = "Percentage")
  

```

```{r}

ggsave("loan_term.png", height = 10, width = 10, units = "in")

```


Home Ownerships: 
Rent and mortgage home owners account for around 90% of the loans


```{r}

loan_status %>%
  group_by(home_ownership) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  ggplot() +
  aes(x = reorder(home_ownership, -percentage), y = percentage, fill = home_ownership, colour = home_ownership) +
  geom_bar(stat = "identity") +
  labs(x = "Home Ownership", y = "Percentage")

```

```{r}
ggsave("home_ownership.png", height = 10, width = 10, units = "in")

```





Verifications: more than 65% of the loans are varified before lending 

```{r}
loan_status %>%
  group_by(verification_status) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  ggplot() +
  aes(x = reorder(verification_status, -percentage), y = percentage, fill = verification_status, colour =      verification_status) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Verification Status", y = "Percentage")


```

```{r}

ggsave("varification_status.png", height = 10, width = 10, units = "in")

```

Purpose: Debt consolidation accounts for 60% of the loans borrowed 

```{r}
loan_status %>%
  group_by(purpose) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  ggplot() +
  aes(x = reorder(purpose, percentage), y = percentage, fill = purpose, colour = purpose) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Purpose of the Loan", y = "Percentage")

```

```{r}

ggsave("loan_purpose.png", height = 10, width = 10, units = "in")

```



Now analyse how these single variable performs in different segments. Such as how does loan amount varies when it is segmented with respect to home ownership or verification status.

The target variable is Loan status, so we will inspect how different continous and discreet variables perform when segmented into loan status.

We assume that grade is a great predictor for the volume of default loans.

Default with respect to Grade:
Default increases with increase in Grade from A-G, which clearly indicates that a loan with grade A means lowest risk of loan default and G means higher risk of loan default.


```{r}

table(loan_status$grade , factor(loan_status$binary_loan_status , c(0 , 1) , c('Fully Paid' , 'Default'))) 

```
 lets visualise this on graph
 
```{r}

loan_status %>%
  ggplot()+
  aes(x = grade, fill = factor(binary_loan_status, c(0 , 1) , c("Fully Paid", "Default"))) +
  theme(legend.title = element_blank()) +
  geom_bar(stat = "count", position = "fill") +
  labs(x = "Grade", y = "Fully Paid Vs Default")


```
 
```{r}
ggsave("default_rate.png", height = 10, width = 10, units = "in")

```
 
Default with respect to Term: 
The percentage of default in case of loans with 60 months term is higher as compared to the loans with 36 months term.


```{r}
loan_status %>%
  ggplot() +
  aes(x = term, fill = factor(binary_loan_status, c(0 , 1) , c("Fully Paid", "Default"))) +
  geom_bar(stat = "count", position = "fill") +
  theme(legend.title = element_blank()) +
  labs(x = "Term", y = "Fully paid Vs Default")


```

```{r}
ggsave("default_term.png", height = 10, width = 10, units = "in")

```



Default with respect to Home Ownership :

The default rate in Own, rent and mortgage home status is almost same


```{r}

loan_status %>%
  ggplot() +
  aes(x = home_ownership, fill = factor(binary_loan_status, c(0 , 1) , c("Fully Paid", "Default"))) +
  geom_bar(stat = "count", position = "fill") +
  theme(legend.title = element_blank()) +
  labs(x = "Home Ownership", y = "Fully paid Vs Default")

```




Verification status with respect to default:

The default rate in verified category is slightly more than non verified categories


```{r}

loan_status %>%
  ggplot() +
  aes(x = verification_status, fill = factor(binary_loan_status, c(0 , 1) , c("Fully Paid", "Default"))) +
  geom_bar(stat = "count", position = "fill") +
  theme(legend.title = element_blank()) +
  labs(x = "Verification Status", y = "Fully paid Vs Default")

```

```{r}
ggsave("default_varification.png", height = 10, width = 10, units = "in")

```



Purpose of Loan with respect to Default: 
The default rate in small business category is highest as compared to other categories.

```{r}

loan_status %>%
  group_by(purpose) %>%
  summarise(default_perc = sum(binary_loan_status)/n())%>%
  ggplot() +
  aes(x = reorder(purpose, default_perc), y = default_perc, fill = purpose, colour = purpose) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip()+
  labs(x = "Purpose of the loan", y = "Default Percent")


```


```{r}

ggsave("default_purpose.png", height = 10, width = 10, units = "in")

```


```{r}

loan_status2 = loan_status %>%
        select(-loan_status,) %>%
        filter(binary_loan_status %in% c(0 , 1))

tail(loan_status2)

```





Now let’s try to find out what impact the annual income of the borrower has on the other variables

```{r}
ggplot(loan_status2[sample(244179 , 10000) , ] , aes(x = annual_inc , y = loan_amnt , color = int_rate)) +
        geom_point() + 
        geom_smooth(se = F , color = 'darkred' , method = 'loess')+
        xlim(c(0 , 300000)) + 
        labs(x = 'Annual Income' , y = 'Loan Ammount' , color = 'Interest Rate')
```




Data modelling
Modelling Process:

We created the binary_loan_status which will be our response variable.
We exclude some independent variables in order to make the model simpler.
We split the dataset to training set(75%) and testing set(25%) for the validation.
We train a model to predict the probability of default.

Because of the binary response variable we can use logistic regression. Rather than modelling the response Y directly, logistic regression models the probability that Y belongs to a particular category, in our case the probability of a non-performing loan. This probability can be computed by the logistic function,





```{r}
options(scipen = 999)
# Split dataset 

idx <- sample(dim(loan_status2)[1] , 0.75*dim(loan_status2)[1] , replace = F)
train <- loan_status2[idx , ]
test  <- loan_status2[-idx , ]

# Fit logistic regression

glm_model_default <- glm(binary_loan_status ~ ., train, family = binomial(link = 'logit'))

summary(glm_model_default)

```

The coefficients of the following features are positive:

Loan Amount
Interest Rate
Home Ownership - Other
Term
The better the grade are less likely to be default
This means the probability of defaulting on the given credit varies directly with these factors. For example more the given amount of the loan, more the risk of losing credit.

The coefficients of the following features are negative:

Annual Income
Home Ownership - Own
Home Ownership - Rent
Borrowers with 10+ years of experience are more likely to pay their debt
There is no significant difference in the early years of employment
This means that the probability of defaulting is inversely proportional to the factors mentioned above.


```{r}

pred_default <- predict(glm_model_default, train, type = "response")

head(pred_default)

 
```



```{r}
# Density of probabilities

ggplot(data.frame(pred_default) , aes(pred_default)) + 
        geom_density(fill = 'light green' , alpha = 0.4) +
        labs(x = 'Predicted Probabilities on train set')

```

```{r}

ggsave("prob_train.png", height = 10, width = 10, units = "in")

```


A threshold of 25% - 30% seems ideal cause further increase of the cut off percentage does not have significant impact on the accuracy of the model. The Confusion Matrix for cut off point at 30% will be this

```{r}

pred_30 = ifelse(pred_default > 0.3 , 1 , 0)

confusion_matrix_30 <- table(Predicted = pred_30 , Actual = train$binary_loan_status)

confusion_matrix_30

```

```{r}

sum(diag(confusion_matrix_30))/sum(confusion_matrix_30)

1 - sum(diag(confusion_matrix_30))/sum(confusion_matrix_30)


```



```{r}
pred_default_test <- predict(glm_model_default, test, type = "response")

head(pred_default_test)

```




```{r}

pred2 <- ifelse(pred_default_test > 0.3 , 1 , 0)

confusion_matrix2 <- table(Predicted = pred2 , Actual = test$binary_loan_status)

confusion_matrix2

```

```{r}

sum(diag(confusion_matrix2))/sum(confusion_matrix2)

1 - sum(diag(confusion_matrix2))/sum(confusion_matrix2)

```

```{r}
# goodness of fit test

with(glm_model_default, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F))

```

This model is statistically significant as P-value is zero.


```{r}
library(pROC)

# Area Under Curve

auc(roc(train$binary_loan_status, pred_default))


```

```{r}
# Plot ROC curve

plot.roc(train$binary_loan_status, pred_default, main = "Confidence interval of a threshold" , percent = TRUE , 
         ci = TRUE , of = "thresholds" , thresholds = "best" , print.thres = "best" , col = 'blue')


```

```{r}

ggsave("CI_level.png", height = 10, width = 10, units = "in")

```

























































 
 


























