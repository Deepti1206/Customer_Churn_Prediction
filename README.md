# Customer Churn Prediction in Telecom Industry

<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/churn.png" width="600" height="200">

## Table of Contents
- [Project Objective](#project-objective)
- [Business Understanding](#business-understanding)
- [Dataset Overview](#dataset-overview)
- [Data Analysis](#data-analysis)
- [Machine Learning](#machine-learning)
- [Summary of Results](#summary-of-results)
- [Highlights and key findings](#highlights-and-key-findings)
- [Classification model and analysis](#classification-model-and-analysis)
- [Recommendations to the company](#recommendations-to-the-company)

## Project Objective
The objective of the project is to predict the customers who are discontinuing the services. This projects aims to predict these churn customers based on different factors given in the data.

## Business Understanding
Customer churn prediction is a vital component of customer relationship management and business strategy. It involves using data analysis and predictive modeling techniques to identify customers who are likely to discontinue their business relationship with a company. By analyzing historical data, customer behavior, and various attributes, businesses can anticipate and address the factors contributing to churn. This proactive approach allows companies to implement retention strategies, improve customer satisfaction, and ultimately reduce customer attrition, leading to increased revenue and business sustainability. Customer churn prediction helps organizations make data-driven decisions, enhance customer engagement, and create targeted interventions to retain their valuable customer base.

## Dataset Overview
This dataset contains different factors related to churn rate. The Churn column is given as 'Yes' or 'No' meaning it is a categorical column.

```{r warning = FALSE, message = FALSE, include=TRUE}
# Suppress dplyr summarise grouping warning messages
options(dplyr.summarise.inform = FALSE)

## Add R libraries
library(tidyverse)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(discrim)
library(klaR)
library(tune)
library(kknn)
library(vip)
library(MASS)


# Load the dataset
telecom_df <- readRDS(url('https://gmubusinessanalytics.netlify.app/data/telecom_df.rds'))

```
```{r vp}
skimr::skim(telecom_df)

```

```{r rr, include=FALSE}

colnames(telecom_df)

telecom_df %>% dplyr::group_by(canceled_service) %>% summarise(count = n())

# random <- randomForest::randomForest(canceled_service ~., data= telecom_df)
# print(random)
# 
# library(randomForest)
# importance(random)
# varImpPlot(random)
```


# Data Analysis


```{r , include = FALSE}
cancel_percent_df <- telecom_df %>% 
  group_by(canceled_service) %>%
  summarise( total = n())

cancel_percent_df$percent_cancel <- round(cancel_percent_df$total/sum(cancel_percent_df$total)*100,1)

# Creating a bar graph of distribution of canceled service

# cancel_bar <- ggplot( cancel_percent_df, aes( x = canceled_service, 
#                                         y = percent_cancel)) +
#   geom_bar(stat = "identity", 
#            fill = "cadetblue") +
#   geom_text(aes(label = percent_cancel), vjust = 1.5, 
#             colour = "black") +
#   labs(title = "Percentage of customer canceled service") +
#   xlab("Customer subscription status") +
#   ylab("Percentage") +
#   theme( plot.title = element_text(size = 10),
#          plot.subtitle = element_text(size = 8))
```
There are 427 customers who has cancelled the services and which contributes to 36.3% to the population

```{r ouo}
cancel_percent_df

```

# Question 1

**Question**:
Is there any relation between canceled service, months_with_company and monthly_charges?

**Answer**:
The customers who have cancelled the service show a high variation w.r.t the months with the company. However, the monthly charges doesn't seem to affect much to the cancellation of the service

```{r ww, include=FALSE}
telecom_df %>% group_by(canceled_service)%>%                 
                            summarise(months_with_company = n(),
                            avg_months = mean(months_with_company),
                            sd_months = sd(months_with_company))

```

```{r ee}
plot_comp <- ggplot(data = telecom_df, aes(x= canceled_service , y = months_with_company)) +
  geom_jitter(color = "cadetblue") +
  geom_boxplot(width = 0.3, color = "azure4")

plot_charge <- ggplot(data = telecom_df, aes(x = canceled_service, y =  monthly_charges)) +
  geom_jitter(color = "cadetblue") +
  geom_boxplot(color = "azure4", width = 0.3)

ggarrange(plot_comp,plot_charge, nrow=1 )
```
  <img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/1.png" width="600" height="400">

# Question 2

**Question**:
Do cancelled service has any relation with avg_call_mins and avg_intl_mins?

**Answer**:
It can be said that the customers who are making more international calls seems to continue with the subscription as compared to the customers who do less international calls.
On the other hand, the customers who make more calls on a average tends to cancel the service. This means that majority of them can be local calls cancelling the service

```{r ol}
plot_call <- ggplot(telecom_df, aes(y = canceled_service,
                    x = avg_call_mins, color = canceled_service)) + geom_boxplot()

plot_int <- ggplot(telecom_df, aes(y = canceled_service,
                    x = avg_intl_mins, color = canceled_service)) + geom_boxplot()

ggarrange(plot_call, plot_int, nrow = 2, ncol = 1)

```


```{r fzg}
calling_data <- telecom_df %>% dplyr::select(canceled_service,
                                             avg_call_mins, avg_intl_mins)

calling_data$local <- calling_data$avg_call_mins - calling_data$avg_intl_mins

perc_local <- calling_data %>% 
  group_by(canceled_service) %>%
  summarise(avg = mean(local)) %>%
  mutate(perct = round(avg/sum(avg)*100,2))

# call <- telecom_df %>% 
#   group_by(canceled_service) %>%
#   summarise(avg = mean(avg_call_mins)) %>%
#   mutate(perct = round(avg/sum(avg)*100,2))

intl <- telecom_df %>% 
  group_by(canceled_service) %>%
  summarise(avg = mean(avg_intl_mins)) %>%
  mutate(perct = round(avg/sum(avg)*100,2))

local_plot <- ggplot(perc_local, aes(x = canceled_service, y= perct)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  geom_text(aes(label=perct), vjust=1.6, color="white", size=3.5) +
  ylab("Avg local mins percentage")

intl_plot <- ggplot(intl, aes(x = canceled_service, y= perct)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  geom_text(aes(label=perct), vjust=1.6, color="white", size=3.5) +
  ylab("Avg intl call mins percentage")

ggarrange(local_plot,intl_plot, nrow= 1 )

```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/2.png" width="600" height="400">

# Question 3

**Question**:
Is there any relation between the canceled service, average call by the customers (mins) and the contract?

**Answer**:
It can be said that the customers who subscribed on monthly basis contract tend to cancel the service. There is no such pattern observed in the customers who have more than a year contract. Also, there is no pattern observed among the customers contract with the company and the average call (mins) made by the customer.

```{r tt}
contract_box <- ggplot(telecom_df, aes(x= contract, fill= canceled_service)) +
  geom_boxplot(aes(y = months_with_company)) +
  labs(title = "Contract v/s customer months with company") +
  theme(legend.position = "bottom", 
        plot.title = element_text(size=8))

call_box <- ggplot(telecom_df, aes(x= contract, 
                                   fill= canceled_service)) +
  geom_boxplot(aes(y = avg_call_mins)) +
  labs(title = "Contract v/s Avg call mins ") +
  theme(legend.position = "bottom",
        plot.title = element_text(size=8))

ggarrange(contract_box, call_box, nrow=1)
```
```{r vl}
contract_perc <- telecom_df %>% 
  group_by(contract, canceled_service) %>%
  summarise(cnt = n()) %>%
  mutate(perct = round(cnt/sum(cnt)*100,2))

ggplot(contract_perc, aes(x = contract, y= perct, 
                          fill= canceled_service)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label=perct), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5) +
  ylab("Percentage of customers")
```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/3.png" width="600" height="400">

# Question 4

**Question**:
Is the canceled service related to the cellular service and internet service?

**Answer**:
From the bar graph, we can clearly see that the customer subscription cancellation is related to the internet service and the cellular service. It can be seen  that the customer who is using digital service and has a single cell line tend to cancel the service than the one who uses fiber optic and multiple cellular line.

```{r gi}

cell_box <- ggplot(telecom_df, aes(x= cellular_service, 
                                   y = months_with_company,
                                   fill= canceled_service)) +
  geom_boxplot() +
  labs(title = "Cellular service v/s customer months with company") +
  theme(legend.position = "bottom")

internet_box <- ggplot(telecom_df, aes(x= internet_service,
                                       y = months_with_company,
                                       fill= canceled_service)) +
  geom_boxplot() +
  labs(title = "Internet v/s Avg call per customer mins") +
  theme(legend.position = "bottom")

ggarrange(cell_box, internet_box, nrow=1)

```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/5.png" width="600" height="400">

# Question 5


**Question**:
Is the the canceled service is related to the payment method and monthly charges?

**Answer**:
There is a moderate relation between cancel_service, monthly-charge and payment method. The cancellation rate of the customer who prefers to pay by electronic check is around 50%.

```{r ii}

charges_per <- telecom_df %>%
  group_by(payment_method, canceled_service) %>%
  summarise(count = n()) %>%
  mutate(per= prop.table(count) * 100)

ggplot(telecom_df, aes( x= canceled_service, 
                        y = monthly_charges, fill = canceled_service) ) +
  geom_violin() +
  geom_boxplot(fill = NA, width = 0.1) +
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~ payment_method, nrow = 2)

ggplot(charges_per, aes(x = canceled_service, y= per, fill = canceled_service)) +
   geom_bar(stat = "identity") +
  geom_text(aes(label = round(per,digits =2)), vjust=-0.3, color="black", size=3.5) +
   facet_wrap(~ payment_method, nrow = 2)
```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/4.png" width="600" height="400">

# Machine Learning

# Model 1

## Logistic Regression

```{r vv}
## Data Splitting

set.seed(271)

telecom_split <- initial_split(telecom_df, prop = 0.75,
                               strata = canceled_service)

telecom_training <- telecom_split %>%
  training()

telecom_test <- telecom_split %>%
  testing()

# Cross validation folds for hyperparamter tuning  

set.seed(271)

telecom_folds <- vfold_cv(telecom_training, v = 5)

## Feature Engineering

telecom_recipe <- recipe(canceled_service ~ .,
                         data = telecom_training) %>% 
                 step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
                 step_normalize(all_numeric(), -all_outcomes()) %>% 
                 step_dummy(all_nominal(), -all_outcomes())

## Checking transformations

telecom_recipe %>% 
  prep(training = telecom_training) %>% 
  bake(new_data = NULL)

## Specifying logistic regression model

logistic_model <- logistic_reg() %>% 
                  set_engine('glm') %>% 
                  set_mode('classification')

## Creating a workflow

logistic_wf <- workflow() %>% 
               add_model(logistic_model) %>% 
               add_recipe(telecom_recipe)

## Fitting a model
logistic_fit <- logistic_wf %>% 
                last_fit(split = telecom_split)

## Collecting predictions
logistic_results <-  logistic_fit %>% 
                     collect_predictions()

## Evaluating model performance
# ROC curve
roc_curve(logistic_results, 
          truth = canceled_service, 
          estimate = .pred_yes) %>% 
  autoplot()

# ROC AUC
roc_auc(logistic_results, 
        truth = canceled_service,
        .pred_yes)

# Confusion matrix
conf_mat(logistic_results, 
         truth = canceled_service, 
         estimate = .pred_class)


```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/1.1.png" width="600" height="400">

# Model 2
## Linear Discriminant Analysis

```{r pp}
# Specifying LDA model

lda_model <- discrim_regularized(frac_common_cov = 1) %>% 
             set_engine('klaR') %>% 
             set_mode('classification')

# Creating LDA workflow

lda_wf <- workflow() %>% 
          add_model(lda_model) %>% 
          add_recipe(telecom_recipe)

# Fitting a model
lda_fit <- lda_wf %>%
  last_fit(split = telecom_split)

# Collect predictions
lda_fit %>% collect_metrics()


lda_results <-  lda_fit %>% 
                collect_predictions()


# Evaluating model performance

# ROC curve
roc_curve(lda_results, 
          truth = canceled_service, 
          estimate = .pred_yes) %>% 
  autoplot()

# ROC AUC
roc_auc(lda_results, 
          truth = canceled_service, 
          estimate = .pred_yes)

# Confusion matrix
conf_mat(lda_results, 
         truth = canceled_service, 
         estimate = .pred_class)

f_meas(lda_results,
       truth = canceled_service, 
         estimate = .pred_class)

```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/1.2.png" width="600" height="400">


# Model 3
## KNN Classification

```{r aa}
# Specifying KNN model

knn_model <- nearest_neighbor(neighbors = tune()) %>% 
             set_engine('kknn') %>% 
             set_mode('classification')

# Creating KNN workflow

knn_wf <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(telecom_recipe)

# Tuning hyper parameter
## Create tuning grid

k_grid <- tibble(neighbors = c(10, 15, 25, 45, 60, 80, 100, 120, 140, 180))

## Grid search

set.seed(314)

knn_tuning <- knn_wf %>% 
              tune_grid(resamples = telecom_folds,
                        grid = k_grid)

## Selecting best model

best_k <- knn_tuning %>% 
          select_best(metric = 'roc_auc')

## Finalizing workflow

final_knn_wf <- knn_wf %>% 
                finalize_workflow(best_k)

# Fit Model

knn_fit <- final_knn_wf %>% 
           last_fit(split = telecom_split)

# Collecting predictions

knn_results <-  knn_fit %>% 
                collect_predictions()

# Evaluating model performance

# ROC curve
roc_curve(knn_results, 
          truth = canceled_service, 
          estimate = .pred_yes) %>% 
  autoplot()

# ROC AUC
roc_auc(knn_results, 
          truth = canceled_service, 
          estimate = .pred_yes)

# Confusion matrix
conf_mat(knn_results, 
         truth = canceled_service, 
         estimate = .pred_class)

## Model performance grade = B

```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/1.3.png" width="600" height="400">


# Model 4
## Random Forest

```{r jj}
# Model specification

rf_model <- rand_forest(mtry = tune(),
                        trees = tune(),
                        min_n = tune()) %>% 
            set_engine('ranger', importance = "impurity") %>% 
            set_mode('classification')

rf_workflow <- workflow() %>% 
               add_model(rf_model) %>% 
               add_recipe(telecom_recipe)

# Tuning hyper parameters

set.seed(314)

rf_grid <- grid_random(mtry() %>% range_set(c(2, 4)),
                       trees(),
                       min_n(),
                       size = 10)

rf_grid

rf_tuning <- rf_workflow %>% 
             tune_grid(resamples = telecom_folds,
                       grid = rf_grid)

# show 5 best models

rf_tuning %>% show_best('roc_auc')

best_rf <- rf_tuning %>% 
           select_best(metric = 'roc_auc')

best_rf

# Finalize workflow

final_rf_workflow <- rf_workflow %>% 
                     finalize_workflow(best_rf)

# Fit model

rf_wf_fit <- final_rf_workflow %>% 
             fit(data = telecom_training)

rf_fit <- rf_wf_fit %>% 
          pull_workflow_fit()

# Variable improtance
vip(rf_fit)

# Train & evaluate last fit

rf_last_fit <- final_rf_workflow %>% 
               last_fit(telecom_split)

rf_last_fit %>% collect_metrics()

# ROC curve

rf_last_fit %>% collect_predictions() %>% 
                roc_curve(truth  =canceled_service, 
                          estimate = .pred_yes) %>% 
                autoplot()
# Confusion matrix

rf_predictions <- rf_last_fit %>% 
  collect_predictions()

conf_mat(rf_predictions, truth = canceled_service,
         estimate = .pred_class)
```
<img src="https://github.com/Deepti1206/Predicting_Customer_Cancellation_Telecom_Services/blob/main/Images/1.4.png" width="600" height="400">

# Summary of Results

## Summary

The telecom company continues to lose customers, and the rate of service cancellation is rising. The telecom provider is attempting to determine the main factors that contributed to their customer canceling the telecom service. 

The goal of this project is to suggest a strategy to company by identifying important factors and giving them a plan retain customers. In this project, the data of about 1000 customers is analyzed through machine learning methods to identify the main causes. The objective is to assist the business in lowering the likelihood that the client will terminate the service.

## Highlights and key findings

The highlights and the key findings of the analysis are as follows:

- The **Customer subscribed for less month** tend to cancel the service than the customers who are subscribed for a longer time

- The cancellation rate of the customers who are doing local calls are 11% higher than that of the customers doing internal calls. It is more likely to say that the local customers tend to cancel the service

- The customers who had **month-to-month contract** cancelled the service than the customers who had contracts for more than a year

- The customer who preferred to use internet service as **digital** tends to cancel the service than the customers who opted for fiber optics

- It is observed the moderate relation between the service status, paymnet method and the monthly charges. The cancellation rate of the customer who prefers to pay by **electronic check** is around 50%.


## Classification model and analysis

There were four types of classification models used for the analysis of the data. Each model is tested by their performance metric **ROC_AUC**. ROC_AUC is used to evaluate the model performance.

First, the **Logistic regression** were performed which gave an ROC_AUC of **89.96%**. Second, the **Linear Discriminant analysis** was applied which gave an ROC_AUC of **90.23% **. Third, the **K- Nearest Neighbor** was used which gave an ROC_AUC of **86.31%** and lastly the **Random Forest** was applied which gave an ROC_AUC of **88.75%**. As it is observed that, the ROC_AUC of Linear Discriminant is highest among all the four and hence for this analysis the **Linear Discriminant Analysis** is the best machine learning model to predict the likelihood of the customer service with the company.

The important features identified through this analysis are the customer's months with the company, customer average calling minutes, customer average international calling minutes and the monthly charges paid by the customers. Focusing on this features will help company to retain the customers.

## Recommendations to the company
  
As per the findings and the analysis, the following recommendations can benefit the company in order to retain the customers:

- It is observed that the customer who are subscribed to the service for a longer time continues to remain with the service. Company should come out with the customer engagement plans and focus on the customer who have contract on monthly basis. Moving monthly contract customers to yearly plans will decrease the service cancellation rate by around 36%

- Company should also focus on re-evaluating plans on the local calling. As it is seen that local customers cancellation rate is 11% higher than international calls. Retaining the local customers will decrease the service cancellation of the local customers.

- The company should come out with the plan for internet services. They can try to shift the customers to use fiber optics or re-evaluate on the digital services scheme to attract customers, this totally depends on company's decision

- As observed that the almost 50% of the customers who prefers electronic payment cancels the service. They might or might not continue with the services. Company should investigate into these customers and try to solve their problem if there's any technical issues.
