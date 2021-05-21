########### CUTS AND EXPERIMENTS #################


#*Designate Features and Classes*
  ```{r}
#identify features in iphone_df
iphone_df_features <- iphone_df_train[, 1:11]

```





## Recursive Feature Selection ##################################

```{r}
set.seed(123)

#define 3,000 obs. sample
iphone_sample <-  slice_sample(.data = iphone_df,
                               n = 3000, 
                               replace = FALSE)


#move target variable to end
iphone_sample <- iphone_sample %>% 
  relocate(iphonesentiment,
           .after = ios)

#set up rfe control with randomn forest and repeated CV
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5, 
                   verbose = FALSE)
```


```{r}
set.seed(999)

rfe_results <- rfe(iphone_sample[, 1:11], 
                   iphone_sample$iphonesentiment, 
                   rfeControl = ctrl)

#get results
rfe_results
```

```{r}
plot(rfe_results)
```

library(nnet)

#fit multinomial logistic regression model --an extension of logistic regression for 
#multi-class classification tasks

multi_model <- nnet::multinom(iphonesentiment ~., 
                              data = iphone_df_train)


#summarize the model
summary(multi_model)
#i don't know what the output means


#make predictions
multi_model_predictions <- multi_model %>% 
  predict(iphone_df_test)

head(multi_model_predictions)


#model accuracy
mean(multi_model_predictions == iphone_df_test$iphonesentiment)
## [1] 0.6154242

#  multinomial logistic regression in R. This method is used for multiclass problems. In practice, it is not used very often. Discriminant analysis (Chapter 27 ) is more popular for multiple-class classification.



##################### DISCRIMINANT ANALYSIS FOR MULII-CLASS CLASSIFICATION

#estimate data preprocessing parameters
preproc_params <- iphone_df_train %>% 
  preProcess(method = c("center", "scale"))

#transform training data using the estimated parameters
iphone_df_train_trans <- preproc_params %>% 
  predict(iphone_df_train)

#transform test data using the est. params
iphone_df_test_trans <- preproc_params %>% 
  predict(iphone_df_test)


#i'll use flexible discriminant analysis FDA because it does not assume the vars are normally distributed or linear relationships among vars

library(mda)

#fit the model 
fda_model <- fda(iphonesentiment ~.,
                 data = iphone_df_train_trans)

#make predictions
fda_model_preds <- fda_model %>% 
  predict(iphone_df_test_trans)

#model accuracy
mean(fda_model_preds == iphone_df_test_trans$iphonesentiment)
## [1] 0.6136247



######### NAIVE BAYES MODEL ################

# can be used for both binary and multi-class tasks

#load pkg 
library(klaR)

#build the model
set.seed(123)

nb_model <- train(iphonesentiment ~.,
                  data = iphone_df_train,
                  method = "nb", 
                  trControl = trainControl("cv",
                                           number = 5))

#make predictions
nb_model_preds <- nb_model %>% 
  predict(iphone_df_test)

#model accuracy
mean(nb_model_preds == iphone_df_test$iphonesentiment)
## [1] 0.6457584

