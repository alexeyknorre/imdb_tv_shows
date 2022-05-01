set.seed(1)

shows <- readRDS("data/shows.rds")
seasons <- readRDS("data/seasons.rds")
episodes <- readRDS("data/episodes.rds")
shows_slopes <- readRDS("data/slopes.rds")


shows_slopes <- fastDummies::dummy_cols(shows_slopes, select_columns = "genres",split = ",",remove_selected_columns = T,remove_first_dummy = T)
names(shows_slopes) <- gsub("-","_",names(shows_slopes))


### Model 1: average rating

covars <- c("n_of_seasons", "n_of_episodes",
            "numVotes",
            "startYear", "runtimeMinutes",
            "genres_Action", "genres_Crime", "genres_Adventure", 
            "genres_Animation", "genres_Biography", "genres_Comedy", "genres_Drama", 
            "genres_Family", "genres_Fantasy", "genres_Romance", "genres_Sci_Fi", 
            "genres_Western", "genres_Sport", "genres_Thriller", "genres_History", 
            "genres_Horror", "genres_Reality_TV", "genres_Mystery", "genres_War", 
            "genres_Game_Show", "genres_Documentary", "genres_Music", "genres_Short", 
            "genres_Talk_Show","genres_Musical")


covars <- paste(covars, collapse = " + ")

reg_formula = as.formula(paste0("averageRating~ ",covars))

reg_1 <- lm(formula = reg_formula,
            data = shows_slopes)

## Top 5% of shows have at least 27K votes
#quantile(shows$numVotes,.95)
#95% 
#27245.2 
reg_1_top <- lm(formula = reg_formula,
            data = shows_slopes[numVotes > 27000])

### Model 2: slope

covars <- c("n_of_seasons", "n_of_episodes", "averageRating", 
  "numVotes",
  "startYear", "runtimeMinutes",
  "genres_Action", "genres_Crime", "genres_Adventure", 
  "genres_Animation", "genres_Biography", "genres_Comedy", "genres_Drama", 
  "genres_Family", "genres_Fantasy", "genres_Romance", "genres_Sci_Fi", 
  "genres_Western", "genres_Sport", "genres_Thriller", "genres_History", 
  "genres_Horror", "genres_Reality_TV", "genres_Mystery", "genres_War", 
  "genres_Game_Show", "genres_Documentary", "genres_Music", "genres_Short", 
  "genres_Talk_Show", "genres_Musical")


covars <- paste(covars, collapse = " + ")

reg_formula_2 = as.formula(paste0("coef ~ ",covars))

reg_2 <- lm(formula = reg_formula_2,
            data = shows_slopes)

reg_2_top <- lm(formula = reg_formula_2,
            data = shows_slopes[numVotes > 27000])


regressions <- list(reg_1, reg_1_top, reg_2, reg_2_top)
saveRDS(regressions,"results/estimated_ols.rds")

#### XGBoost

library(xgboost)
library(dplyr)


# Predicting average rating

shows_slopes <- shows_slopes %>% select(-show_id, -titleType, -isAdult,
                                        -originalTitle,-primaryTitle, -title)
shows_train = shows_slopes %>% sample_frac(.8)
shows_test = anti_join(shows_slopes, shows_train)


dtrain = xgb.DMatrix(data = select(shows_train, -averageRating) %>% data.matrix,
                     label = pull(shows_train, averageRating))
dtest = xgb.DMatrix(data = select(shows_test, -averageRating) %>% data.matrix,
                    label = pull(shows_test, averageRating))


params = list(booster = 'gbtree',    
              # which model to use (Yes, there are other models apart from gradient tree boosting).
              # default: 'gbtree' for gradient tree boosting
              eta = 0.2, 
              # shrinkage tuning parameter. Default: 1
              max_depth = 3, 
              # max depth of a tree. Default: 6
              subsample = 0.7, 
              # subsample ratio for observations. Default: 1
              gamma = 1,
              # penalty on number of leaves in a tree. Default: 0
              lambda = 1,
              # penalty on score 2-norm. Default: 1
              colsample_bytree = sqrt(ncol(shows_train)-1) / sqrt(ncol(shows_train)-1),
              # subsample ratio for features. Default: 1
              min_child_weight = 1,
              # minimum sum of instance scores in a leaf. 
              # If the sum is smaller than this minimum in some leaf, then the leaf will be dropped. 
              # So the less this value, the smaller this tree is. Default: 1
              objective = 'reg:squarederror'
              # form of loss function
              # default: 'reg:squarederror' for regression with squared loss
              # other options include: 
              # 'reg:logistic' for logistic function for numeric response at [0,1]
              # 'binary:logistic' for logistic function for binary response
)


xgb.cv.result = xgb.cv(data=dtrain, params = params, nrounds = 500,
                       early_stopping_rounds = 50,
                       # if the model performance keep becoming worse after k rounds of iteration, 
                       # the algorithm will automatically stop.
                       nfold = 10,
                       # folds of cv
                       prediction = TRUE,
                       verbose = 0
                       # show prediction result
)

# fit the model using cv result
xgb.fit = xgboost(data=dtrain, 
                  params = params, 
                  nrounds=xgb.cv.result$best_iteration,
                  verbose = 0)
pred = predict(xgb.fit, dtest)
rmse_xgb_default_1 <- mean((pred - pull(shows_test, averageRating))^2) %>% sqrt()

#########
ntrees = xgb.cv.result$best_iteration
param_grid = expand.grid(
  nrounds = ntrees,
  eta = seq(2,24,2)/ntrees,
  subsample = c(0.5,0.7,1.0),
  colsample_bytree = 1.0,
  max_depth = c(1,2,3,4,5,6),
  gamma = 1,
  min_child_weight = 1
)

library(caret)
xgb_control <- trainControl(
  method="cv",
  number = 5
)

xgb.tuned <- train(averageRating~., data=shows_train, trControl=xgb_control, tuneGrid=param_grid, lambda=1, method="xgbTree")
colnames(dtest) <- NULL

pred = predict(xgb.tuned$finalModel, dtest)
rmse_xgboost_tuned_1 = mean((pred - pull(shows_test,averageRating))^2) %>% sqrt()



xgb_imp <- xgb.importance(feature_names = xgb.tuned$finalModel$feature_names,
                          model = xgb.tuned$finalModel)

features_1 <- head(xgb_imp[,1:2],10)


### Predicting slope

dtrain = xgb.DMatrix(data = select(shows_train, -coef) %>% data.matrix,
                     label = pull(shows_train, coef))
dtest = xgb.DMatrix(data = select(shows_test, -coef) %>% data.matrix,
                    label = pull(shows_test, coef))

xgb.cv.result = xgb.cv(data=dtrain, params = params, nrounds = 500,
                       early_stopping_rounds = 50,
                       # if the model performance keep becoming worse after k rounds of iteration, 
                       # the algorithm will automatically stop.
                       nfold = 10,
                       # folds of cv
                       prediction = TRUE,
                       verbose = 0
                       # show prediction result
)

# fit the model using cv result
xgb.fit = xgboost(data=dtrain, 
                  params = params, 
                  nrounds=xgb.cv.result$best_iteration,
                  verbose = 0)
pred = predict(xgb.fit, dtest)
rmse_xgb_default_2 <- mean((pred - pull(shows_test, coef))^2) %>% sqrt()

#########
ntrees = xgb.cv.result$best_iteration


library(caret)
xgb_control <- trainControl(
  method="cv",
  number = 5
)

xgb.tuned <- train(coef~., data=shows_train, trControl=xgb_control, lambda = 1, tuneGrid=param_grid, method="xgbTree")

colnames(dtest) <- NULL

pred = predict(xgb.tuned$finalModel, dtest)
rmse_xgboost_tuned_2 = mean((pred - pull(shows_test,coef))^2) %>% sqrt()



xgb_imp <- xgb.importance(feature_names = xgb.tuned$finalModel$feature_names,
                          model = xgb.tuned$finalModel)

features_2 <- head(xgb_imp[,1:2],10)

#### OLS

reg_1 <- lm(formula = reg_formula,
            data = shows_train)

reg_1_predicted <- predict(reg_1,newdata = shows_test)
rmse_ols_1 <- sqrt(mean((reg_1_predicted - shows_test$averageRating)^2))


reg_2 <- lm(formula = reg_formula_2,
            data = shows_train)

reg_2_predicted <- predict(reg_2,newdata = shows_test)
rmse_ols_2 <- sqrt(mean((reg_2_predicted - shows_test$coef)^2))

#### Wrapping up

xgboost_features <- list(features_1,features_2)
saveRDS(xgboost_features,"results/xgboost_features.rds")

prediction_results <- data.frame(RMSE=c("Average show rating","Seasonal change (slope)"),
                                 ols = c(rmse_ols_1,rmse_ols_2),
                                 xgboost_default = c(rmse_xgb_default_1,rmse_xgb_default_2),
                                 xgboost_tuned = c(rmse_xgboost_tuned_1,rmse_xgboost_tuned_2))

saveRDS(prediction_results,"results/prediction_results.rds")

