library(tidyverse)
library(caret)

set.seed(42)

# Build the dataset
df <- tibble(
  x = rnorm(50, mean = 2, sd = 2),
  y = rcauchy(50),
  z = runif(n = 50, min = 5, max = 10)
)

# Split into train/test samples
train_indexes <- sample(x = nrow(df), size = 0.8*nrow(df) )
df_train <- df %>% slice(train_indexes)
df_test <- df %>% slice(-train_indexes)

## CARET
# Compute scaling parameters solely on the train sample
standardize_params <- preProcess(df_train, method = c("center", "scale"))
normalize_params <- preProcess(df_train, method = c("range"))

# For example, standardize both train and test samples
df_train_standardized_caret <- predict(standardize_params, df_train)
df_test_standardized_caret <- predict(standardize_params, df_test)

## BASE R
# Compute scaling parameters solely on the train sample
means <- apply( X = df_train, MARGIN = 2, FUN = mean )
stdvs <- apply( X = df_train, MARGIN = 2, FUN = sd )

# Standardize the test sample
df_test_standardized_baseR <- df_test %>%
  sweep( MARGIN = 2, STATS = means, FUN = "-" ) %>%
  sweep( MARGIN = 2, STATS = stdvs, FUN = "/" )

## TIDYVERSE'S DPLYR
# Compute scaling parameters solely on the train sample
means <- df_train %>% summarise(across(everything(), mean))
stdvs <- df_train %>% summarise(across(everything(), sd))

# Standardize the test sample
df_test_standardized_dplyr <- df_test %>%
  rowwise() %>%
  mutate(
    (across(everything())-means)/stdvs
    ) %>%
  ungroup()

## Check the results are consistent
df_test_standardized_caret %>% summarise_all(mean)
df_test_standardized_baseR %>% summarise_all(mean)  
df_test_standardized_dplyr %>% summarise_all(mean) 

# The results are all the same :)