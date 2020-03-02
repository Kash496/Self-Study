install.packages("tidyverse")
install.packages("gapminder")
install.packages("tidyr")
install.packages("corrgram")
library("corrgram")
library("dplyr")
library("tidyr")
library("tidyverse")
library("gapminder")
data <-read.csv("C:/Users/Aishu/Desktop/MS Information Systems/Aishwarya/Statistical Computing/new-york-city-airbnb-open-data/AB_NYC_2019.csv")
head(data)
view(data)
#-----------------VISUALIZATION----------------#
bar <- ggplot(data = data) + 
  geom_bar(
    mapping = aes(x = neighbourhood_group, fill = room_type), 
    show.legend = TRUE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()
bar

#-----------------------PRICE AND NEIGHBORHOOD----------------------#
data_filt1 <- data %>% 
  filter(room_type == 'Entire home/apt')
view(head(data_filt1))
data_filt2 <- data %>% 
  filter(room_type == 'Private room')
view(head(data_filt2))
data_filt3 <- data %>% 
  filter(room_type == 'Shared room')
view(head(data_filt3))
scp_filt1 <- ggplot(data = data_filt1) + 
  geom_point(mapping = aes(x = price, y = number_of_reviews, color=neighbourhood_group))
scp_filt1
scp_filt2 <- ggplot(data = data_filt2) + 
  geom_point(mapping = aes(x = price, y = number_of_reviews, color=neighbourhood_group))
scp_filt2
scp_filt3 <- ggplot(data = data_filt3) + 
  geom_point(mapping = aes(x = price, y = number_of_reviews, color=neighbourhood_group))
scp_filt3
#--------------------SELECTION, ORDER_BY AND AGGREGATE FUNCTIONS------------------------#
data_sel <- data %>% 
  select(name, neighbourhood_group, neighbourhood, room_type, price, minimum_nights, availability_365)
view(head(data_sel))
data_simp_grp <- data_sel %>% 
                group_by(neighbourhood_group) %>% 
                summarise(count = n())
view(data_simp_grp)
data_n_grp <- data_sel %>% 
  group_by(neighbourhood) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
view(head(data_n_grp))
data_room <- data_sel %>% 
  group_by(neighbourhood,room_type) %>% 
  summarise(count = n(), avg_price = mean(price)) %>% 
  arrange(neighbourhood, avg_price)
view(head(data_room))
#------------------COUNTING AND REMOVING NULL VALUES IN THE DATASET----------------------#
count_NA <- function(x) {
  
  total <- sum(is.na(x$reviews_per_month))
  return(total)
  
}
missing_count <- count_NA(data)
missing_count
if(missing_count > 0)
{
  clean_data <- data %>% drop_na()
} else {
  clean_data <- data
}
view(clean_data)
#------------------REMOVING ROOMS WITH ZERO AVAILABILTY----------------------------#
clean_data <- filter(clean_data, availability_365 > 0)
view(clean_data)
#---------------------ANALYZING CORRELATIONS-----------------------------------------#
cor(clean_data %>% select(price, reviews_per_month, availability_365))
corrgram(clean_data %>% select(price, minimum_nights, calculated_host_listings_count, reviews_per_month, availability_365))
#-----------------LINEAR REGRESSION ON PRICE AND AVAILABILITY----------------------#
model <- lm(price ~ availability_365, clean_data)
summary(model)

plot(model$fitted.values, model$residuals)
hist(model$residuals)
model$coefficients
avail_coeff <- model$coefficients[2]
100 * avail_coeff
paste0("For each $100 increase in price, the average availability increases by ", round(100*avail_coeff,2), " days.", sep = "")

#---------------ASSUMPTIONS------------------------
res_hat <- model$residuals
res_hist <- ggplot(data.frame(res_hat), aes(res_hat)) + geom_histogram()
res_hist
shapiro.test(res_hat)
t.test(res_hat)
plot(model$fitted.values, res_hat)
#-----------------------TRANSFORMATIONS---------------------------------------#
ggplot(clean_data) + geom_point(aes(x = availability_365, y = price))
clean_data <- clean_data %>% mutate(ln_price = log(price),ln_availability_365 = log(availability_365))
view(clean_data)
# inspect the relationship with a scatter plot - Transforming Availability
ggplot(clean_data) + geom_point(aes(x=availability_365, y=ln_price))
model2 <- lm(ln_price ~ availability_365, clean_data)
summary(model2)
plot(model2$fitted.values, model2$residuals)
hist(model2$residuals)

#Transforming Price - Doesn't fit
ggplot(clean_data) + geom_point(aes(x=ln_availability_365, y=price))
model3<- lm(price ~ ln_availability_365, clean_data)
summary(model3)
plot(model3$fitted.values, model3$residuals)
hist(model3$residuals)
#------------------------DOESN'T WORK-------------------------------------------
clean_data <- filter(clean_data, price > 0)
ggplot(clean_data) + geom_point(aes(x=ln_availability_365, y=ln_price))
model4<- lm(ln_price ~ ln_availability_365, clean_data)
summary(model4)
plot(model4$fitted.values, model4$residuals)
hist(model4$residuals)

#PERFORMANCE EVALUATION
train_idx <- sample(nrow(clean_data), .70*nrow(clean_data))

ab_train <- clean_data[train_idx,]
ab_test <- clean_data[-train_idx,]

str(ab_train)
str(ab_test)

#--------------------MODEL--------------------
mod1 <- lm(availability_365 ~ price, ab_train)
summary(mod1)

mod2 <- lm(ln_availability_365 ~ ln_price, ab_train)
summary(mod2)

mod1_OOS_preds <- predict(mod1, ab_test)
str(mod1_OOS_preds)

mod2_OOS_preds <- predict(mod2, ab_test)
str(mod2_OOS_preds)

mod2_OOS_preds <- exp(mod2_OOS_preds)
str(mod2_OOS_preds)

#------------------------PERFORMANCE----------------------------
calc_performance <- function(actual, pred) {
  
  rmse <- sqrt(mean((actual - pred)**2))
  mae <- mean(abs(actual - pred))
  mape <- mean(abs((actual-pred)/actual))
  
  retvals <- list(rmse = rmse, mae = mae, mape = mape)
  return(retvals)
  
}
OOS_mod1 <- calc_performance(ab_test$price, mod1_OOS_preds)
OOS_mod1

OOS_mod2 <- calc_performance(ab_test$price, mod2_OOS_preds)
OOS_mod2