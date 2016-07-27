library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
#library(caret)

rm(list = ls())

suburb <- "Hope Valley"
suburb_data <- read.delim2(paste("suburbs/", suburb, ".csv", sep = ""), header=FALSE, comment.char="#", stringsAsFactors=FALSE)

suburb_data$V1 <- str_trim(suburb_data$V1, side = "both")

sub <- suburb_data

len <- dim(sub)[1]

sub_obs <- c()
S <- 0
E <- 0
for(i in c(1:len)){
  new_obs <- c()
  
  if(grepl(sub$V1[i], pattern = suburb)){
    S <<- i
    next
  }
  if(grepl(sub$V1[i], pattern = "Attributes")){
    E <<- i
    new_obs <- sub$V1[S:E]
    if(length(new_obs) == 6){
      sub_obs <<- rbind(sub_obs, new_obs)
      next
    }
  }
}

get_attr_count <- function(x, type){
  attribs <- unlist(str_split(x,  pattern  = ", "))
  result <- attribs[grep(type, attribs)]
  count <- unlist(gsub(pattern = "[^0-9]", replacement = "", result))
  count <- as.numeric(count)
  if(length(count) <= 0){
    count[1] <- 0
  }
  return(count)
}

sub_obs <- as.data.frame(sub_obs)

sub_vals <- sub_obs %>%
  mutate(street = factor(str_split_fixed(string = V1, pattern = " ", n = 2)[,2])) %>%
  mutate(sale_price = as.numeric(gsub(pattern = "(Sale Price:|[$,])", replacement = "", V2))) %>%
  mutate(sale_date = dmy(gsub(pattern = "(Sale Date:)", replacement = "", V3))) %>%
  mutate(area = as.numeric(gsub(pattern = "(Area:|m2|,|Approx)", replacement = "", V4))) %>%
  mutate(eq_area = as.numeric(gsub(pattern = "(Eq\\. Building Area:|m2|,|Approx)", replacement = "", V5))) %>%
  mutate(atts = gsub(pattern = "(Attributes:)", replacement = "", V6)) %>%
  group_by(atts) %>%
  mutate(beds = get_attr_count(atts, "Beds")) %>%
  mutate(baths = get_attr_count(atts, "Baths")) %>%
  mutate(car_space = get_attr_count(atts, "Car")) %>%
  mutate(garage = get_attr_count(atts, "Garages")) %>%
  mutate(ensuite = get_attr_count(atts, "Ensuites"))

sub_vals <- as.data.frame(sub_vals[sub_vals$sale_price > 0,c(7:17)])

write.table(sub_vals, file = paste("output/ ", suburb,"_df.csv",sep = ""), sep = ",", row.names = F)

p <- ggplot(data = sub_vals[sub_vals$sale_price < 2000000,], aes(x = year(sale_date), y = sale_price))
p + geom_point() + geom_smooth()
p + stat_summary(fun.y = "median", geom = "point")

baths <- ggplot(data = sub_vals[sub_vals$sale_price < 1e6 & year(sub_vals$sale_date) >= 2015,], aes(x = eq_area, y = sale_price))
baths + stat_summary(fun.y = "median", geom = "point")
baths + geom_point()

# top10 <- sort(table(sub_vals$street), decreasing = T)[1:10]
# 
# by_street <- sub_vals[year(sub_vals$sale_date) > 2015,] %>% 
#   group_by(street) %>%
#   mutate(mean_yr_street_price = mean(sale_price)) %>%
#   distinct()

by_year <- sub_vals %>%
  group_by(sale_date) %>%
  mutate(year = year(sale_date)) %>%
  group_by(year) %>%
  mutate(median_sale_price = median(sale_price, na.rm = T)) %>%
  mutate(mean_area = mean(area, na.rm = T)) %>%
  select(median_sale_price, mean_area) %>%
  distinct() %>% 
  ungroup() %>%
  arrange(desc(year))


by_street <- sub_vals %>%
  group_by(street) %>%
  mutate(mean_street_sale = mean(sale_price)) %>% 
  select(street, mean_street_sale) %>%
  distinct()

View(sub_obs)
View(sub_vals)
View(by_year)
View(by_street)

###########################
## BEGIN MODEL TRAINING ###
###########################
library(caret)

set.seed(1234)
mydata <- sub_vals[complete.cases(sub_vals),]
mydata <- mydata %>% filter(year(sale_date) >= 2015, sale_price < 1000000)

train_index <- createDataPartition(mydata$sale_price, p = 0.5, list = FALSE)
train <- mydata[train_index,]
test <- mydata[-train_index,]

frm <- sale_price ~ area + eq_area + beds + baths + ensuite + garage

ctrl <- trainControl(method = "cv", number = 10)

ctree_model <- train(form = frm,
                     data = train,
                     method = "ctree",
                     trControl = ctrl,
                     tuneLength = 3)

rf_model <- train(form = frm,
                  data = train,
                  method = "rf",
                  trControl = ctrl,
                  ntree = 300, 
                  tuneLength = 1)
rf_predict <- predict(rf_model, test)
plot(test$sale_price ~ test$ensuite)
lines(sort(rf_predict) ~ sort(test$ensuite), lwd = 2, col = "blue")

lm_model <- train(form = frm,
                  data = train,
                  method = "lm",
                  trControl = ctrl,
                  tuneLength = 3)
lm_predict <- predict(lm_model, test)
plot(test$sale_price ~ test$eq_area)
lines(sort(lm_predict) ~ sort(test$ensuite), lwd = 2, col = "red")
# 
gbm_tune <- expand.grid(shrinkage = c(0.1, 0.05, 0.01),
                        n.minobsinnode = c(5, 7, 10),
                        n.trees = 250,
                        interaction.depth = c(1,2,3,4,5))
gbm_model <- train(form = frm,
                   data = train,
                   method = "gbm",
                   trControl = ctrl,
                   tuneLength = 3)
gbm_predict <- predict(gbm_model, test)
# 
# #
predict(gbm_model, newdata = data.frame(area = 1015,
                                       eq_area = 170,
                                       beds = 4,
                                       baths = 2,
                                       ensuite = 1,
                                       garage = 4))


