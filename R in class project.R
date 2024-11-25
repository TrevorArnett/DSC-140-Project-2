#Part 1
#Import datasets
yelp_user <- read.csv("~/R in class Project/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(yelp_user)
yelp_business <- read.csv("~/R in class Project/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(yelp_business)
print(head(yelp_business, 5))
#Import ggplot libreary
library(ggplot2)
#Bar graph
ggplot(yelp_business)+ geom_bar(aes(x=state))
ggplot(yelp_business)+ pie(aes(stars))
#Pie chart
cont_table<- table(yelp_business$stars)
print(cont_table)
pie(cont_table, main="Stars", col=rainbow(3))

#Box-plots
ggplot(yelp_business,aes(x=review_count,y=stars==1:5,
                    fill=review_count))+
  geom_boxplot(show.legend=FALSE) +labs(x= "Amount of Reviews",
                                        y="Stars")

#Chi-squared test
five_stars <- subset(yelp_business,stars ==5)
print(five_stars)
View(five_stars)

one_stars <- subset(yelp_business, stars==1)
View(one_stars)

yelp_cont_table = table(yelp_business$stars==5, yelp_business$stars==1)
print(yelp_cont_table)
chisq.test(yelp_cont_table) # This test tells me that there is no correlation between stars and review count

#Part 2
names(yelp_user) # Prints the names of the columns

# Pearson's R Correlation
corr <- cor(yelp_user[, c("cool_votes", "funny_votes","useful_votes")])
print(corr)
#Linear regression analysis
linear_model <- lm(yelp_user$cool_votes~yelp_user$funny_votes)
print(linear_model)

ggplot(yelp_user) + geom_point(aes(cool_votes,funny_votes)) + 
  geom_smooth(aes(cool_votes,funny_votes), method="lm", se=F) + 
  labs(x="Cool Votes", y="Funny Votes")


linear_model <- lm(yelp_user$review_count~yelp_user$fans)
# Print the linear model
print(linear_model)

#scatter plot of the data with a line of best fit added.
ggplot(yelp_user) + geom_point(aes(review_count,fans)) + 
  geom_smooth(aes(review_count,fans), method="lm", se=F) + 
  labs(x="Review Count", y="Fans")
# Writing reviews dows not always mean more fans, there are plenty
#of users with alot of fans that have less reviews than others
linear <- lm(yelp_user$funny_votes~yelp_user$fans)
print(linear)
ggplot(yelp_user) + geom_point(aes(funny_votes,fans)) + 
  geom_smooth(aes(funny_votes,fans), method="lm", se=F) + 
  labs(x="Funny Votes", y="Fans")
# Funny Reviews tend to provide alot of followers
# Not sure how to do the Kmeans question
