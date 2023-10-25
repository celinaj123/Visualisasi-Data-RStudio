achievements_data = read.csv("F:/binus/Data Mining/Sesi IV/Sesi IV/Dataset/achievements.csv", na.strings =  c("", "NA"))
#View(data1)

achievements_category_data = read.csv("F:/binus/Data Mining/Sesi IV/Sesi IV/Dataset/achievements-category.csv", na.strings =  c("", "NA"))
#View(data2)

#top_5_ach_cat = data2[order(data2$Category,
 #                                    decreasing = TRUE), ]
#top_5_ach_cat = top_5_ach_cat[1:5,]

#Importing Data
#achievements_category_data <- read.csv("achievements_category.csv")
#achievements_data <- read.csv("achievements.csv")


#Clean data
achievements_category_data <-
  achievements_category_data[complete.cases(achievements_category_data), ]

achievements_data <-
  achievements_data[complete.cases(achievements_data), ]

#show top 5 base category with pie
library(ggplot2)

category_counts <- table(achievements_category_data$Category)
sorted_categories <- sort(category_counts, decreasing = TRUE)
top_5_ach_cat <- head(sorted_categories, 5)

pie(top_5_ach_cat, main = "Top 5 Achievements", col = rainbow(5),
    labels = paste(names(top_5_ach_cat), "\n", top_5_ach_cat))

#show bar 

sequential_counts <- table(achievements_data$Sequential)
barplot(sequential_counts, col = rainbow(length(sequential_counts)),
        ylab = "Frequency", main = "Frequency of Sequential(s) Achievements")


#show tier based rewardtier1 into small,normal,large
tier_reward <- merged$Reward.Tier.1
large <- sum(tier_reward > 820)
normal <- sum(tier_reward >480 & tier_reward <= 820)
small <- sum(tier_reward <= 480)

barplot(
  c(large,normal,small),
  main = "Total Number of Achievement based on Reward Tier 1",
  names.arg = c("Large","Normal","Small"),
  xlab = "Reward Categories",
  ylab = "Frequency"
  col = rainbow(3)
)

#data preprocessing
new_achievements <- merged[
  merged$Category != 'Money' &
    merged$Category != 'Communication' &
    merged$Category != 'LandMaking' &
    merged$Sequential != 'No' &
    merged$Num.of.Tier != 6,
]

#data transform
splitted_data <- split(new_achievements$Category, new_achievements$Num.of.Tiers)
library(arules)

transactions <- as(splitted_data, "transactions")

#Apriori
rules <- apriori(
  transactions,
  parameter = list(
    support = 0.6,
    target = "frequent itemsets"
  )
)

inspect(rules)

#Assoc Rules
rules <- ruleInduction(rules,confidence = 0.8)

inspect(rules)
