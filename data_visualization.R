library(readxl)
library(ggplot2)

default <- read_excel("ccdefault.xls",col_names=TRUE, skip=1) #skipped the first row, because wanted to assign column names from the second row
default.clean <- default[,-1] #deleted ID column

# One categorical variable Bar plot of counts on PAY_# columns paste0("PAY_", 1:6)
for (i in paste0("PAY_", 1:6)) {
  print(ggplot(default.clean, aes(default.clean[[i]])) + geom_bar(fill = "darkblue") +
          scale_x_continuous(breaks=seq(-2,8,1)) +
          theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(title = i))
}


#Plotting the Credit Limit, expand to see x-axis ticks
ggplot(default.clean, aes(default.clean$LIMIT_BAL)) + geom_bar(fill = "purple", position="identity", width=4000) +
  scale_y_continuous(trans='log10') + scale_x_continuous(breaks=seq(0,1000000,10000)) +
  theme(axis.text.x = element_text(angle=90), axis.title.x=element_blank(), axis.title.y=element_blank()) +
  labs(title = "LIMIT_BAL")

#Plotting Gender distribution
ggplot(default.clean, aes(default.clean$SEX)) +
  geom_bar(fill = c('blue', 'pink'), position="identity") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(title = "Gender") + 
  scale_x_continuous(breaks=seq(1,2,1), labels=c("male", "female")) + scale_y_continuous(breaks=seq(0,20000,2000))

#Plotting Education distribution
ggplot(default.clean, aes(default.clean$EDUCATION)) +
  geom_bar(position="identity", fill='blue') +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle=45)) + labs(title = "EDUCATION") + 
  scale_x_continuous(breaks=seq(1,4,1), limits = c(0, 5), labels=c("graduate \n school", "university", "high \n school", "others")) +
                       scale_y_continuous(breaks=seq(0,15000,2000))

#Plotting Marriage distribution
ggplot(default.clean, aes(default.clean$MARRIAGE)) +
  geom_bar(position="identity", fill = c('black','green', 'red', 'yellow')) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(title = "Marriage") + 
  scale_x_continuous(breaks=seq(1,3,1), limits = c(0, 4), labels=c("married", "single", "other")) + scale_y_continuous(breaks=seq(0,20000,2000))

#Age distribution
ggplot(default.clean, aes(default.clean$AGE)) + geom_bar() + scale_x_continuous(breaks=seq(20,80,4)) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(angle=90)) +
  labs(title = "Age")


#Plotting the amount of people who default and doesn't default
ggplot(default.clean, aes(default.clean$default_next_month)) +
  geom_bar(fill = c('green', 'red'), position="identity") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(title = "default_next_month") + 
  scale_x_continuous(breaks=seq(0,1,1), labels=c("OK", "default"))
