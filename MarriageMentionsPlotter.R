library(readr)
library(ggplot2)
library(scales)

d <- read_csv('/Users/michael/Documents/Personal/marriage_mentions_2019.csv')
m <- read_csv('/Users/michael/Documents/Personal/marriage_meta_data_2019.csv')
l <- read_csv('/Users/michael/Documents/Personal/marriage_dates_of_note_2019.csv')

# convert date to unix time stamps
d$UnixDate <- as.numeric(as.POSIXct(d$Date))
l$UnixDate <- as.numeric(as.POSIXct(l$Date))

months <- c('2018-12-1','2019-1-1','2019-2-1','2019-3-1','2019-4-1','2019-5-1','2019-6-1','2019-7-1','2019-8-1','2019-9-1','2019-10-1','2019-11-1','2019-12-1')
months_pretty <- c('December 2018','January 2019','February 2019','March 2019','April 2019','May 2019','June 2019','July 2019','August 2019','September 2019','October 2019','November 2019','December 2019')
months_unix <- as.numeric(as.POSIXct(months))

# add the month meta data to main tibble
d$Month <- factor(m$Month)
l$UnixDateLabel <- l$UnixDate - 300000
min_date = d[d$UnixDate == min(d$UnixDate),]$Date
max_date = d[d$UnixDate == max(d$UnixDate),]$Date
p <- ggplot(data=d,aes(x=UnixDate,y=Count,color=Month)) + geom_path(aes(group=1),size=3) + geom_point(size=4) + 
  theme_bw() +
  geom_text(data=l,aes(x=UnixDateLabel, label=Label, y=9), colour="black", angle=90, size=18) +
  ggtitle('Number of Times Family Mentioned Michael Getting Married in 2019')+
  xlab('Time')+
  ylab('Number of Mentions')+
  scale_y_continuous(breaks = round(seq(min(d$Count), max(d$Count), by = 1),1))+
  # this time scale is going from the unix time stamp of Dec 1 2018 to Dec 31 2019: manually change it as needed
  scale_x_continuous(breaks = months_unix,
                     labels = months_pretty)+
  theme(plot.title = element_text(size=72,hjust = 0.5),
        plot.margin = unit(c(2,2,2,2), "in"),
        axis.text=element_text(size=28),
        axis.title=element_text(size=40,face="bold"),
        legend.position="none")

# add each event of note to the plot
for(i in seq(nrow(l))){
 p = p + geom_vline(xintercept = as.numeric(l[i,3]),linetype = "dotted", color = 'red',size=3)
}
ggsave('/Users/michael/Documents/Personal/marriage_mentions_plot_2019.png',width = 48, height = 36, units = c("in"))
