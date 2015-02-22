
#import libraries
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(wordcloud)
library(tm)
# library(rjson) used for the ip convertor
library(rworldmap)


#Import data

#Main dataset
dat <- read.csv("Disgust dataset.csv")
#Change the gender to 1 for Male and 2 for Female
dat$Sex <- factor(dat$Sex)
levels(dat$Sex)[levels(dat$Sex)=="1"] <- "Female"
levels(dat$Sex)[levels(dat$Sex)=="2"] <- "Male"

#Set of IPs only, editted out as I don't want to upload the ips onto github
# ips <- read.csv("IP.csv") 




#Functions

#Freegeop function to translate the ips into countries
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}   

#Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Lets do some charts!

#Demographics

#RWA histogram
ChartRWA <- ggplot(dat, aes(x=RWAscore)) + geom_histogram(binwidth=5, colour="black", fill="lightgrey") +
  geom_vline(aes(xintercept=mean(RWAscore)),   # Ignore NA values for mean
             color="#ba1026", linetype="dashed", size=1) +
  ggtitle("RWA Scores") +
  xlab("") +
  ylab("") +
  theme_bw()
ChartRWA

#Gender barchart
ChartGender <- ggplot(dat, aes(x=Sex, fill=Sex)) + 
  theme_bw() +
  geom_bar(binwidth=5, colour="Black", guide=FALSE) +
  scale_fill_manual(values=c("#10baa4", "#ba1026"), guide=FALSE) +
  ggtitle("Gender") +
  xlab("") +
  ylab("") +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust= +2)
ChartGender

#Age histogram
ChartAge <- ggplot(dat, aes(x=Age)) + geom_histogram(binwidth=5, colour="black", fill="lightgrey") +
  geom_vline(aes(xintercept=mean(Age)),   # Ignore NA values for mean
             color="#ba1026", linetype="dashed", size=1) +
  ggtitle("Age") +
  xlab("") +
  ylab("") +
  theme_bw() 
ChartAge

# Country map

# This first part is for translating the ip numbers into country names. Commented out as I don't want to accidentily run it again
# ips[] <- lapply(ips, as.character)
# countries <- freegeoip(ips[,1])$country_name
# At this stage I can see the data in summary of countries, so I'm just going to write it straight in.

d <- data.frame(
  country=c("Ireland", "United Kingdom", "Sweden", "United States", "France","Denmark","Australia","Canada","Czech Republic","Singapore","Netherlands","Estonia","Brazil","New Zealand","South Africa","Romania","Germany","Italy","Gibraltar"),
  value=c(57, 91, 2, 170, 2, 2, 6, 9, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1))
n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")


colourpalette <-brewer.pal(8,'Greens') 
#mapCountryData(n, xlim=c(-10, 40), ylim=c(35, 70), nameColumnToPlot="value", mapTitle="",oceanCol="lightblue",missingCountryCol="lightgrey", catMethod=c(0,25,50,75,100,125,150,175,200), colourPalette=colourpalette)
MapChart <- mapCountryData(n, xlim=c(-120, 130), ylim=c(-50, 50), nameColumnToPlot="value", mapTitle="Participant location",oceanCol="lightblue",missingCountryCol="lightgrey", catMethod=c(0,25,50,75,100,125,150,175,200), colourPalette=colourpalette)
MapChart


dbar <- data.frame(
  country=c("Ireland", "United Kingdom", "United States" ,"Australia","Canada","Other"),
  value=c(57, 91, 170, 6, 9, 19 ))

ChartCountries <- ggplot(dbar, aes(x=reorder(country, value),y=value,fill=country)) + geom_bar(stat="identity") +
  ggtitle("Country") +
  xlab("") +
  ylab("") +
  coord_flip() +
  theme_bw()+
  scale_fill_manual(values=c("#10baa4", "#a410ba","#baa410", "#ba1026","#10baa4", "#ba4f10"), guide=FALSE) +
  geom_text(aes(label = value), hjust=1)
ChartCountries

#Multiplot doesn't draw the MapChart :( must only for ggplot
multiplot(ChartAge, ChartGender, ChartCountries, ChartRWA, cols=2)


#SCATTERPLOTS

# Age correlation scatterplot for cooked crickets
sp1 <- ggplot(dat, aes(x=Age, y=BeforeCricket)) +
  geom_point(shape=1,position=position_jitter(height=.2)) +   
  geom_smooth(method=lm) +   # Add linear regression line 
#  (by default includes 95% confidence region)
  ggtitle("Cooked crickets") +
  scale_y_continuous(limits=c(0, 10.2), breaks=0:10*2) +   
  theme_bw() +
  xlab("") +
  ylab("")
  
# Age correlation scatterplot for bug based bar
sp2 <- ggplot(dat, aes(x=Age, y=BeforeCricketBar)) +
  geom_point(shape=1,      # Use hollow circles
             position=position_jitter(height=.2)) +  
  geom_smooth(method=lm) +   # Add linear regression line 
  #  (by default includes 95% confidence region)
  ggtitle("Cricket flour bar")+
  scale_y_continuous(limits=c(0, 10.2), breaks=0:10*2) +   
  theme_bw()+
  xlab("") +
  ylab("")

#Draw the plots
multiplot(sp1, sp2, cols=2)



#BOX PLOTS


#box plots for crickets
bp1 <- ggplot(dat, aes(x=Sex, y=BeforeCricket, fill=Sex)) + 
  geom_boxplot()
bp1 <- bp1 + scale_fill_manual(values=c("#10baa4", "#ba1026"),guide=FALSE) + 
  theme_bw()  +
  scale_y_continuous(limits=c(0, 10.2), breaks=0:10*2) +  
  theme(legend.position="top") +
  ggtitle("Cooked crickets") +
  ylab("") +
  xlab("")

#box plots for bug based bar
bp2 <- ggplot(dat, aes(x=Sex, y=BeforeCricketBar, fill=Sex)) + 
  geom_boxplot()
bp2 <- bp2 + scale_fill_manual(values=c("#10baa4", "#ba1026"),guide=FALSE, 
                       name="",
                       labels=c("Male", "Female")) + 
  theme_bw()  +
  scale_y_continuous(limits=c(0, 10.2), breaks=0:10*2) +  
  theme(legend.position="top") +
  ggtitle("Cricket flour bars")+
  ylab("") +
  xlab("")

#Draw the plots
multiplot(bp1, bp2, cols=2)



# RWA correlation scatterplot for cricket
rwa2 <- ggplot(dat, aes(x=RWAscore, y=CricketChange)) +
  geom_point(shape=1, position=position_jitter(height=.2)) +
  geom_smooth(method=lm) +   # Add linear regression line 
#  (by default includes 95% confidence region)
  ggtitle("RWA / Cooked cricket rating change") +
  scale_y_continuous(limits=c(-6, 7), breaks=-5:5*2) +  
  theme(legend.position="top") +
  ylab("") +
  xlab("") +
  theme_bw()

# RWA correlation scatterplot for cricket
rwa3 <- ggplot(dat, aes(x=RWAscore, y=BarChange)) +
  geom_point(shape=1,      # Use hollow circles
             position=position_jitter(height=.2)) + 
  geom_smooth(method=lm) +   # Add linear regression line 
  #  (by default includes 95% confidence region)
  theme_bw() +
  ggtitle("RWA / Cricket flour bar rating change") +
  scale_y_continuous(limits=c(-6, 7), breaks=-5:5*2) +  
  theme(legend.position="top") +
  ylab("") +
  xlab("")

#Draw the plots
multiplot(rwa2, rwa3, cols=2)



# Word cloud
words <- Corpus (DirSource("comments/"))
words <- tm_map(words, stripWhitespace)
words <- tm_map(words, content_transformer(tolower))
words <- tm_map(words, removeWords, stopwords("english"))
words <- tm_map(words, removePunctuation)
words <- tm_map(words, removeWords, c("eating","eat","eaten"))
words <- tm_map(words, removeWords, c("crickets","cricket","insects","insect"))
wordcloud(words, max.words=100, min.freq=5, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



# Before and after histograms

# Prepping the data, the code wanted it in a particular format so I just went with that

#Cricket Before and After
cut1 <- summarySE(dat,measurevar="BeforeCricket", groupvars="Group")
cut1$BeforeAfter <-c("0","0")
cut1$Group <-c("Intellectual","Social")
names(cut1)[names(cut1)=="BeforeCricket"] <- "Rating"
cut2 <- summarySE(dat,measurevar="AfterCricket", groupvars="Group")
cut2$BeforeAfter <-c("1","1")
cut2$Group <-c("Intellectual","Social")
names(cut2)[names(cut2)=="AfterCricket"] <- "Rating"

#Cricket Bar Before and After
cut3 <- summarySE(dat,measurevar="BeforeCricketBar", groupvars="Group")
cut3$BeforeAfter <-c("0","0")
cut3$Group <-c("Intellectual","Social")
names(cut3)[names(cut3)=="BeforeCricketBar"] <- "Rating"
cut4 <- summarySE(dat,measurevar="AfterCricketBar", groupvars="Group")
cut4$BeforeAfter <-c("1","1")
cut4$Group <-c("Intellectual","Social")
names(cut4)[names(cut4)=="AfterCricketBar"] <- "Rating"

cut5 <- rbind(cut1,cut2)
cut6 <- rbind(cut3,cut4)

cut5$BeforeAfter <- as.factor(cut5$BeforeAfter)
cut5$Group <- as.factor(cut5$Group)
cut6$BeforeAfter <- as.factor(cut6$BeforeAfter)
cut6$Group <- as.factor(cut6$Group)


# Bar charts
BarChart1 <- ggplot(cut5, aes(x=Group, y=Rating, fill=BeforeAfter)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), size=.3, width=.2, position=position_dodge(.9)) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c("#005ea7", "#a74900"), name="", breaks=c("0", "1"), labels=c("Before", "After")) +
  ggtitle("Rating before and after for Cooked cricket") +
  scale_y_continuous(limits=c(0, 10), breaks=0:10*2) +
  theme_bw() +
  theme(legend.position=c(.15, .87))


BarChart2 <- ggplot(cut6, aes(x=Group, y=Rating, fill=BeforeAfter)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3, ) +      # Thinner lines
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c("#005ea7", "#a74900"), name="", breaks=c("0", "1"), labels=c("Before", "After")) +
  ggtitle("Rating before and after for Cricket flour bar") +
  scale_y_continuous(limits=c(0, 10), breaks=0:10*2) +   
  theme_bw()+
  theme(legend.position=c(.15, .87))

multiplot(BarChart1, BarChart2, cols=2)