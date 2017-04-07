
his.combined <- read.csv("//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/his.combined.csv")

# Next time use stringsAsFactors = F in read.csv



# Loading packages

library(ggplot2)
library(ggthemes)

# Sourcing Multiplot function

source("Multiplot.R")

his.combined[his.combined == "."] <- NA


# Converting variables from factors to numeric
columns <- names(his.combined)
count_cols <- grep( pattern = "count_", x = columns)

convert_factor <- function(x){
  as.numeric(as.character(x))
}

his.combined[, count_cols] <- lapply( his.combined[, count_cols], convert_factor)

lapply( his.combined[, count_cols], levels)



theme_get()
ls(package:ggthemes)


# EWARN

EWARN.puntland <- read.csv("//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/puntland2013.csv", 
                           stringsAsFactors = F, na.strings = ".")
EWARN.central <- read.csv("//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/central2013.csv", 
                          stringsAsFactors = F, na.strings = ".")
EWARN.somaliland <- read.csv("//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/somaliland2013.csv", 
                             stringsAsFactors = F, na.strings = ".")
EWARN.southern <- read.csv("//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/southern2013.csv", 
                           stringsAsFactors = F, na.strings = ".")





# Plots

# Creating Plot #1
p1 <- ggplot( data = his.combined[his.combined$clinic=="JHAS",], 
              aes( x= week, y = count_M_over60_other)) 
plot1 <- p1 + 
    geom_line(size=1) +
    xlab("Epidemiological Week") +
    ylab("Cases (male over 60, 'other' conditions)") +
    ggtitle("Cases by Week (HIS)") +
    theme_gdocs() + 
    scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50"))

   

#p2 <- ggplot( data = his.combined[his.combined$clinic=="RAF",],
#              aes( x= week, y = count_M_under5_ili)) 

#plot2 <-  p2 +
#  geom_line(size=1) + 
#  xlab("Epidemiological Week") + 
#  ylab("Cases (male under 5, ili conditions)") + 
#  ggtitle("Cases by Week") +
#  theme_gdocs() + 
#  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
#                     labels = c("10", "20", "30", "40", "50"))      

p2 <- ggplot( data = his.combined[his.combined$clinic=="RAF",],
              aes( x= week, y = count_M_18_59_dental_cond))

plot2 <- p2 + 
  geom_line(size=1) + 
  xlab("Epidemiological Week") + 
  ylab("Cases (male 18-59, dental conditions)") + 
  ggtitle("Cases by Week (HIS)") +
  theme_gdocs() + 
  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50"))  

#p4 <- ggplot( data = his.combined[his.combined$clinic=="RAF",],
#              aes( x= week, y = count_F_18_59_dental_cond))

#plot4 <- p4 + 
#  geom_line(size=1) + 
#  xlab("Epidemiological Week") + 
#  ylab("Cases (female 18-59, dental conditions)") + 
  ggtitle("Cases by Week") +
  theme_gdocs() + 
  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50")) 

p3 <- ggplot( data = EWARN.central[EWARN.central$facility=="Banadir Hospital",],
                aes( x= Week, y = shigcasesless5))
  
plot3 <- p3 + 
    geom_line(size=1) + 
    xlab("Epidemiological Week") + 
    ylab("Cases (under 5, shigellosis)") + 
    ggtitle("Cases by Week (EWARN)") +
    theme_gdocs() + 
    scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                       labels = c("10", "20", "30", "40", "50"))    

  
p4 <- ggplot( data = EWARN.central[EWARN.central$facility=="Banadir Hospital",], 
              aes( x= Week, y = wccasesless5)) 

plot4 <- p4 + 
  geom_line(size=1) + 
  xlab("Epidemiological Week") + 
  ylab("Cases (under 5, whooping cough)") + 
  ggtitle("Cases by Week (EWARN)") +
  theme_gdocs() + 
  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50")) 

png('//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/Figure4.png', 8, 6.5, units='in', res=300)

multiplot(plot1, plot2, plot4, plot3, cols=2)

dev.off()

#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#http://stackoverflow.com/questions/14323294/how-to-improve-jagged-line-graph-in-ggplot2 
#http://blog.yhat.com/posts/replicating-five-thirty-eight-in-r.html


p5 <- ggplot( data = his.combined[his.combined$clinic=="MFH",], 
              aes( x= week, y = count_F_5_17_intestinal_worms)) 

plot5 <- p5 + 
  geom_line(size=1) +
  xlab("Epidemiological Week") +
  ylab("Cases (female 5-17, intestinal worms)") +
  theme(axis.title.y = element_text(size = 8)) +
  ggtitle("Cases by Week (HIS)") +
  theme_gdocs() + 
  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50")) #+
  #annotate("text", x=26, y=24, label= "Outlier: week 19", size=5)


p6 <- ggplot( data = his.combined[his.combined$clinic=="JHAS",], 
              aes( x= week, y = count_M_18_59_ear_infection)) 

plot6 <- p6 + 
  geom_line(size=1) +
  xlab("Epidemiological Week") +
  ylab("Cases (male 18-59, ear infection)") +
  theme(axis.title.y = element_text(size = 8)) +
  ggtitle("Cases by Week (HIS)") +
  theme_gdocs() + 
  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50")) #+
  #annotate("text", x=31, y=34, label= "Outlier: week 23", size=5)



p7 <- ggplot( data = EWARN.puntland[EWARN.puntland$facility=="Bossaso MCH U/Y",], 
              aes( x= Week, y = malcases5over)) 

plot7 <- p7 + 
  geom_line(size=1) +
  xlab("Epidemiological Week") +
  ylab("Cases (5 and over, malaria)") + 
  
  ggtitle("Cases by Week (EWARN)") +
  theme_gdocs() + 
  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50")) #+
  #annotate("text", x=18, y=81, label= "Outlier: week 12", size=5)

p8 <- ggplot( data = EWARN.puntland[EWARN.puntland$facility=="Rakko MCH",], 
              aes( x= Week, y = cholcase5over)) 

plot8 <- p8 + 
  geom_line(size=1) +
  xlab("Epidemiological Week") +
  ylab("Cases (5 and over, cholera)") +
  ggtitle("Cases by Week (EWARN)") +
    theme_gdocs() + 
  scale_x_continuous(breaks=c(10, 20, 30, 40, 50), 
                     labels = c("10", "20", "30", "40", "50")) #+
  #annotate("text", x=15, y=7.8, label= "Outlier: week 8", size=5)

png('//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/Figure5.png', 8, 6.5, units='in', res=300)

multiplot(plot5, plot6, plot7, plot8, cols=2)

dev.off()

# digit preference

# plot 9

final.digit = 0:9
Freq = c(14, 5, 2, 5, 1, 10, 2, 5, 4, 3)
t1 <- data.frame(cbind(final.digit, Freq))
t1$final.digit <- as.factor(t1$final.digit)

p9 <- ggplot(t1, aes(x=final.digit, y=Freq)) + 
      geom_bar(stat="identity") 

plot9 <- p9 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (5 and over, other cases)") +
  ylab("Frequency") +
  ggtitle("Occurances of Final Digit (EWARN)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11))

# plot 10

final.digit1 <- data.frame(EWARN.somaliland[EWARN.somaliland$fcode=="SLTO07",]$occasesless5)
names(final.digit1) <- "count"
final.digit <- final.digit1$count - round(final.digit1$count, digits = -1)
final.digit[final.digit < 0] <- 10 + final.digit[final.digit < 0] 
t2 <- data.frame(table(final.digit))

p10 <- ggplot(t2, aes(x=final.digit, y=Freq)) + 
  geom_bar(stat="identity") 

plot10 <- p10 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (under 5, other cases)") +
  ylab("Frequency") +
  ggtitle("Occurances of Final Digit (EWARN)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11))

# Plot 11

count_M_18_59_other 

final.digit = 0:9
Freq = c(15, 0, 0, 2, 2, 5, 1, 3, 4, 3)
t3 <- data.frame(cbind(final.digit, Freq))
t3$final.digit <- as.factor(t3$final.digit)

p11 <- ggplot(t3, aes(x=final.digit, y=Freq)) + 
  geom_bar(stat="identity") 

plot11 <- p11 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (male 18-59, other conditions)") +
  ylab("Frequency") +
  ggtitle("Occurances of Final Digit (HIS)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11))

# Plot 12

count_F_5_17_other

final.digit = 0:9
Freq = c(12, 1, 2, 2, 3, 2, 3, 4, 2, 4)
t4 <- data.frame(cbind(final.digit, Freq))
t4$final.digit <- as.factor(t4$final.digit)

p12 <- ggplot(t4, aes(x=final.digit, y=Freq)) + 
  geom_bar(stat="identity") 

plot12 <- p12 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (female 5-17, other conditions)") +
  ylab("Frequency") +
  ggtitle("Occurances of Final Digit (HIS)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11))
        

png('//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/digit.png', 8, 6.5, units='in', res=300)

multiplot(plot11, plot12, plot9, plot10, cols=2)

dev.off()
































    
theme(axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      plot.title   = element_text(size=25),
      text         = element_text(size=15)
) +
  
#axis.text.x, axis.text.y
+
   
theme(plot.title = element_text(lineheight=5, face="bold"))
theme(element_text(lineheight=5, face="bold")) 







proc sgplot data=his.combined;
where clinic="JHAS";
series x=week y=count_M_over60_other;
run;

proc sgplot data=his.combined;
where clinic="RAF";
series x=week y=count_M_under5_ili;
run;

proc sgplot data=his.combined;
where clinic="RAF";
series x=week y=count_M_18_59_dental_cond;
run;