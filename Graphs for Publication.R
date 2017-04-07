
his.combined <- read.csv("//cdc.gov/private/L330/ykf1/New folder/Data Quality/Paper/his.combined.csv",
                         stringsAsFactors = F)

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

#---------------------------------------------------------------------------------------------#

# Figure 1A (HIS, female 18-59, AMR)

final.digit = 0:9
Freq = c(15, 0, 0, 2, 2, 5, 1, 3, 4, 3)
t3 <- data.frame(cbind(final.digit, Freq))
t3$final.digit <- as.factor(t3$final.digit)

p1 <- ggplot(t3, aes(x=final.digit, y=Freq)) + 
  geom_bar(stat="identity") 

plot1 <- p1 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (female 18-59, other conditions)") +
  ylab("Count") +
  ggtitle("Occurances of Final Digit (HIS)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(5, 20, 5)) + 
  expand_limits(y=21) +
  annotate("text", x=2, y=20, label = "p < .0001", size=5)

# Figure 1B (EWARN)

final.digit = 0:9
Freq = c(14, 5, 2, 5, 1, 10, 2, 5, 4, 3)
t1 <- data.frame(cbind(final.digit, Freq))
t1$final.digit <- as.factor(t1$final.digit)

p2 <- ggplot(t1, aes(x=final.digit, y=Freq)) + 
  geom_bar(stat="identity") 

plot2 <- p2 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (5 and over, other cases)") +
  ylab("Count") +
  ggtitle("Occurances of Final Digit (EWARN)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(5, 20, 5)) + 
  expand_limits(y=21) +
  annotate("text", x=2, y=20, label = "p = .0008", size=5)

# Figure 1C (HIS, male 18-59, AMR)

final.digit = 0:9
Freq = c(12, 1, 2, 2, 3, 2, 3, 4, 2, 4)
t4 <- data.frame(cbind(final.digit, Freq))
t4$final.digit <- as.factor(t4$final.digit)

p3 <- ggplot(t4, aes(x=final.digit, y=Freq)) + 
  geom_bar(stat="identity") 

plot3 <- p3 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (male 18-59, other conditions)") +
  ylab("Count") +
  ggtitle("Occurances of Final Digit (HIS)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(5, 20, 5)) + 
  expand_limits(y=21) +
  annotate("text", x=2, y=20, label = "p = .0027", size=5)

# Figure 1D (EWARN)

final.digit = 0:9
Freq = c(1, 4, 5, 8, 0, 1, 4, 1, 5, 21)
t2 <- data.frame(cbind(final.digit, Freq))
t2$final.digit <- as.factor(t2$final.digit)

p4 <- ggplot(t2, aes(x=final.digit, y=Freq)) + 
  geom_bar(stat="identity") 

plot4 <- p4 +
  geom_bar(stat="identity", fill="grey16") +
  xlab("Final Digit (under 5, other cases)") +
  ylab("Count") +
  ggtitle("Occurances of Final Digit (EWARN)") +
  theme_base() +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(5, 20, 5)) + 
  expand_limits(y=21) +
  annotate("text", x=2, y=20, label = "p < .0001", size=5)


# Saving Figure 1

png('C:/Data-Quality/Figure1.png', 8, 6.5, units='in', res=300)

multiplot(plot1, plot3, plot2, plot4, cols=2)

dev.off()

#---------------------------------------------------------------------------------------------#

# Figure 4A

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

# Figure 4B

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

# Figure 4C

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

# Figure 4D

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

# Saving Figure 4

png('C:/Data-Quality/Figure4.png', 8, 6.5, units='in', res=300)

multiplot(plot3, plot4, plot1, plot2, cols=2)

dev.off()

#---------------------------------------------------------------------------------------------#

# Figure 5A

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


# Figure 5B

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

# Figure 5C

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

# Figure 5D

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

# Saving Figure 5

png('C:/Data-Quality/Figure5.png', 8, 6.5, units='in', res=300)

multiplot(plot5, plot6, plot7, plot8, cols=2)

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