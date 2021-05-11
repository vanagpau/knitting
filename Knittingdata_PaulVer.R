# Clear R environment of all variables.
rm(list = ls())

#install.packages(c("tidyverse", "ggplot2", "papaja"))


#Libraries----

# Load packages and import data----
# Load in all relevant packages for script below.
library(tidyverse)
library(ggplot2)
library(papaja)


# Import data-----

# Set working directory to call data into R environment
# Change this however relevant Paul
setwd("~/Etienne project")

# Import data set of 7000+ participants into data frame.
df <- read.csv(file = "Kntiing7000dat1.csv", header = TRUE)


# Data cleaning----

# Removal of unnecessary columns for ease of viewing later on
# set into reduced data frame.
red_df <- df[,-c(2,23,24)]


# Changing column names of the reduced data
# Paul you can change these names as you see fit
# it will make the data frame more readable for you 
# than what comes in from the orignal file.

red_df <- red_df %>%
  rename(Participant = ï..Unique.Response.Number,
         Gender = X71..What.is.your.gender.,
         BDI = BDI..0.9.16.29.63.,
         FMI = FMI..14.56.,
         PSS = PSS..0.13.26.40.,
         STAI = STAI..20.80.,
         #country of birth
         Cofbirth = X72..In.which.country.were.you.born.,
         #Current country
         Ccountry = X73..In.which.country.do.you.currently.live..Which.region.,
         #Date of Birth
         DOB = X74..What.is.the.year.of.your.birth.,
         #frequency of kniiting
         Freq_knit = X76..About.knitting...,
         #length of kniting sessions
         Session_L_Knit = X77..How.long.does.a.typical.knitting.session.last.,
         #when to beging learn to knit
         w_BL_K = X78..When.did.you.learn.to.knit.,
         Length_ofReg_K = X79..How.long.have.you.been.knitting.regularly.,
         CrochetQ = X80..About.crochet...,
         Session_L_Croch = X81..How.long.does.a.typical.crochet.session.last.,
         Age_beg_Croch = X82..At.what.age.did.you.learn.to.crochet.,
         #How long for regular crocheting
         HL_ofReg_C = X83..How.long.have.you.been.crocheting.regularly.,
         Feeling_when_KC = X84..What.is.your.general.experience.when.you.knit.crochet..How.do.you.feel.,
         Purpose_of_KC = X85..For.what.purpose.do.you.knit.crochet.,
         Med_Condtions = X86..Do.you.have.a.diagnosed.condition..If.so..would.you.mind.telling.us.what.,
         #does the participant knit alone
         Knit_alone = X87..Do.you.knit.crochet...)


# Creating a variable to "lookfor" values to remove missing values
look.for <- "#VALUE!"
# Generate data frame with these missing values
missing_BDI <- red_df[red_df$BDI %in% look.for, ]

# Generating new Data frame with the missing values
# for questionnaire measures removed
V_red_df <- red_df[!(red_df$Participant %in% missing_BDI$Participant),]

# Changing BDI scores to numeric 
V_red_df$BDI <- as.numeric(V_red_df$BDI)

# Change all relevant variables to factor variables
factor_cols <- c("Gender", "Knit_alone")
V_red_df[factor_cols] <- lapply(V_red_df[factor_cols], factor)


# Generating subset data frames for males and females for 
# plotting data.
Female_df <- subset(V_red_df, Gender == "Female.")
Male_df <- subset(V_red_df, Gender == "Male.")

# Prefer not to say very few a analysis decision has to be 
# made whether to remove
PNS_Df  <- subset(V_red_df, Gender == "Prefer not to say.")

# Sub-setting the alone, both and solely in group knitters
alone_df <- subset(V_red_df, Knit_alone == "alone")
both_df <- subset(V_red_df,Knit_alone == "both")
IAG_df <- subset(V_red_df,Knit_alone == "in a group")

# Plots for female knitter subset----

# Density plot for the female knitters BDI scores
ggplot(Female_df, aes(x = BDI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Female BDI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for the female knitters BDI scores
ggplot(Female_df, aes(x = FMI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Female FMI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for the female knitters PSS scores
ggplot(Female_df, aes(x = PSS)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the PSS Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for the female knitters FMI scores
ggplot(Female_df, aes(x = FMI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Female FMI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for the female knitters STAI scores
ggplot(Female_df, aes(x = STAI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Female STAI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Plots for male knitter subset----

# Density plot for the male knitters BDI scores
ggplot(Male_df, aes(x = BDI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Male BDI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for the male knitters PSS scores
ggplot(Male_df, aes(x = PSS)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa(base_size = 14)+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Male PSS Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for the male knitters FMI scores
ggplot(Male_df, aes(x = FMI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Male FMI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for the male knitters STAI scores
ggplot(Male_df, aes(x = STAI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the Male STAI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Overlay plot example needs to be greatly improved Paul
# ggplot(V_red_df, aes(x= STAI, fill= Gender, color = Gender))+
#    geom_histogram(aes(y= ..density..), binwidth = 1)+
#   geom_density(alpha=.2, fill="#FF6666")+
#   scale_y_continuous(expand = c(0,0))+scale_x_continuous(expand = c(0,0))+
#   theme_apa()

# Plots for in a group knitters----

# Density plots for in a group (IAG) knitter only subset for each questionnaire score

# Density plot for IAG knitters BDI scores

ggplot(IAG_df, aes(x = BDI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting in a group BDI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for IAG knitters PSS scores
ggplot(IAG_df, aes(x = PSS)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting in a group PSS Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

#Density plot for IAG knitters FMI scores
ggplot(IAG_df, aes(x = FMI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting in a group FMI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

#Density plot for IAG knitters STAI scores
ggplot(IAG_df, aes(x = STAI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting in a group STAI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))



# Plots for both knitters group----
# Density plots for the both alone and in group knitters subset

# Density plot for both knitter subset BDI scores
ggplot(both_df, aes(x = BDI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting both in a group and alone BDI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for both knitter subset PSS scores
ggplot(both_df, aes(x = PSS)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting both in a group and alone PSS Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))


# Density plot for both knitter subset FMI scores
ggplot(both_df, aes(x = FMI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting both in a group and alone FMI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot for both knitter subset STAI scores
ggplot(both_df, aes(x = STAI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting both in a group and alone STAI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))


# Plot for alone knitters----

# Density plots for the alone knitters subset across the questionnaire ratings

# Density plot of the alone knitters BDI scores
ggplot(alone_df, aes(x = BDI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting alone BDI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot of the alone knitters PSS scores
ggplot(alone_df, aes(x = PSS)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting alone PSS Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot of the alone knitters FMI scores
ggplot(alone_df, aes(x = FMI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting alone FMI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))

# Density plot of the alone knitters STAI scores
ggplot(alone_df, aes(x = STAI)) + 
  geom_histogram(aes(y=..density..), binwidth = 1, colour="black", fill="white")+
  theme_apa()+geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Histogram of the knitting alone STAI Scores")+scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))
 
# Note to Paul there is lots that can be done here an this script 
# is objectively inadequate. Fortunately you can have stab a likely just
# taking this as a skeleton.

# Additionally, it might be worth you checking my excel 
# file (i.e errors in some of the formulas in the 
# 'results-for-science-not-fluff-2020-07-07-0939' file that created the export
# file of Kntiing7000dat1 file used in this script) 


