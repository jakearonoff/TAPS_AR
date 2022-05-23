library("haven")
library("tidyverse")
library("ggpubr")
library(gamm4)


setwd("~/Desktop/TAPS paper/")

TAPS <- read_dta("file_path.dta", encoding = "latin1")

TAPS <- rename(TAPS, id = idssnY234567890,
               age = idage_beckyY234567890,
               male = idmaleY234567890,
               bmi = ianbmiY234567890,
               sum2skin = iansumsfY234567890,
               sum4skin = iansumsk4Y234567890,
               biskin = ianbicepY234567890,
               triskin = iantricepY234567890,
               subskin = ianscapularY234567890,
               supraskin = iansuprailY234567890,
               anyerror = ianbadanthY234567890)

# select variables for analysis
d <- dplyr::select(TAPS, id, age, male, bmi, sum2skin, sum4skin, biskin, triskin, subskin, supraskin, anyerror)
rm(TAPS)

# only include ages 0-18
# filter out any recorded measurement error
# filter out some individuals for which sex was not consistently coded throughout panel study
d <- filter(d, age <= 18, anyerror==0 & id != 124 & id!=328 & id != 536 
            & id != 1326 & id != 1569 & id!=2228 & id!=1891 & id!=2231 & id!=2234)

# take out measurement error variable 
d <- dplyr::select(d, id, age, male, bmi, sum2skin, sum4skin, biskin, triskin, subskin, supraskin)

# remove missing data
d <- na.omit(d)

# remove unrealistically high measures 
d <- filter(d, supraskin < 50, subskin < 35, biskin < 20) # 7 obs removed 

# code sex variable 
d$Sex <- ifelse(d$male == 1, "Boys", "Girls")

# turn to factor for use in ggplot
d$Sex <- as.factor(d$Sex)


#################################################################################
## Plotting 
#################################################################################

bmiplot <- ggplot(d, aes(x = age, y = bmi, color = Sex)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  labs(x = "Age", y = "BMI") + 
  scale_color_discrete(name="") + theme_bw() 
bmiplot

biskinplot <- ggplot(d, aes(x = age, y = biskin, color = Sex)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  labs(y = "Biceps") + 
  scale_color_discrete(name="") + theme_bw() + theme(axis.title.x=element_blank(),
                                                     axis.ticks.x=element_blank())
triskinplot <- ggplot(d, aes(x = age, y = triskin, color = Sex)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  labs(y = "Triceps") + 
  scale_color_discrete(name="") + theme_bw() + theme(axis.title.x=element_blank(),
                                                     axis.ticks.x=element_blank(),
                                                     legend.position = "none")
subskinplot <- ggplot(d, aes(x = age, y = subskin, color = Sex)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  labs(x = "Age", y = "Subscapular") + 
  scale_color_discrete(name="") + theme_bw() + theme(legend.position = "none")
supraskinplot <- ggplot(d, aes(x = age, y = supraskin, color = Sex)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  labs(x = "Age", y = "Suprailliac") + 
  scale_color_discrete(name="") + theme_bw() 

ggarrange(triskinplot, biskinplot, subskinplot, supraskinplot, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,
          widths = c(.45, .55),
          heights = c(.49, .51))


##### all ages plots (same as above, but no age filter applied)
## alpha argument fades datapoints, making it easier to see lines
bmiplot <- ggplot(d, aes(x = age, y = bmi, color = Sex)) + geom_point(alpha = 0.05) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  labs(x = "Age", y = "BMI") + 
  scale_color_discrete(name="") + theme_bw() 
bmiplot

sum4skinplot <- ggplot(d, aes(x = age, y = sum4skin, color = Sex)) + geom_point(alpha = 0.05) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  labs(x = "Age", y = "Skinfold Sum") + 
  scale_color_discrete(name="") + theme_bw() 
sum4skinplot



############################################################################################################
############ NHANES  
############################################################################################################

# load in demographics (age, etc.)
demo <- read_xpt(
  "file_path.XPT",
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
)

adiposity <- read_xpt(
  "file_path.XPT",
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
)

n <- merge(demo, adiposity, by = "SEQN")

n <- rename(n,
            gender = RIAGENDR,
            age = RIDAGEYR,
            triskin = BMXTRI,
            triskincomment = BMITRI,
            subskin = BMXSUB,
            subskincomment = BMISUB)

n <- dplyr::select(n, gender, age, triskin, triskincomment, subskin, subskincomment)

rm(adiposity, demo)

n <- filter(n, age <= 18 & is.na(triskincomment) & is.na(subskincomment)) # taking out skinfold measures with comment regarding measurement problem 

n$male <- ifelse(n$gender == 1, 1, 0)
n$sum2skin <- n$triskin + n$subskin
n <- dplyr::select(n, male, age, triskin, subskin, sum2skin)
n <- na.omit(n) # 3650 individuals with complete data

n$Sex <- ifelse(n$male == 1, "Boys", "Girls")
n$Sex <- as.factor(n$Sex)

nn <- dplyr::select(n, age, Sex, sum2skin)
nn$data <- ifelse(nn$Sex == "Boys", "US Boys", "US Girls")
nn <- dplyr::select(nn, age, sum2skin, data)

##### combining with Tsimane' data
dd <- dplyr::select(d, age, Sex, sum2skin)
dd$data <- ifelse(dd$Sex == "Boys", "Tsimane' Boys", "Tsimane' Girls")
dd <- dplyr::select(dd, age, sum2skin, data)

combined <- rbind(dd, nn)
combined$Data <- as.factor(combined$data)

combined_plot <- ggplot(combined, aes(x = age, y = sum2skin, group = Data)) + 
  geom_smooth(aes(color = Data, linetype = Data), method = "gam", formula = y ~ s(x, bs = "cr", k = 19), se = F) + 
  scale_color_manual(values=c("brown2", "darkturquoise", "orangered1", "cyan3")) + 
  scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed")) + theme_bw() + labs(x="Age", y="Skinfold Sum") 
combined_plot




############################################################################################################
############ GAMM  
############################################################################################################

dboys <- filter(d, Sex == "Boys") # 789 boys ,  3424 obs 
dgirls <- filter(d, Sex == "Girls") # 762 girls ,  3317 obs 

gammskingirls <- gamm4(sum4skin ~ s(age, bs = "cr", k = 19), data = dgirls, random = ~(1|id))
summary(gammskingirls$gam)

gammbmigirls <- gamm4(bmi ~ s(age, k = 19, bs = "cr"), data = dgirls, random = ~(1|id))
summary(gammbmigirls$gam)


gammskinboys <- gamm4(sum4skin ~ s(age, k = 19, bs = "cr"), data = dboys, random = ~(1|id))
summary(gammskinboys$gam)

gammbmiboys <- gamm4(bmi ~ s(age, k = 19, bs = "cr"), data = dboys, random = ~(1|id))
summary(gammbmiboys$gam)


## predicting adiposity across ages at 0.1 intervals 

age <- seq(0,18,by = 0.1)
pred_data_boys <- as.data.frame(age)
pred_data_boys$predbmi <- predict.gam(gammbmiboys$gam, pred_data_boys)
pred_data_boys$predskin4 <- predict.gam(gammskinboys$gam, pred_data_boys)
minbmiboys <- filter(pred_data_boys, predbmi == min(predbmi))   
View(minbmiboys)  #  5.9 years lowest predicted BMI
minskinboys <- filter(pred_data_boys, predskin4 == min(predskin4))
View(minskinboys) #    7.4 years lowest predicted skinfold sum4


pred_data_girls <- as.data.frame(age)
pred_data_girls$predbmi <- predict.gam(gammbmigirls$gam, pred_data_girls)
pred_data_girls$predskin4 <- predict.gam(gammskingirls$gam, pred_data_girls)
minbmigirls <- filter(pred_data_girls, predbmi == min(predbmi))  
View(minbmigirls) #  5.6 years lowest predicted BMI
minskingirls <- filter(pred_data_girls, predskin4 == min(predskin4))
View(minskingirls) #    6.3 years lowest predicted skinfold sum4
















