###############################################################################
######################### CET 521/IND E 546 Project ###########################
###############################################################################

#Libraries 
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)  
library(ggplot2)
library(xtable)
library(rsq)
library(MASS)
library(gt)
library(faraway)
library(som)  
library(factoextra)
library(reshape2)


################################################################################
############################ Reading CSV Files #################################
################################################################################

Operating_Expense_2020 <- read.csv("Operating Expenses_2020.csv", 
                                   na.strings=c("", "NA"), 
                                   header=TRUE)

Operating_Expense_2018 <- read.csv("Operating Expenses_2018(New).csv", 
                                   na.strings=c("", "NA"), 
                                   header=TRUE)

#Clean up data and remove NAs
Operating_Expense_2020 = Operating_Expense_2020[c('Agency', 'Legacy.NTD.ID', 'City', 'Total')]
Operating_Expense_2018 = Operating_Expense_2018[c('Agency', 'Legacy.NTD.ID','City', 'Total')]

################################################################################

Capital_Expense_2018 <- read.csv("2018_Capex.csv", 
                                 na.strings=c("", "NA"), 
                                 header=TRUE)

Capital_Expense_2020 <- read.csv("2020_Capex.csv", 
                                 na.strings=c("", "NA"), 
                                 header=TRUE)

colnames(Capital_Expense_2018)[1] <- "Agency"
colnames(Capital_Expense_2020)[1] <- "Agency"
Capital_Expense_2018 = Capital_Expense_2018[c('Agency', 'Legacy.NTD.ID','City', 'Total')]
Capital_Expense_2020 = Capital_Expense_2020[c('Agency', 'Legacy.NTD.ID','City', 'Total')]
################################################################################

Capital_Funding_Sources_2018 <- read.csv("2018 Overview Capital.csv", 
                                         na.strings=c("", "NA"), 
                                         header=TRUE)



Capital_Funding_Sources_2020 <- read.csv("2020_Capital_Funding.csv", 
                                         na.strings=c("", "NA"), 
                                         header=TRUE)

colnames(Capital_Funding_Sources_2018)[1] <- "Agency"
colnames(Capital_Funding_Sources_2018)[16] <- "State Fund"
colnames(Capital_Funding_Sources_2020)[1] <- "Agency"
colnames(Capital_Funding_Sources_2020)[16] <- "State Fund"
Capital_Funding_Sources_2018 = Capital_Funding_Sources_2018[c('Agency', 'Legacy.NTD.ID','City', 'Local', 'State Fund', 'Federal', 'Total')]
Capital_Funding_Sources_2020 = Capital_Funding_Sources_2020[c('Agency', 'Legacy.NTD.ID','City', 'Local', 'State Fund', 'Federal', 'Total')]
################################################################################

Operating_Funding_Sources_2018 <- read.csv("2018 Overview Operating.csv", 
                                           na.strings=c("", "NA"), 
                                           header=TRUE)

Operating_Funding_Sources_2020 <- read.csv("2020_Operating_Funding.csv", 
                                           na.strings=c("", "NA"), 
                                           header=TRUE)

colnames(Operating_Funding_Sources_2018)[1] <- "Agency"
colnames(Operating_Funding_Sources_2018)[16] <- "State Fund"
colnames(Operating_Funding_Sources_2020)[1] <- "Agency"
colnames(Operating_Funding_Sources_2020)[16] <- "State Fund"
Operating_Funding_Sources_2018 = Operating_Funding_Sources_2018[c('Agency', 'Legacy.NTD.ID', 'City', 'Local', 'State Fund', 'Federal', 'Total')]
Operating_Funding_Sources_2020 = Operating_Funding_Sources_2020[c('Agency', 'Legacy.NTD.ID','City', 'Local', 'State Fund', 'Federal', 'Total')]
################################################################################

Monthly_Ridership <- read.csv("Monthly Ridership.csv", 
                              na.strings=c("", "NA"), 
                              header=TRUE)


Monthly_Ridership = Monthly_Ridership[, -c(1,4:201)]
Monthly_Ridership = Monthly_Ridership[, -c(15:26)]
Monthly_Ridership = Monthly_Ridership[, -c(27:38)]
names(Monthly_Ridership)[names(Monthly_Ridership) == 'X4.digit.NTD.ID'] <- 'Legacy.NTD.ID'

################################################################################
################################## Functions ###################################
################################################################################

#creating function that will select only these agencies we want 
selected.agencies <- function(a){
  result = a[(
    a$Legacy.NTD.ID == '2008'|
      a$Legacy.NTD.ID == '2080'|
      a$Legacy.NTD.ID == '9154'|
      a$Legacy.NTD.ID == '0001'|
      a$Legacy.NTD.ID == '3030'|
      a$Legacy.NTD.ID == '5066'|
      a$Legacy.NTD.ID == '6008'|
      a$Legacy.NTD.ID == '1003'|
      a$Legacy.NTD.ID == '3019'|
      a$Legacy.NTD.ID == '3034'|
      a$Legacy.NTD.ID == '9036'|
      a$Legacy.NTD.ID == '8006'|
      a$Legacy.NTD.ID == '4034'|
      a$Legacy.NTD.ID == '2078'|
      a$Legacy.NTD.ID == '8001'|
      a$Legacy.NTD.ID == '2188'|
      a$Legacy.NTD.ID == '6056'|
      a$Legacy.NTD.ID == '5118'|
      a$Legacy.NTD.ID == '2100'|
      a$Legacy.NTD.ID == '9015'|
      a$Legacy.NTD.ID == '0008'|
      a$Legacy.NTD.ID == '3022'|
      a$Legacy.NTD.ID == '5027'|
      a$Legacy.NTD.ID == '4022'|
      a$Legacy.NTD.ID == '6011'|
      a$Legacy.NTD.ID == '9002'|
      a$Legacy.NTD.ID == '9014'|
      a$Legacy.NTD.ID == '9026'|
      a$Legacy.NTD.ID == '6048'|
      a$Legacy.NTD.ID == '9045'|
      a$Legacy.NTD.ID == '9013'|
      a$Legacy.NTD.ID == '4035'|
      a$Legacy.NTD.ID == '9003'|
      a$Legacy.NTD.ID == '9032'|
      a$Legacy.NTD.ID == '4029'|
      a$Legacy.NTD.ID == '7006'|
      a$Legacy.NTD.ID == '5015'|
      a$Legacy.NTD.ID == '5008'|
      a$Legacy.NTD.ID == '4008'|
      a$Legacy.NTD.ID == '0040'|
      a$Legacy.NTD.ID == '2004'|
      a$Legacy.NTD.ID == '2076'|
      a$Legacy.NTD.ID == '5119'|
      a$Legacy.NTD.ID == '3051'|
      a$Legacy.NTD.ID == '2098'|
      a$Legacy.NTD.ID == '2002'|
      a$Legacy.NTD.ID == '9019'|
      a$Legacy.NTD.ID == '9023'|
      a$Legacy.NTD.ID == '2082'|
      a$Legacy.NTD.ID == '0035'
  ),]
  return(result)
}
################################################################################
#change to numerical value Expenses Table
ToNumericExpense <- function(a){
  a$Total = str_replace_all(a$Total, "[^[:alnum:]]", "" )
  a$Total = as.numeric(a$Total)
  return(a)
}
################################################################################
#change to numerical value Funding Tables
ToNumericFunding <- function(a){
  a$Local = str_replace_all(a$Local, "[^[:alnum:]]", "" )
  a$`State Fund` = str_replace_all(a$`State Fund`, "[^[:alnum:]]", "" )
  a$Federal = str_replace_all(a$Federal, "[^[:alnum:]]", "" )
  a$Total = str_replace_all(a$Total, "[^[:alnum:]]", "" )
  a$Local = as.numeric(a$Local)
  a$`State Fund` = as.numeric(a$`State Fund`)
  a$Federal = as.numeric(a$Federal)
  a$Total = as.numeric(a$Total)
  return(a)
}
####################################################################################################################################

#Creating New Dataframes with the Selected Agencies
CE18 = selected.agencies(Capital_Expense_2018)
CE20 = selected.agencies(Capital_Expense_2020)
CFS18 = selected.agencies(Capital_Funding_Sources_2018)
CFS20 = selected.agencies(Capital_Funding_Sources_2020)
OPE18 = selected.agencies(Operating_Expense_2018)
OPE20 = selected.agencies(Operating_Expense_2020)
OPFS18 = selected.agencies(Operating_Funding_Sources_2018)
OPFS20 = selected.agencies(Operating_Funding_Sources_2020)

CE18 = drop_na(CE18)
CE20 = drop_na(CE20)
CFS18 = drop_na(CFS18)
CFS20 = drop_na(CFS20)
OPE18 = drop_na(OPE18)
OPE20 = drop_na(OPE20)
OPFS18 = drop_na(OPFS18)
OPFS20 = drop_na(OPFS20)

#ToNumeric
CE18 = ToNumericExpense(CE18)
CE20 = ToNumericExpense(CE20)
CFS18 = ToNumericFunding(CFS18)
CFS20 = ToNumericFunding(CFS20)
OPE18 = ToNumericExpense(OPE18)
OPE20 = ToNumericExpense(OPE20)
OPFS18 = ToNumericFunding(OPFS18)
OPFS20 = ToNumericFunding(OPFS20)


#Monthly Ridership
MR = selected.agencies(Monthly_Ridership)
MR = drop_na(MR)
for (i in 3:ncol(MR)) {
  MR[,i] = as.numeric(gsub(",","",MR[,i]))
}
MR = ddply(MR,"Agency",numcolwise(sum))

MR <- transform(MR, Total_Ridership_2018 = MR$JAN18 + MR$FEB18 + MR$MAR18 + MR$APR18 + MR$MAY18 + 
                  MR$JUN18 + MR$JUL18 + MR$AUG18 + MR$SEP18 + MR$OCT18 + MR$NOV18 + MR$DEC18)
MR <- transform(MR, Total_Ridership_2020 = MR$JAN20 + MR$FEB20 + MR$MAR20 + MR$APR20 + MR$MAY20 + 
                  MR$JUN20 + MR$JUL20 + MR$AUG20 + MR$SEP20 + MR$OCT20 + MR$NOV20 + MR$DEC20)
################################################################################

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
################################################################################
#################################### Graphs ####################################
################################################################################
histoExpense = data.frame(Name = 'CE18', CE18[CE18$Legacy.NTD.ID == '0001', ])
histoExpense[nrow(histoExpense) + 1,] <- c('CE20',CE20[CE20$Legacy.NTD.ID == '0001', ])
histoExpense[nrow(histoExpense) + 1,] <- c('OPE18',OPE18[OPE18$Legacy.NTD.ID == '0001', ])
histoExpense[nrow(histoExpense) + 1,] <- c('OPE20',OPE20[OPE20$Legacy.NTD.ID == '0001', ])

histoFund = data.frame(Name = 'CFS18', CFS18[CFS18$Legacy.NTD.ID == '0001', ])
histoFund[nrow(histoFund) + 1,] <- c('CFS20',CFS20[CFS20$Legacy.NTD.ID == '0001', ])
histoFund[nrow(histoFund) + 1,] <- c('OPFS18',OPFS18[OPFS18$Legacy.NTD.ID == '0001', ])
histoFund[nrow(histoFund) + 1,] <- c('OPFS20',OPFS20[OPFS20$Legacy.NTD.ID == '0001', ])



histoExpense = histoExpense[c('Name','Total')]
histoFund = histoFund[c('Name', 'Local', 'State.Fund', 'Federal', 'Total')]
Total = data.frame(c('Cap18', 'Cap20', 'Oper18', 'Oper20' ), histoExpense[,2], histoFund[,5])
colnames(Total) = c('Variable', 'Expenses', 'Funding')

dfm <- melt(Total[,c('Variable','Expenses','Funding')],id.vars = 1)

ggplot(dfm, aes(x = Variable, y = value))+
  geom_bar(aes(fill = variable), stat="identity", position = "dodge")
############################ Summary of Data ###################################
MR18 = MR[,27]
MR20= MR[,28]
summary(CE18['Total'])
summary(CE20['Total'])
summary(OPE18['Total'])
summary(OPE20['Total'])
summary(CFS18['Total'])
summary(CFS20['Total'])
summary(OPFS18['Total'])
summary(OPFS20['Total'])
summary(MR18)
summary(MR20)

######################### Heat Map for Correlations ############################
#Data for 2020
Exp20 = CE20[,4] + OPE20[,4]
FundLoc20 = CFS20[,4] + OPFS20[,4]
FundState20 = CFS20[,5] + OPFS20[,5]
FundFed20= CFS20[,6] + OPFS20[,6]
MR20= MR[,27]

#Data for 2018
Exp18 = CE18[,4] + OPE18[,4]
FundLoc18 = CFS18[,4] + OPFS18[,4]
FundState18 = CFS18[,5] + OPFS18[,5]
FundFed18 = CFS18[,6] + OPFS18[,6]
MR18 = MR[,26]

dfCorAll = data.frame(Exp18, FundLoc18, FundState18, FundFed18, MR18,
                      Exp20, FundLoc20, FundState20, FundFed20, MR20)

cormat <- round(cor(dfCorAll),2)
melted_cormat <- melt(cormat)
dev.off()
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#Add numbers to boxes
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


################################################################################
#################################### Models ####################################
################################################################################

#Linear Model showing the capital and operating expenses/funding compared to the 
#monthly ridership

#Data for 2020
x = CE20[,4] + OPE20[,4]
x1 = CFS20[,4] + OPFS20[,4]
x2 = CFS20[,5] + OPFS20[,5]
x3= CFS20[,6] + OPFS20[,6]
x4= MR[,27]

#Data for 2018
y = CE18[,4] + OPE18[,4]
y1 = CFS18[,4] + OPFS18[,4]
y2 = CFS18[,5] + OPFS18[,5]
y3= CFS18[,6] + OPFS18[,6]
y4= MR[,26]

Expenses = data.frame(CE18,CE20,OPE18,OPE20)
FundSource = data.frame((OPFS18), (OPFS20), (CFS18), (CFS20))
Ridership = data.frame(MR)

AllData = data.frame(Expenses,FundSource,Ridership)

#2020
LR1<-lm(x~ x1+x2+x3+x4,data=AllData)
summary(LR1)

#2018
LR2<-lm(y~ y1+y2+y3+y4,data=AllData)
summary(LR2)


xtable(LR1)
xtable(LR2)
#plot 4 different graphs that shows analysis
par(mfrow=c(2,2))
plot(LR1)
plot(LR2)

#Results LR1(2020)
pvalue = round(summary(LR1)$coefficients[,4], digits = 3)
c = 1
for (i in pvalue) {
  if(i < 0.001){
    pv = 0.001
    pvalue[c] = paste('<', pv, sep = ' ')
  }
  c = c + 1
}
Tabledf = data.frame(Variables = c('Intercept', 'Local Funding', 
                                   'State Funding', 'Federal Funding', 'Ridership' ),
                     round(coef(summary(LR1)), digits = 3), pvalue)
colnames(Tabledf) = c('Variables', 'Estimate', 'Std. Error', 't-Value', 'Remove', 'p-value')
drops <- c('Remove')
Tabledf = Tabledf[ , !(names(Tabledf) %in% drops)]
gt_tbl = gt(Tabledf)
gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = "Top 50 Agencies Funding/Expenses and Ridership for 2020",
    subtitle = "Linear Regression Model"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  )
gt_tbl

#Results LR2(2018)
pvalue2 = round(summary(LR2)$coefficients[,4], digits = 3)
c2 = 1
for (i in pvalue2) {
  if(i < 0.001){
    pv = 0.001
    pvalue2[c2] = paste('<', pv, sep = ' ')
  }
  c2 = c2 + 1
}
#Results Table  
Tabledf2 = data.frame(Variables = c('Intercept', 'Local Funding', 
                                    'State Funding', 'Federal Funding', 'Ridership' ),
                      round(coef(summary(LR2)), digits = 3), pvalue2)
colnames(Tabledf2) = c('Variables', 'Estimate', 'Std. Error', 't-Value', 'Remove', 'p-value')
drops2 <- c('Remove')
Tabledf2 = Tabledf2[ , !(names(Tabledf2) %in% drops2)]
gt_tbl2 = gt(Tabledf2)
gt_tbl2 <- 
  gt_tbl2 %>%
  tab_header(
    title = "Top 50 Agencies Funding/Expenses and Ridership for 2018",
    subtitle = "Linear Regression Model"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  )
gt_tbl2
############################## Cluster Analysis ################################
CFS18.2 = CFS18[c('Agency', 'Legacy.NTD.ID', 'City', 'Total')]
CFS20.2 = CFS20[c('Agency', 'Legacy.NTD.ID', 'City', 'Total')]
OPFS18.2 = OPFS18[c('Agency', 'Legacy.NTD.ID', 'City', 'Total')]
OPFS20.2 = OPFS20[c('Agency', 'Legacy.NTD.ID', 'City', 'Total')]
MR2018 = MR[,26]
MR2020 = MR[,27]

AllTotals = data.frame(CE18$Total, CE20$Total, OPE18$Total, OPE20$Total
                       , CFS18.2$Total, CFS20.2$Total, OPFS18.2$Total, OPFS20.2$Total, 
                       MR2018, MR2020)

AllTotals.norm<-normalize(AllTotals[1:10])  
data.frame(AllTotals.norm) 
colnames(AllTotals.norm)<-c("CE18 Total","CE20 Total","OPE18 Total","OPE20 Total",
                            "CFS18 Total","CFS20 Total","OPFS18 Total","OPFS20 Total", 
                            "MR2018", "MR2020")

################################################################################
d<-dist(AllTotals.norm, method="euclidean") # distance matrix

#Complete linkage (Chose this one)
fit.comp <- hclust(d, method= "complete") 
summary(fit.comp)
plot(fit.comp,plot=NULL, main="Cluster Analysis: Complete Linkage of Class Patterns",xlab="Complete linkage", hang=-1) 
#to see plot of distance between each sequence of linkage
plot(fit.comp$height)

#3 cluster groups 
groups2 <- cutree(fit.comp, k=2) 
table(groups2)
rect.hclust(fit.comp, k=3, border="red")

#4 cluster groups
groups2 <- cutree(fit.comp, k=4) 
table(groups2)
rect.hclust(fit.comp, k=4, border="purple")

fviz_cluster(list(data = AllTotals, cluster = groups2))

################################################################################
#Two cluster k-means
AllTotals_kmeans<-kmeans(AllTotals.norm,center=2)
table(AllTotals_kmeans$cluster) #number of samples in each cluster

summary(AllTotals_kmeans)

colnames(AllTotals_kmeans$centers)<-c("CE18 Total","CE20 Total","OPE18 Total","OPE20 Total",
                                      "CFS18 Total","CFS20 Total","OPFS18 Total","OPFS20 Total", 
                                      "MR2018", "MR2020")

par(mfrow=c(1,1))
Group1<-AllTotals_kmeans$centers[1,]
Group2<-AllTotals_kmeans$centers[2,]
plot(Group1,Group2, ylim = c (-3,3), xlim=c(-3,3),type="n",xlab="Group 1",ylab="Group2")
text(Group1,Group2, labels=colnames(AllTotals.norm))

Cluster3<-AllTotals_kmeans$cluster
k3AllTotals<-as.data.frame(cbind(AllTotals, Cluster3))

##check data
k3AllTotals[1:5,]

fviz_cluster(list(data = AllTotals, cluster = Cluster3))

wss <- (nrow(AllTotals.norm)-1)*sum(apply(AllTotals.norm,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(AllTotals.norm, centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
