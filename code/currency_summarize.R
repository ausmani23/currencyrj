########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(data.table)
require(tidyr)
require(ggplot2)
require(rprojroot)

homedir<-find_root(
  criterion=has_file('racialequality.Rproj')
)
datadir<-file.path(
  homedir,"data"
)
filesdir<-file.path(
  homedir,"files"
)
outputdir<-file.path(
  homedir,"output"
)
codedir<-file.path(
  homedir,"code"
)

setwd(codedir); dir()
source('getTaxRates.R')

########################################################
########################################################

#load the SCF data
setwd(datadir); dir()
require(haven)
fulldf<-read_dta('rscfp2019.dta') %>%
  data.table
names(fulldf)

oldnames<-c(
  'networth',
  'income',
  'racecl4',
  'edcl',
  'wgt'
)
newnames<-c(
  'networth',
  'income',
  'race',
  'educ',
  'weight'
)

fulldf <- fulldf[,oldnames,with=F]
names(fulldf)<-newnames

# #recodings
# tmplevels <- c(1:4)
# tmplabels <- c('white','black','hispanic','other')
# fulldf$race <- factor(fulldf$race,tmplevels,tmplabels)

#for setup, we want to know people's location in the distribution
require(spatstat)
tmpf<-ewcdf(fulldf$networth,fulldf$weight)
fulldf$networth_q <- 100 * tmpf(fulldf$networth)
#also generate race-specific percentile
tmpfunction<-function(x,w) {
  tmpf<-ewcdf(x,w)
  100 * tmpf(x)
}
fulldf[
  ,
  networth_q_race := tmpfunction(networth,weight)
  ,
  by=c('race')
]

#also order everyone from poorest to richest
fulldf$rank<-rank(fulldf$networth,ties.method='random')
#also rank within race
fulldf[
  ,
  rank_race := rank(networth,ties.method='random')
  ,
  by=c('race')
]

########################################################
########################################################

#DESCRIPTIVE STATISTICS 
#descriptive statistics for the introduction

#median wealth, by race
medians<-fulldf[
  ,
  weighted.median(networth,weight)
  ,
  by=c('race')
]
print(medians)

#mean wealth, by race
means<-fulldf[
  ,
  weighted.mean(networth,weight)
  ,
  by=c('race')
]
print(means)

#bw disparity, ratio
medians[race==1,'V1']/medians[race==2,'V1']
means[race==1,'V1']/means[race==2,'V1']

#bw disparity, absolute
medians[race==1,'V1'] - medians[race==2,'V1']
means[race==1,'V1'] - means[race==2,'V1']

#share of wealth held by top 10%
100 * fulldf[networth_q>=90,sum(networth*weight)]/
  fulldf[,sum(networth*weight)] 

#share of wealth held by wealthiest quintile
100 * fulldf[networth_q>=80,sum(networth*weight)]/
  fulldf[,sum(networth*weight)] 

#share of wealth held by bottom 50%
100 * fulldf[networth_q<=50,sum(networth*weight)]/
  fulldf[,sum(networth*weight)] 

#black americans
100 * fulldf[networth_q_race>=90 & race==2,sum(networth*weight)]/
  fulldf[race==2,sum(networth*weight)] 
100 * fulldf[networth_q_race>=80 & race==2,sum(networth*weight)]/
  fulldf[race==2,sum(networth*weight)] 
100 * fulldf[networth_q_race<=50 & race==2,sum(networth*weight)]/
  fulldf[race==2,sum(networth*weight)] 
fulldf[networth_q_race<=50 & race==2,sum(networth*weight)]


#closing the gap at the median 
##giving every black family below white median income the white median income
fulldf[
  race==2 & networth < unlist(medians[race==1,'V1']),
  #difference between white median income and their networth * weight
  sum( (unlist(medians[race==1,'V1']) - networth) * weight )
] 
##giving every black family above/equal to black median and below white median
fulldf[
  race==2 & 
    networth>=unlist(medians[race==2,'V1']) & 
    networth<unlist(medians[race==1,'V1'])
  ,
  sum( (unlist(medians[race==1,'V1']) - networth) * weight )
] 

#closing the gap at the mean
fulldf[race==2,sum(weight)] * 
  (unlist(means[race==1,'V1']) - unlist(means[race==2,'V1']))

#perfect equality
fulldf[,sum(weight*networth)]/
  fulldf[,sum(weight)]

#kleptocratic equality
blackshare<-fulldf[race==2,sum(weight)]/fulldf[,sum(weight)]
whiteshare<-fulldf[race==1,sum(weight)]/fulldf[,sum(weight)]
fulldf[,sum(weight*networth)]*blackshare
fulldf[,sum(weight*networth)]*whiteshare

########################################################
########################################################

#FIGURE 1
#align the curves

plotdf<-fulldf[race%in%c(1,2),]

#add equality
tmpdf<-data.frame(
  networth_q_race=1:100,
  networth_q=1:100,
  race=3
)
plotdf<-rbind.fill(
  plotdf,
  tmpdf
) %>% data.table

plotdf$race<-factor(
  plotdf$race,
  levels=c(3,2,1),
  labels=c('Equality','Black','White')
)
tmpcolors<-c('black','blue','red')
tmptypes<-c('dashed','solid','solid')
names(tmptypes)<-names(tmpcolors)<-levels(plotdf$race)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=networth_q_race,
    y=networth_q,
    group=race,
    color=race,
    linetype=race
  )
) +
  geom_line() +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  scale_linetype_manual(
    name="",
    values=tmptypes
  ) +
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  xlab("\nRace-Specific Wealth Quantile") +
  ylab("Overall Wealth Quantile\n")

setwd(outputdir); dir()
ggsave(
  plot=g.tmp,
  'fig_alignthecurves.png',
  width=6,
  height=4
)

#a black household at the 25th percentile of black
plotdf[race=='Black' & round(networth_q_race)==25,mean(networth_q)]
#a white household at the 25th percentile of white
plotdf[race=='White' & round(networth_q_race)==25,mean(networth_q)]

#numbers for Musk, Bezos, Smith, Steward, etc.
#https://www.forbes.com/forbes-400/

#when do white people have negative net assets?
fulldf[race==1 & networth<0,max(networth_q_race)] #around the 8th percentile

########################################################
########################################################
