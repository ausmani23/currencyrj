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

#### ---- PREPARE THE DATASET ---- #####

#load the SCF data
#https://www.federalreserve.gov/econres/files/scfp2022s.zip
setwd(datadir); dir()
require(haven)
fulldf<-read_dta('rscfp2022.dta') %>%
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

#### ---- DESCRIPTIVE STATISTICS ---- #####

#median income, by race
medians<-fulldf[
  ,
  weighted.median(income,weight) %>% round(-3)
  ,
  by=c('race')
]
print(medians)

#mean wealth, by race
means<-fulldf[
  ,
  weighted.mean(income,weight) %>% round(-3)
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

#median wealth, by race
medians<-fulldf[
  ,
  weighted.median(networth,weight) %>% round(-3)
  ,
  by=c('race')
]
print(medians)

#mean wealth, by race
means<-fulldf[
  ,
  weighted.mean(networth,weight) %>% round(-3)
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
#if excluding negative 'wealth'; doesn't really matter..
100 * fulldf[networth_q>=90,sum(networth*weight)]/
  fulldf[networth>0,sum(networth*weight)] 

#share of wealth held by wealthiest quintile
100 * fulldf[networth_q>=80,sum(networth*weight)]/
  fulldf[,sum(networth*weight)] 

#share of wealth held by bottom 50%
100 * fulldf[networth_q<=50,sum(networth*weight)]/
  fulldf[,sum(networth*weight)] 

#black americans top 20%
100 * fulldf[networth_q_race>=80 & race==2,sum(networth*weight)]/
  fulldf[race==2,sum(networth*weight)] 
#bottom 50% have negative.. 
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
round(fulldf[,sum(weight*networth)]/fulldf[,sum(weight)],-3)

########################################################
########################################################

#### ---- ALIGN THE CURVES ---- #####

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
  dpi=1200,
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

#### ---- REPARATIONS  ---- #####

loopdf<-expand.grid(
  #paper considers either flat or progressive redistribution
  transfer=c('flat','progressive'),
  stringsAsFactors=F
)
loopdf$i<-1:nrow(loopdf)

reparationsdf <- lapply(loopdf$i,function(i) {
  
  #i<-1
  
  print(i)
  thisrow<-loopdf[i,]
  thisdf<-fulldf #this is the version of the df we're working with here
  
  ####(1) CALCULATE
  
  #based on the present-day gap
  whitewealth<-thisdf[race==1,sum(networth*weight)]
  blackwealth<-thisdf[race==2,sum(networth*weight)]
  nwhite<-thisdf[race==1,sum(weight)]
  nblack<-thisdf[race==2,sum(weight)]
  ntotal<-thisdf[,sum(weight)]

  #if we were taking money out of thin air, this would be the transfer
  reparations_transfer_raw <-
    (whitewealth/nwhite - blackwealth/nblack) *nblack
  
  #but it comes from people in this population, so it needs adjustment
  #solving for this analytically is a bit complicated,
  #so we just solve for it by trial + error
  #this is the transfer amount that 
  #equalizes white and black wealth after reparations
  reparations_transfer <-
    reparations_transfer_raw *
    1.00414 * (ntotal-nblack)/ntotal
  
  #### (2) TAX
  
  #nb: only those w/ wealth can be taxed
  
  #this gives us a schedule of progressive tax rates
  thisdf$taxrate <- sapply(
    thisdf$networth_q,
    getTaxRates,
    type='progressive'
  )
  #those w/ no wealth aren't taxed..
  thisdf[networth<=0,taxrate:=0]
  #if we taxed everyone at these rates, we would raise
  revenue_mooted <- sum(thisdf[networth>0,taxrate * networth * weight])
  
  #we need adjust this to get us what we need to get the reparations amount
  #i.e. we retain the progressive schedule, but inflate taxes to get reparations amouint
  thisdf[networth>0,taxrate:=taxrate * reparations_transfer/revenue_mooted]
  
  #this is the tax everyone pays
  thisdf[, networth_tax := networth * taxrate]
  
  #this is the revenue we raise for reparations
  revenue_raised <- sum(thisdf[networth>0,networth_tax * weight])
  
  ### (3) DISTRIBUTE
  
  if(thisrow$transfer=='flat') {
    
    #if transfer is flat, each black person gets an equal amount
    thisdf[
      ,
      networth_transfer := as.numeric(race==2) *
        revenue_raised/ #revenue raised
        sum(weight[race==2])
    ] #number of black people
    #this is how much we will be transferring
    
  } else {
    
    #if transfer is progressive, we take the total amount
    #and distribute it in a way that 'fills the bowl'
    
    #the way to do this is to loop through the CDF
    #and calculate the area under the curve
    #we want to get to the point where the area comes closest
    #to being the total amount of revenue raised
    amountsdf <- lapply(seq(from=97.8,to=97.9,by=0.001),function(q) {
      #print(q)
      #q<-95
      tmpdf<-thisdf[race==2,] #subset to blacks
      tmpdf$gap_to_q<-abs(tmpdf$networth_q_race - q)
      #the person in this q is the lowest level of wealth
      #note that we have to take tax into account
      lowerbound <- unique(tmpdf[gap_to_q==min(tmpdf$gap_to_q),networth - networth_tax])
      tmpdf[
        (networth-networth_tax)<lowerbound,
        networth_transfer:= lowerbound - (networth - networth_tax)
      ]
      tmpdf[(networth-networth_tax)>=lowerbound,networth_transfer:=0]
      data.frame(
        q=q,
        lowerbound=lowerbound,
        amount=sum(tmpdf[,networth_transfer*weight])
      )
    })%>% rbind.fill %>% data.table
    
    #pick the q that is closest to getting closest
    #to distribuitng the revneue we have raised
    amountsdf$revenue_raised <- revenue_raised
    amountsdf$revenue_left <- amountsdf$revenue_raised - amountsdf$amount
    lb<-max(amountsdf[revenue_left>0,q])
    ub<-min(amountsdf[revenue_left<0,q])
    amountsdf<-amountsdf[q%in%c(lb,ub)]
    thisq<-amountsdf[abs(revenue_left)==min(abs(revenue_left)),q]
    
    #implement the progressive redistribution
    lowerbound <- amountsdf[q==thisq,lowerbound]
    thisdf[
      race==2 & networth_q_race<=thisq,
      networth_transfer := lowerbound - (networth - networth_tax)
    ]
    thisdf[
      race!=2 | networth_q_race>=thisq,
      networth_transfer := 0
    ]
    
  }
  
  #calculate reparations
  thisdf[,networth_reparations := networth - networth_tax + networth_transfer]
  
  #return the dfs
  
  thisdf$i<-i
  thisdf
  
}) %>% rbind.fill %>% data.table


#check that networth is equal?

#in flat case
tmpdf <- reparationsdf[i==1,]
tmpdf[race==1,weighted.mean(networth_reparations,weight)]
tmpdf[race==2,weighted.mean(networth_reparations,weight)]
#same when rounded to the 1,000

#in not-flat case
tmpdf <- reparationsdf[i==2,]
tmpdf[race==1,weighted.mean(networth_reparations,weight)]
tmpdf[race==2,weighted.mean(networth_reparations,weight)]
#off by about $4,000, but this is a rounding issue, nothing to be done.. 

########################################################
########################################################

#### ---- IN THE FLAT CASE ---- ####

mydf <- reparationsdf[i==1,]
#how much does each black household receive?
mydf[race==2,weighted.mean(networth_reparations,weight)] - 
  mydf[race==2,weighted.mean(networth,weight)]

require(acid)
require(reldist)
#gini before
mydf[race==2,acid::weighted.gini(networth,weight)]
#gini after
mydf[race==2,acid::weighted.gini(networth_reparations,weight)]

#how much money is being distributed?
mydf[,sum(networth_tax*weight)]
mydf[,sum(networth_transfer*weight)]

########################################################
########################################################

#### ---- IN THE MEANS-TESTED CASE ---- ####

mydf <- reparationsdf[i==2,]

#gini before
mydf[race==2,acid::weighted.gini(networth,weight)]
#gini after
mydf[race==2,acid::weighted.gini(networth_reparations,weight)]

#FIGURE 
#plot the CDFs

plotdf<-mydf
#poorest blacks
plotdf[race==2,min(networth_reparations)]
plotdf[race==2,summary(networth_reparations)]
plotdf[race!=2,summary(networth_reparations)]

plotdf<-plotdf[race%in%c(1,2,3),]
plotdf<-pivot_longer(
  plotdf,
  names_to='var',
  values_to='val',
  cols=c('networth','networth_reparations')
) %>% data.table

plotdf$race<-factor(
  plotdf$race,
  levels=c(3,2,1),
  labels=c('Hispanic','Black','White')
)
tmpcolors<-c('#ffbf00','blue','red')
names(tmpcolors)<-levels(plotdf$race)

plotdf$var<-factor(
  plotdf$var,
  levels=c('networth','networth_reparations'),
  labels=c('Before Means-Tested Reparations','After Means-Tested Reparations')
)

#make the graph
g.tmp<-ggplot(
  plotdf,
  aes(
    x=networth_q_race,
    y=val,
    color=race
  )
) + 
  geom_line(
    size=1
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) + 
  scale_y_log10(
    breaks = 10^(0:9),
    labels = c(
      "$1 (and below)",
      "$10",
      "$100",
      "$1000",
      "$10,000",
      "$100,000",
      "$1 million",
      "$10 million",
      "$100 million",
      "$1 billion"
    )
  ) +
  theme_bw() +
  ylab("Net Worth\n") +
  xlab("\nWithin-Race Quantile") +
  facet_wrap(
    ~ var,
    ncol=1
  ) +
  theme(
    legend.position='top',
    legend.direction='horizontal',
    panel.grid.minor=element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'fig_reparations_cdfs.png',
  dpi=1200,
  width=4,
  height=6
)

########################################################
########################################################

#germany reparations to israel
#between 1952 and 1966
reparations_sum <- 3 * 10^9 #https://en.wikipedia.org/wiki/Reparations_Agreement_between_Israel_and_the_Federal_Republic_of_Germany
#w german gdp at this time
setwd(datadir); dir()
tmpdf<-fread('nominalgdp.csv')
tmpdf<-tmpdf[countryname=='West Germany' & year%in%1952:1965,c('year','value')]
1/(((reparations_sum))/sum(tmpdf$value)) #1/1248th of national product

########################################################
########################################################


























