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

#load the reparations
setwd(filesdir); dir()
reparationsdf <- fread('reparationsdf.csv')

########################################################
########################################################

#get some summary stats

require(acid)
require(reldist)
loopdf<-expand.grid(
  race=c(0,1,2,3,4),
  type=c('networth','networth_reparations'),
  world=unique(reparationsdf$i),
  stringsAsFactors=F
)

sumdf <- lapply(1:nrow(loopdf),function(ii) {
  #ii<-6
  
  #get thisdf
  mydf<-reparationsdf[i==loopdf$world[ii],]
  
  print(ii)
  if(loopdf$race[[ii]]!=0) {
    tmp<-mydf$race==loopdf$race[[ii]]
  } else {
    tmp<-rep(T,nrow(mydf))
  }
  gini<-weighted.gini(mydf[[loopdf$type[ii]]][tmp],w=mydf$weight[tmp])$Gini %>%
    as.vector
  total<-sum(mydf[[loopdf$type[ii]]][tmp]*mydf$weight[tmp])
  mean<-weighted.mean(mydf[[loopdf$type[ii]]][tmp],mydf$weight[tmp])
  median<-modi::weighted.quantile(mydf[[loopdf$type[[ii]]]][tmp],mydf$weight[tmp],0.5)
  data.frame(
    i=loopdf$world[ii],
    N=sum(mydf$weight[tmp]),
    gini,
    total,
    mean,
    median
  )
}) %>% rbind.fill 
sumdf <- cbind(loopdf,sumdf) %>% data.table

sumdf<-merge(
  sumdf,
  unique(reparationsdf[,c('i','based','tax','transfer')]),
  by='i'
)

########################################################
########################################################

#TABLE 1

#plot tables of relevant stats

plotdf<-sumdf[
  based=='gap' & 
    tax=='progressive' & 
    transfer=='flat'
]
plotdf<-gather(
  plotdf,
  var,
  val,
  N:median
) %>% data.table
plotdf<-plotdf[race%in%c(0,1,2) & var%in%c('gini','median','mean')]

plotdf$race<-factor(
  plotdf$race,
  levels=c(0,1,2,3,4),
  labels=c('Overall','White','Black','Hispanic','Other')
)

plotdf$type<-factor(
  plotdf$type,
  levels=c('networth','networth_reparations') %>% rev,
  labels=c('Before Reparations','After Reparations') %>% rev
)

#round these numbers to the nearest 1000
tmp<-plotdf$var%in%c('mean','median')
plotdf$val[tmp] <- 10^3 *round(plotdf$val[tmp]/10^3)

#make them pretty
plotdf$val <- prettyNum(plotdf$val,digits=2,big.mark = ',')
plotdf$val[plotdf$var%in%c('mean','median')]<-
  paste0("$",plotdf$val[plotdf$var%in%c('mean','median')])

plotdf$var <- factor(
  plotdf$var,
  levels=c('mean','median','gini'),
  labels=c('Mean','Median','Gini')
)

tmpcolors<-c('red','black')
names(tmpcolors)<-levels(plotdf$type)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=race,
    y=type,
    label=val,
    color=type
  )
) +
  geom_tile(color='black',fill='white') +
  geom_text() +
  scale_color_manual(
    guide='none',
    values=tmpcolors
  ) +
  scale_x_discrete(position='top') + 
  xlab("") + ylab("") +
  facet_wrap(
    ~ var,
    ncol=1
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line= element_blank(),
    panel.border=element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'tab_reparations.png',
  width=4,
  height=4
)


########################################################
########################################################

#FIGURE 

#make a summary plot in the progressive world

plotdf<-sumdf[based=='gap' & tax=='progressive' & transfer=='progressive']
plotdf<-gather(
  plotdf,
  var,
  val,
  N:median
) %>% data.table
plotdf<-plotdf[race%in%c(0,1,2) & var%in%c('gini','median','mean')]

# plotdf$var<-factor(
#   plotdf$var,
#   levels=c('mean','median'),
#   labels=c('Mean','Median')
# )

plotdf$race<-factor(
  plotdf$race,
  levels=c(0,1,2,3,4),
  labels=c('Overall','White','Black','Hispanic','Other')
)

plotdf$type<-factor(
  plotdf$type,
  levels=c('networth','networth_reparations') %>% rev,
  labels=c('Before Reparations','After Reparations') %>% rev
)

plotdf$val <- prettyNum(plotdf$val,digits=2,big.mark = ',')
plotdf$val[plotdf$var%in%c('mean','median')]<-
  paste0("$",plotdf$val[plotdf$var%in%c('mean','median')])

plotdf$var <- factor(
  plotdf$var,
  levels=c('mean','median','gini'),
  labels=c('Mean','Median','Gini')
)

tmpcolors<-c('red','black')
names(tmpcolors)<-levels(plotdf$type)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=race,
    y=type,
    label=val,
    color=type
  )
) +
  geom_tile(color='black',fill='white') +
  geom_text() +
  scale_color_manual(
    guide='none',
    values=tmpcolors
  ) +
  scale_x_discrete(position='top') + 
  xlab("") + ylab("") +
  facet_wrap(
    ~ var,
    ncol=1
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line= element_blank(),
    panel.border=element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'tab_reparations_progressive.png',
  width=4,
  height=4
)


########################################################
########################################################

#FIGURE 
#plot the CDFs

plotdf<-reparationsdf[
  race%in%c(1,2,3,4) & 
    based=='gap' & 
    tax=='progressive' & 
    transfer=='progressive',
  c('race','networth_q_race','networth','networth_reparations')
]

plotdf[race==3,min(networth_reparations)]


#poorest blacks
plotdf[race==2,min(networth_reparations)]
plotdf[race==2,summary(networth_reparations)]
plotdf[race!=2,summary(networth_reparations)]

plotdf<-plotdf[race%in%c(1,2,3),]
plotdf<-gather(
  plotdf,
  var,
  val,
  networth:networth_reparations
)

plotdf$race<-factor(
  plotdf$race,
  levels=c(3,2,1),
  labels=c('Hispanic','Black','White')
)
tmpcolors<-c('blue','red')
names(tmpcolors)<-levels(plotdf$race)

plotdf$var<-factor(
  plotdf$var,
  levels=c('networth','networth_reparations'),
  labels=c('Before','After')
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
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'fig_reparations_progressive_cdfs.png',
  width=6,
  height=8
)

########################################################
########################################################

#FIGURE 

#focus on the non-black poor
#imagine we generated 13 trillion in a progressive way
#what else could we do with it?
setwd(filesdir); dir()
thisdf <- fread('fulldf.csv')

#this is the reparations bill
whitewealth<-thisdf[race==1,sum(networth*weight)]
blackwealth<-thisdf[race==2,sum(networth*weight)]
nwhite<-thisdf[race==1,sum(weight)]
nblack<-thisdf[race==2,sum(weight)]
reparations_transfer <- 
  (whitewealth/nwhite - blackwealth/nblack) *
  (nwhite*nblack)/(nwhite+nblack)

#generate it via taxation
#this gives us our tax rates
thisdf$taxrate <- sapply(thisdf$networth_q,getTaxRates,type='progressive')
thisdf[networth<=0,taxrate:=0]

#if we taxed everyone at these rates, we would raise
revenue_mooted <- sum(thisdf[networth>0,taxrate * networth * weight])

#but in fact we need to raise amount equal to the reparations bill
#so tax rates should be calibrated accordingly
thisdf[networth>0,taxrate:=taxrate * reparations_transfer/revenue_mooted]

#and some share of it will come from black people,
#so in fact the bill has to be a bit higher
#this is a bit complicated 
#some comes from non-white and non-black people, some comes from non-black people
#an adjustment factor calibrated to equalize white and networth
adjustmentfactor<-1.05285 #later, solve this analytically
thisdf[networth>0, taxrate:=taxrate * adjustmentfactor]

#calculate the tax
thisdf[, networth_tax := networth * taxrate]
revenue_raised <- sum(thisdf[,networth_tax * weight])

#use this money to fill up the bucket
amountsdf <- lapply(seq(from=50,to=100,by=1),function(q) {
  #print(q)
  #q<-50
  tmpdf<-thisdf
  tmpdf$gap_to_q<-abs(tmpdf$networth_q - q)
  #the person in this q is the lowest level of wealth
  #note that we have to take tax into account
  lowerbound <- unique(
    tmpdf[gap_to_q==min(tmpdf$gap_to_q),
          networth - networth_tax]
  ) %>% min #take the minimum
  tmpdf[
    (networth-networth_tax)<lowerbound, #for all those who will be below lowerbound
    networth_transfer:= lowerbound - (networth - networth_tax) #bring them up to lowerbound
  ]
  tmpdf[(networth-networth_tax)>=lowerbound, #for all those above
        networth_transfer:=0 #they get nothing
  ]
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

#implement this redistribution in class-based fashion
lowerbound <- amountsdf[q==thisq,lowerbound] 
thisdf[
  networth_q<=thisq,
  networth_transfer := lowerbound - (networth - networth_tax)
]
thisdf[
  networth_q>=thisq,
  networth_transfer := 0
]

#now put these two worlds together
classdf<-thisdf
reparationsdf <- fread('reparationsdf.csv')
reparationsdf <- reparationsdf[
  based=='gap' & 
    tax=='progressive' & 
    transfer=='flat'
]
classdf <- classdf[,c('hhid','networth_transfer')]
reparationsdf <- reparationsdf[,c('hhid','networth_transfer')]
classdf$scenario<-'classbased'; reparationsdf$scenario<-'reparations'

diffdf <- rbindlist(
  list(classdf,reparationsdf)
)

#merge identifying information
setwd(filesdir); dir()
thisdf <- fread('fulldf.csv')
thisdf <- thisdf[
  ,
  c(
    'hhid',
    'race',
    'networth',
    'networth_q',
    'networth_q_race',
    'weight'
  )
]

#regenerate quantiles
thisdf$race[thisdf$race==4] <- 3
require(spatstat)
tmpfunction<-function(x,w) {
  tmpf<-ewcdf(x,w)
  100 * tmpf(x)
}
thisdf[
  ,
  networth_q_race := tmpfunction(networth,weight)
  ,
  by=c('race')
]

#merge this in
plotdf <- merge(
  thisdf,
  diffdf,
  by=c('hhid')
)

#prep for plotting
plotdf$race <- factor(
  plotdf$race,
  c(1,2,3),
  c('White','Black','Other')
)

plotdf$scenario <- factor(
  plotdf$scenario,
  c('reparations','classbased'),
  c('Race-Based Reparations','Wealth-Based Redistribution')
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=networth_q_race,
    y=networth_transfer,
    group=race,
    color=race
  )
) +
  geom_line() +
  scale_color_manual(
    name="",
    values=c(
      'White'='red',
      'Black'='blue',
      'Other'='darkgreen'
    )
  ) +
  scale_y_continuous(
    breaks=c(0,2.5*10^5,5*10^5,7.5*10^5,1*10^6),
    labels=c('$0','$250,000','$500,000','$750,000','$1,000,000')
  ) +
  facet_wrap(
    ~ scenario,ncol=1
  ) +
  xlab('\nWithin-Race Quantile') + 
  ylab('Size of Transfer\n') +
  theme_bw() +
  theme(legend.position='top')

setwd(outputdir)
ggsave(
  plot=g.tmp,
  'fig_transfercontrast.png',
  width=6,
  height=8
)

#quick estimates





