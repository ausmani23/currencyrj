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
fulldf$networth_q <- round(100 * tmpf(fulldf$networth))
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

########################################################
########################################################

#LOOP THROGUH AND IMPLEMENT DIFFERENT KINDS OF REPARATIONS

loopdf<-expand.grid(
  #how to calculate the bill
  based=c('gap','history'),
  #how to tax
  tax=c('flat','progressive'),
  #how to redistribute
  transfer=c('flat','progressive'),
  stringsAsFactors=F
)
loopdf$i<-1:nrow(loopdf)

reparationsdf <- lapply(loopdf$i,function(i) {
  
  #i<-6
  
  print(i)
  thisrow<-loopdf[i,]
  thisdf<-fulldf #this is the version of the df we're working with here
  
  ####(1) CALCULATE
  
  if (thisrow$based=='history') {
    
    #all estimates from Darity and Mullen
    reparations_transfer_pb<-list(
      ransomsutch=c(14 * 10^9,19.7 * 10^9, 27.7 * 10^9), #p. 326
      neal=c(5.7 * 10^12,8.1 * 10^12, 11.4 * 10^12), #p. 326
      craemer=c(16.4 * 10^12,17 * 10^12, 17.7 * 10^12), #p. 327
      marketti=c(8.6 * 10^12,12.1 * 10^12, 17.1 * 10^12), #327
      benjamin=c(2 * 10^12, 9.3 * 10^12, 42.2 * 10^12), #p.327
      freedmen=c(168 * 10^9, 733.2 * 10^9, 3.1 * 10^12), #p. 328
      homestead=c(671 * 10^9, 2.9 * 10^12, 12.6 * 10^12) #p. 329
    )
    sapply(reparations_transfer_pb,function(x) x[1]) %>% mean
    sapply(reparations_transfer_pb,function(x) x[2]) %>% mean
    #we'll take the average of the 6% estimates..
    reparations_transfer <- sapply(reparations_transfer_pb,function(x) x[3]) %>% mean
    
  } else {
    
    #based on the present-day gap
    whitewealth<-thisdf[race==1,sum(networth*weight)]
    blackwealth<-thisdf[race==2,sum(networth*weight)]
    nwhite<-thisdf[race==1,sum(weight)]
    nblack<-thisdf[race==2,sum(weight)]
    reparations_transfer <- 
      (whitewealth/nwhite - blackwealth/nblack) *
      (nwhite*nblack)/(nwhite+nblack)
    
  }
  
  #### (2) TAX
  
  #nb: only those w/ wealth can be taxed
  
  #this gives us our tax rates
  thisdf$taxrate <- sapply(thisdf$networth_q,getTaxRates,type=thisrow$tax)
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
  
  ### (3) DISTRIBUTE
  
  if(thisrow$transfer=='flat') {
    
    #if transfer is flat, each black person gets an equal amount
    thisdf[
      ,
      networth_transfer:= as.numeric(race==2) *
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
    amountsdf <- lapply(seq(from=97,to=100,by=0.01),function(q) {
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


########################################################
########################################################


reparationsdf<-merge(
  loopdf,
  reparationsdf,
  by='i'
)

setwd(filesdir); dir()
write.csv(
  reparationsdf,
  'reparationsdf.csv',
  row.names=F
)





########################################################
########################################################
