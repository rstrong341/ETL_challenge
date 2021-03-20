#!/usr/bin/env Rscript
require(ppcor) # for partial correlations

# load file and get rid of 0 values for log
t <- read.csv('GPI and religion.csv')
inds_rm <- which(t$gdp_pc==0 | t$percentage_non_religious==0)

# variables of interest
x_GDP <- log10(t$gdp_pc[-inds_rm])
x_GPI <- t$gpi_rank[-inds_rm]
y <- log10(t$percentage_non_religious[-inds_rm]/100)

#########
## GDP ##
#########
# correlation statistics
cc <- cor.test.0(x_GDP,y,method='s')
rho <- cc$estimate
pv <- cc$p.value

# correlation statistics (correcting for GPI)
ccp <- pcor.test(x_GDP,y,x_GPI, method='s')
rhop <- ccp$estimate
pvp <- ccp$p.value

png('plotGDPVsReligion.png',width=7,height=7,unit='in',res=300)
par(mar = c(6,6,5,5), 
    cex.lab = 1.3, 
    cex.axis = 1.3, 
    cex.main = 1, 
    pch = 19
    )
plot(x_GDP, y,
       xlab='GDP per capita (log10)',
       ylab='% non religious (log10)',
       main=paste0('Spearman\'s rho = ',signif(rho,2),' (p=',signif(pv,2),')\n',
                  'Partial correlation corrected for GPI: rho = ',signif(rhop,2),' (p=',signif(pvp,2),')\n'
                  ),
       cex=0.6,
       col=rgb.0('blue')
       )
abline(lm(y ~ x_GDP))
dev.off()

#########
## GPI ##
#########
# correlation statistics
cc <- cor.test.0(x_GPI,y,method='s')
rho <- cc$estimate
pv <- cc$p.value

# correlation statistics (correcting for GDP)
ccp <- pcor.test(x_GPI,y,x_GDP, method='s')
rhop <- ccp$estimate
pvp <- ccp$p.value

png('plotGPIVsReligion.png',width=7,height=7,unit='in',res=300)
par(mar = c(6,6,5,5), 
    cex.lab = 1.3, 
    cex.axis = 1.3, 
    cex.main = 1, 
    pch = 19
    )
plot(x_GPI, y,
       xlab='GPI rank',
       ylab='% non religious (log10)',
       main=paste0('Spearman\'s rho = ',signif(rho,2),' (p=',signif(pv,2),')\n',
                  'Partial correlation corrected for GDP: rho = ',signif(rhop,2),' (p=',signif(pvp,2),')\n'
                  ),
       cex=0.6,
       col=rgb.0('blue')
       )
abline(lm(y ~ x_GPI))
dev.off()
