
######################
###    MODELLING    ###
#######################
#trip.i.coords <- merge(trip.i.coords,meta[,c("dev","sex")],all.x=TRUE)
#mean.i.coords <- merge(mean.i.coords,meta[,c("dev","sex")],all.x=TRUE)

# ignore forest as biome in analysis
#trip.i.coords$i.biome <- ifelse(trip.i.coords$i.biome == 'forest','other',trip.i.coords$i.biome)
#mean.i.coords$i.mean.biome <- ifelse(mean.i.coords$i.mean.biome == 'forest','other',mean.i.coords$i.mean.biome)

# define some funcitons to extract AIC for various models
calc.AIC <- function(x) {loglik <- logLik(x)
n   <- attributes(loglik)$nobs # following user20650 recommendation 
p   <- attributes(loglik)$df # following user20650 recommendation
dev <- -2*as.numeric(loglik)
(my_AIC  <- dev + 2 * p)
(my_AICc <- my_AIC + (2 * p^2 + 2 * p)/(n - p - 1))
(my_BIC  <- dev +  p * log(n))
as.data.frame(cbind(my_AIC,my_AICc,my_BIC,p,n,dev))
}

### MODEL WITHIN INDIVIDUAL VARIANCE IN FUNCTION OF BIOME AND SEAOSN
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## TEMPORAL
mod1 <- lmerTest::lmer(i.var.temp~trip * i.mean.biome + (1|dev) ,data=mean.i.coords) #mixed model
mod2 <- lmerTest::lmer(i.var.temp~trip + i.mean.biome + (1|dev) ,data=mean.i.coords) #mixed model
mod3 <- lmerTest::lmer(i.var.temp~trip  + (1|dev) ,data=mean.i.coords) #mixed model
mod4 <- lmerTest::lmer(i.var.temp~i.mean.biome  + (1|dev) ,data=mean.i.coords) #mixed model

modlist <- list(mod1,mod2,mod3,mod4)

R2m <- c(as.data.frame(r.squaredGLMM(mod1))$R2m, 
         as.data.frame(r.squaredGLMM(mod2))$R2m, 
         as.data.frame(r.squaredGLMM(mod3))$R2m, 
         as.data.frame(r.squaredGLMM(mod4))$R2m)
R2c <- c(as.data.frame(r.squaredGLMM(mod1))$R2c, 
         as.data.frame(r.squaredGLMM(mod2))$R2c, 
         as.data.frame(r.squaredGLMM(mod3))$R2c, 
         as.data.frame(r.squaredGLMM(mod4))$R2c)
AIC <- c(calc.AIC(mod1)$my_AIC,
         calc.AIC(mod2)$my_AIC,
         calc.AIC(mod3)$my_AIC,
         calc.AIC(mod4)$my_AIC)
dfs <- c(calc.AIC(mod1)$p,
         calc.AIC(mod2)$p,
         calc.AIC(mod3)$p,
         calc.AIC(mod4)$p)
nobs <- c(calc.AIC(mod1)$n,
          calc.AIC(mod2)$n,
          calc.AIC(mod3)$n,
          calc.AIC(mod4)$n)
formulas <- c(formula(mod1)[3],
              formula(mod2)[3],
              formula(mod3)[3],
              formula(mod4)[3])

intratemporal <- as.data.frame(cbind(as.character(formulas),nobs,dfs,AIC,R2m,R2c))
intratemporal$AIC <- as.numeric(intratemporal$AIC)
intratemporal$R2m <- as.numeric(intratemporal$R2m)
intratemporal$R2c <- as.numeric(intratemporal$R2c)
intratemporal$dAIC <- intratemporal$AIC - min(intratemporal$AIC)
intratemporal$vartype <- rep("Temporal")
colnames(intratemporal)[1] <- "predictors"

bestmod.intratemporal<- as.data.frame(coef(summary(mod1)))
bestmod.intratemporal$var <- rownames(coef(summary(mod1)))
bestmod.intratemporal$resp <- rep("Intra-Temporal")
bestmod.intratemporal <- bestmod.intratemporal[,c("Estimate","Std. Error","t value","Pr(>|t|)","var","resp")]

## SPATIAL
mod1 <- lmerTest::lmer(i.var.spat~trip * i.mean.biome + (1|dev) ,data=mean.i.coords) #mixed model
mod2 <- lmerTest::lmer(i.var.spat~trip + i.mean.biome + (1|dev) ,data=mean.i.coords) #mixed model
mod3 <- lmerTest::lmer(i.var.spat~trip  + (1|dev) ,data=mean.i.coords) #mixed model
mod4 <- lmerTest::lmer(i.var.spat~i.mean.biome  + (1|dev) ,data=mean.i.coords) #mixed model

modlist <- list(mod1,mod2,mod3,mod4)

R2m <- c(as.data.frame(r.squaredGLMM(mod1))$R2m, 
         as.data.frame(r.squaredGLMM(mod2))$R2m, 
         as.data.frame(r.squaredGLMM(mod3))$R2m, 
         as.data.frame(r.squaredGLMM(mod4))$R2m)
R2c <- c(as.data.frame(r.squaredGLMM(mod1))$R2c, 
         as.data.frame(r.squaredGLMM(mod2))$R2c, 
         as.data.frame(r.squaredGLMM(mod3))$R2c, 
         as.data.frame(r.squaredGLMM(mod4))$R2c)
AIC <- c(calc.AIC(mod1)$my_AIC,
         calc.AIC(mod2)$my_AIC,
         calc.AIC(mod3)$my_AIC,
         calc.AIC(mod4)$my_AIC)
dfs <- c(calc.AIC(mod1)$p,
         calc.AIC(mod2)$p,
         calc.AIC(mod3)$p,
         calc.AIC(mod4)$p)
nobs <- c(calc.AIC(mod1)$n,
          calc.AIC(mod2)$n,
          calc.AIC(mod3)$n,
          calc.AIC(mod4)$n)
formulas <- c(formula(mod1)[3],
              formula(mod2)[3],
              formula(mod3)[3],
              formula(mod4)[3])

intraspatial <- as.data.frame(cbind(as.character(formulas),nobs,dfs,AIC,R2m,R2c))
intraspatial$AIC <- as.numeric(intraspatial$AIC)
intraspatial$R2m <- as.numeric(intraspatial$R2m)
intraspatial$R2c <- as.numeric(intraspatial$R2c)
intraspatial$dAIC <- intraspatial$AIC - min(intraspatial$AIC)
intraspatial$vartype <- rep("Spatial")
colnames(intraspatial)[1] <- "predictors"

bestmod.intraspatial<- as.data.frame(coef(summary(mod1)))
bestmod.intraspatial$var <- rownames(coef(summary(mod1)))
bestmod.intraspatial$resp <- rep("Intra-Spatial")
bestmod.intraspatial <- bestmod.intraspatial[,c("Estimate","Std. Error","t value","Pr(>|t|)","var","resp")]


intravar <- rbind(intraspatial,intratemporal)
rm(intraspatial,intratemporal)

### MODEL BETWEEN INDIVIDUAL VARIANCE IN FUNCTION OF BIOME AND SEAOSN
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## TEMPORAL
mod1 <- glm(i.popvar.temp~trip * i.popmean.biome  ,data=popmean.i.coords) #mixed model
mod2 <- glm(i.popvar.temp~trip + i.popmean.biome ,data=popmean.i.coords) #mixed model
mod3 <- glm(i.popvar.temp~trip,data=popmean.i.coords) #mixed model
mod4 <- glm(i.popvar.temp~i.popmean.biome ,data=popmean.i.coords) #mixed mode

R2 <- c( rsq(mod1), 
         rsq(mod2), 
         rsq(mod3),
         rsq(mod3))
AICc <- c(calc.AIC(mod1)$my_AICc,
          calc.AIC(mod2)$my_AICc,
          calc.AIC(mod3)$my_AICc,
          calc.AIC(mod4)$my_AICc)
dfs <- c(calc.AIC(mod1)$p,
         calc.AIC(mod2)$p,
         calc.AIC(mod3)$p,
         calc.AIC(mod4)$p)
nobs <- c(calc.AIC(mod1)$n,
          calc.AIC(mod2)$n,
          calc.AIC(mod3)$n,
          calc.AIC(mod4)$n)
formulas <- c(formula(mod1)[3],
              formula(mod2)[3],
              formula(mod3)[3],
              formula(mod4)[3])

intertemporal <- as.data.frame(cbind(as.character(formulas),nobs,dfs,AICc,R2))
intertemporal$R2 <- as.numeric(intertemporal$R2)
intertemporal$AICc <- as.numeric(intertemporal$AICc)
intertemporal$dAICc <- intertemporal$AICc - min(intertemporal$AICc)
colnames(intertemporal)[1] <- "predictors"
intertemporal$vartype <- rep("Inter-Temporal")

bestmod.intertemporal <- as.data.frame(coef(summary(mod1)))
bestmod.intertemporal$var <- rownames(coef(summary(mod1)))
bestmod.intertemporal$resp <- rep("Inter-Temporal")

## SPATIAL
mod1 <- glm(i.popvar.spat~trip * i.popmean.biome  ,data=popmean.i.coords) #mixed model
mod2 <- glm(i.popvar.spat~trip + i.popmean.biome ,data=popmean.i.coords) #mixed model
mod3 <- glm(i.popvar.spat~trip,data=popmean.i.coords) #mixed model
mod4 <- glm(i.popvar.spat~i.popmean.biome ,data=popmean.i.coords) #mixed mode

R2 <- c( rsq(mod1), 
         rsq(mod2), 
         rsq(mod3),
         rsq(mod4))
AICc <- c(calc.AIC(mod1)$my_AICc,
          calc.AIC(mod2)$my_AICc,
          calc.AIC(mod3)$my_AICc,
          calc.AIC(mod4)$my_AICc)
dfs <- c(calc.AIC(mod1)$p,
         calc.AIC(mod2)$p,
         calc.AIC(mod3)$p,
         calc.AIC(mod4)$p)
nobs <- c(calc.AIC(mod1)$n,
          calc.AIC(mod2)$n,
          calc.AIC(mod3)$n,
          calc.AIC(mod4)$n)
formulas <- c(formula(mod1)[3],
              formula(mod2)[3],
              formula(mod3)[3],
              formula(mod4)[3])

interspatial <- as.data.frame(cbind(as.character(formulas),nobs,dfs,AICc,R2))
interspatial$R2 <- as.numeric(interspatial$R2)
interspatial$AICc <- as.numeric(interspatial$AICc)
interspatial$dAICc <- interspatial$AICc - min(interspatial$AICc)
colnames(interspatial)[1] <- "predictors"
interspatial$vartype <- rep("Inter-Spatial")

bestmod.interspatial <- as.data.frame(coef(summary(mod1)))
bestmod.interspatial$var <- rownames(coef(summary(mod1)))
bestmod.interspatial$resp <- rep("Inter-Spatial")

intervar <- rbind(interspatial,intertemporal)
rm(interspatial,intertemporal)

### MODEL INDIVIDUAL REPEATABILITY IN FUNCTION OF BIOME AND SEAOSN
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## TEMPORAL
mod1 <- glm(R~trip * i.popmean.biome  ,data=results_temp_df[which(results_temp_df$pred == "dev"),]) #mixed model
mod2 <- glm(R~trip + i.popmean.biome ,data=results_temp_df[which(results_temp_df$pred == "dev"),]) #mixed model
mod3 <- glm(R~trip,data=results_temp_df[which(results_temp_df$pred == "dev"),]) #mixed model
mod4 <- glm(R~i.popmean.biome ,data=results_temp_df[which(results_temp_df$pred == "dev"),]) #mixed mode

R2 <- c( rsq(mod1), 
         rsq(mod2), 
         rsq(mod3),
         rsq(mod3))
AICc <- c(calc.AIC(mod1)$my_AICc,
          calc.AIC(mod2)$my_AICc,
          calc.AIC(mod3)$my_AICc,
          calc.AIC(mod4)$my_AICc)
dfs <- c(calc.AIC(mod1)$p,
         calc.AIC(mod2)$p,
         calc.AIC(mod3)$p,
         calc.AIC(mod4)$p)
nobs <- c(calc.AIC(mod1)$n,
          calc.AIC(mod2)$n,
          calc.AIC(mod3)$n,
          calc.AIC(mod4)$n)
formulas <- c(formula(mod1)[3],
              formula(mod2)[3],
              formula(mod3)[3],
              formula(mod4)[3])

reptemp <- as.data.frame(cbind(as.character(formulas),nobs,dfs,AICc,R2))
reptemp$R2 <- as.numeric(reptemp$R2)
reptemp$AICc <- as.numeric(reptemp$AICc)
reptemp$dAICc <- reptemp$AICc - min(reptemp$AICc)
colnames(reptemp)[1] <- "predictors"
reptemp$vartype <- rep("RepTemp-ID")

bestmod.RepTemp <- as.data.frame(coef(summary(mod1)))
bestmod.RepTemp$var <- rownames(coef(summary(mod1)))
bestmod.RepTemp$resp <- rep("RepTemp-ID")

## SPATIAL
mod1 <- glm(R~trip * i.popmean.biome  ,data=results_spat_df[which(results_spat_df$pred == "dev"),]) #mixed model
mod2 <- glm(R~trip + i.popmean.biome ,data=results_spat_df[which(results_spat_df$pred == "dev"),]) #mixed model
mod3 <- glm(R~trip,data=results_spat_df[which(results_spat_df$pred == "dev"),]) #mixed model
mod4 <- glm(R~i.popmean.biome ,data=results_spat_df[which(results_spat_df$pred == "dev"),]) #mixed mode

R2 <- c( rsq(mod1), 
         rsq(mod2), 
         rsq(mod3),
         rsq(mod4))
AICc <- c(calc.AIC(mod1)$my_AICc,
          calc.AIC(mod2)$my_AICc,
          calc.AIC(mod3)$my_AICc,
          calc.AIC(mod4)$my_AICc)
dfs <- c(calc.AIC(mod1)$p,
         calc.AIC(mod2)$p,
         calc.AIC(mod3)$p,
         calc.AIC(mod4)$p)
nobs <- c(calc.AIC(mod1)$n,
          calc.AIC(mod2)$n,
          calc.AIC(mod3)$n,
          calc.AIC(mod4)$n)
formulas <- c(formula(mod1)[3],
              formula(mod2)[3],
              formula(mod3)[3],
              formula(mod4)[3])

repspat <- as.data.frame(cbind(as.character(formulas),nobs,dfs,AICc,R2))
repspat$R2 <- as.numeric(repspat$R2)
repspat$AICc <- as.numeric(repspat$AICc)
repspat$dAICc <- repspat$AICc - min(repspat$AICc)
colnames(repspat)[1] <- "predictors"
repspat$vartype <- rep("RepSpat-ID")

bestmod.RepSpat <- as.data.frame(coef(summary(mod1)))
bestmod.RepSpat$var <- rownames(coef(summary(mod1)))
bestmod.RepSpat$resp <- rep("RepSpat-ID")

reps <- rbind(repspat,reptemp)
rm(repspat,reptemp)

table2 <- intravar
table3 <- rbind(intervar,reps)

write.csv(table2,'./output - tables/Table2_LMERS-WithinIndividualVariability-vs-Biome-Season.csv')
write.csv(table3,'./output - tables/Table3_LMERS-BetweenIndividualVariability-Repeatability-vs-Biome-Season.csv')

table4 <- rbind(bestmod.intraspatial,bestmod.interspatial,bestmod.RepSpat,bestmod.intratemporal,bestmod.intertemporal,bestmod.RepTemp)
write.csv(table4,'./output - tables/Table4_BestModels-BetweenIndividualVariability-Repeatability-vs-Biome-Season.csv')

rm(table2,table3,reps,intervar,intravar)
rm(R2,AIC,AICc,formulas,nobs,dfs,R2m,R2c,mod1,mod2,mod3,mod4)
rm(bestmod.intraspatial,bestmod.intratemporal,bestmod.RepSpat,bestmod.RepTemp)
