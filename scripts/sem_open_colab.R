opencolab <- read.csv(file.choose())
names(opencolab)
head(opencolab)

library(lavaan)

model <- '
        # latent 
        
        healthcareoutcomes =~ health + healthcare + healthy + ehealth + mhealth + telehealth
        opencollab =~ open + openaccess + openness + opensource + collaborative + collaboratively + collaboratives
        openinnovation =~ open + openaccess + openness + opensource + innovation + innovations
        
        # direct effect
        healthcareoutcomes ~ c*openinnovation
        
        # mediator
        
        opencollab ~ a*healthcareoutcomes
        openinnovation ~ b*opencollab
        
        # indirect effect (a*b)
              ab := a*b
        # total effect
              total := c + (a*b)
'

model <- '
        # latent 
        
        # healthcareoutcomes =~ health + ehealth + mhealth + telehealth
        opencollab =~ open + collaboratives
        openinnovation =~ open + innovations
        
        # direct effect
        health ~ c*openinnovation
        
        # mediator
        
        opencollab ~ a*health
        innovation ~ b*opencollab
        
        # indirect effect (a*b)
              ab := a*b
        # total effect
              total := c + (a*b)
'

fit <- sem(model, scale(opencolab))
summary(fit, fit.measures = TRUE)

getwd()
setwd('D:/Research/papers/HC/oci/R/outputs')
getwd()

write.csv(parameterEstimates(fit), 'open_collab_fit.csv')
write.csv(fitmeasures(fit), 'open_collab_fit_measures.csv')

library(semPlot)
library(semptools)

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
semPaths(fit, layout = 'circle2')
p_pa <- semPaths(fit, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 style = "ram",
                 nCharNodes = 0, nCharEdges = 0, layout = 'circle2') 
p_pa2 <- mark_sig(p_pa, fit)
plot(p_pa2)
