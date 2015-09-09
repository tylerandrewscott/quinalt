getwd()
list.files()
load('test.results.24m.RData')


library(texreg)


texreg(mod_base_24)

form_base_24

form_project_24
mod_base_24
summary(mod_base_24)
summary(mod_project_24)

rownames(mod_project_24$summary.fixed)[17] <- 'Full.Interaction'



summary(mod_project_24)
