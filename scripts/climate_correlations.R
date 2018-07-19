# climate correlations

library(dplyr)
clim <- read.csv("data/all_clim.csv") 

# slim down data to sets of interest
clim_temp <- clim %>% 
  dplyr::select(MAT_hist:Tsum_hist, MAT_exp:Tsum_exp)
clim_ppt <- clim %>% 
  dplyr::select(MAP_hist:PPT_sm_hist, MAP_exp:PPT_sm_exp)
clim_slim <- clim %>% 
  dplyr::select(MAT_hist:Tsum_hist, MAT_exp:Tsum_exp, MAP_hist:PPT_sm_hist, MAP_exp:PPT_sm_exp)

# make nice figures for examining correlations - found function on the web separated from original attribution
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

pairs(clim_temp,lower.panel = panel.smooth, upper.panel = panel.cor)
pairs(clim_ppt,lower.panel = panel.smooth, upper.panel = panel.cor)

# make correlation matrices
ppt_cors <- cor(clim_ppt)
temp_cors <- cor(clim_temp)
all_cors <- cor(clim_slim)

# write out csvs
write.csv(ppt_cors, "data/climate_correlations/cors_ppt.csv")
write.csv(temp_cors, "data/climate_correlations/cors_temp.csv")
write.csv(all_cors, "data/climate_correlations/cors_all.csv")
names(clim)
