## 1. load packages
library(tidyverse)
library(cowplot)
library(cols4all)
library(mgcv)
library(GWmodel)
library(parlitools)
library(broom)
library(spdep)
library(stringr)
install.packages("Metrics")
library(Metrics)



df <- read_csv("C:/Users/Lenovo/Documents/PhD Applied Spatial Data Science and UI/R & R Special Issue/us-data.csv")
# hex.gb = st_transform(hex.sp, 27700)
df <- st_as_sf(df, wkt = "geometry")

coords = st_coordinates(st_centroid(df))

df.gam =
  df %>%
  mutate(Intercept = 1,
         X = coords[,"X"]/1000,
         Y = coords[,"Y"]/1000) %>%
as_tibble()

# write.csv(df.gam, 'ggp-us-dfgam.csv')


gam.m = gam(totalvotes~ 0 + 
              Intercept + s(X,Y,bs='gp',by=Intercept) + 
              pct_black + s(X,Y,bs='gp',by=pct_black) +
              pct_65_over + s(X,Y,bs='gp',by=pct_65_over) +
              pct_age_18_29 + s(X,Y,bs='gp',by=pct_age_18_29) +
              pct_fb + s(X,Y,bs='gp',by=pct_fb) +
              pct_uninsured + s(X,Y,bs='gp',by=pct_uninsured) +
              pct_bach + s(X,Y,bs='gp',by=pct_bach) +
              ln_pop_den + s(X,Y,bs='gp',by=ln_pop_den),
            data = df.gam)   



# GGP-GAM SVCs: setting each beta to 1 others to 0

df.ii = df.gam %>% select(-id)
b0 <- df.gam %>% mutate(pct_black = 0, pct_65_over = 0, pct_age_18_29 = 0, pct_fb = 0,
                        pct_uninsured = 0, pct_bach = 0, ln_pop_den = 0, Intercept = 1)
df.ii <- df.ii %>% mutate(se0 = predict(gam.m, se = TRUE, newdata = b0)$se.fit,
                          b0=predict(gam.m,newdata=b0))
bpct_black <- df.gam %>% mutate(pct_black = 1, pct_65_over = 0, pct_age_18_29 = 0, pct_fb = 0, 
                               pct_uninsured = 0, pct_bach = 0, ln_pop_den = 0, Intercept = 0)
df.ii <- df.ii %>% mutate(sepct_black = predict(gam.m, se = TRUE, newdata = bpct_black)$se.fit,
                          bpct_black=predict(gam.m,newdata=bpct_black))
bpct_65_over <- df.gam %>% mutate(pct_black = 0, pct_65_over = 1, pct_age_18_29 = 0, pct_fb = 0, 
                                 pct_uninsured = 0, pct_bach = 0, ln_pop_den = 0, Intercept = 0)
df.ii <- df.ii %>% mutate(sepct_65_over = predict(gam.m, se = TRUE, newdata = bpct_65_over)$se.fit,
                          bpct_65_over=predict(gam.m,newdata=bpct_65_over))
bpct_age_18_29<- df.gam %>% mutate(pct_black = 0, pct_65_over = 0, pct_age_18_29 = 1, pct_fb = 0, 
                            pct_uninsured = 0, pct_bach = 0, ln_pop_den = 0, Intercept = 0)
df.ii <- df.ii %>% mutate(sepct_age_18_29 = predict(gam.m, se = TRUE, newdata = bpct_age_18_29)$se.fit,
                          bpct_age_18_29=predict(gam.m,newdata=bpct_age_18_29))
bpct_fb <- df.gam %>% mutate(pct_black = 0, pct_65_over = 0, pct_age_18_29 = 0, pct_fb = 1, 
                             pct_uninsured = 0, pct_bach = 0, ln_pop_den = 0, Intercept = 0)
df.ii <- df.ii %>% mutate(sepct_fb = predict(gam.m, se = TRUE, newdata = bpct_fb)$se.fit,
                          bpct_fb=predict(gam.m,newdata=bpct_fb))
bpct_uninsured <- df.gam %>% mutate(pct_black = 0, pct_65_over = 0, pct_age_18_29 = 0, pct_fb = 0, 
                              pct_uninsured = 1, pct_bach = 0, ln_pop_den = 0, Intercept = 0)
df.ii <- df.ii %>% mutate(sepct_uninsured = predict(gam.m, se = TRUE, newdata = bpct_uninsured)$se.fit,
                          bpct_uninsured=predict(gam.m, newdata=bpct_uninsured))
bpct_bach <- df.gam %>% mutate(pct_black = 0, pct_65_over = 0, pct_age_18_29 = 0, pct_fb = 0, 
                              pct_uninsured = 0, pct_bach = 1, ln_pop_den = 0, Intercept = 0)
df.ii <- df.ii %>% mutate(sepct_bach= predict(gam.m, se = TRUE, newdata = bpct_bach)$se.fit,
                          bpct_bach=predict(gam.m,newdata=bpct_bach))
bln_pop_den <- df.gam %>% mutate(pct_black = 0, pct_65_over = 0, pct_age_18_29 = 0, pct_fb = 0,
                                pct_uninsured = 0, pct_bach = 0, ln_pop_den = 1, Intercept = 0)
df.ii <- df.ii %>% mutate(seln_pop_den= predict(gam.m, se = TRUE, newdata = bln_pop_den)$se.fit,
                          bln_pop_den=predict(gam.m,newdata=bln_pop_den))

# create some fit measures
gam_hex.sp = df.ii
gam.pred = predict(gam.m, newdata = gam_hex.sp) 

write.csv(gam_hex.sp, './A/ggp-us-hex.csv')
write.csv(df.ii, './A/ggp-us-dfii.csv')


 rsq <- function(actual, predicted) {
  1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
}

r2 = round(rsq(df$totalvotes, gam.pred),3)
rmse = round(rmse(df$totalvotes, gam.pred),3)
#mae(hex.sp$leave, gam.pred)
aic = round(AIC(gam.m), 1)
st_geometry(gam_hex.sp) <- st_geometry(df)

## 14. Create Tables 4 and 5
tab = tidy(gam.m, digits = 3) 
tab2 <- gam_hex.sp %>% st_drop_geometry() %>% 
  select(pct_black, pct_65_over, pct_age_18_29, pct_fb, pct_uninsured, pct_bach, ln_pop_den)
tab2 = round(apply(tab2,2,summary),3)
colnames(tab2) = c("Intercept", names(hex.sp)[3:10])
tab2 = t(round(tab2, 2))
knitr::kable(tab, booktabs = T, digits = 3, row.names = F, linesep = "",
             caption = paste0("\\label{tab:tab4} The smooth terms of the GGP-GAM model."))
knitr::kable(tab2, booktabs = T, digits = 3, row.names = T, linesep = "",
             caption = paste0("\\label{tab:tab5}The distributions of the GGP-GAM spatially varying coefficient estimates ($R^{2}$ = ",r2, "; AIC = ", aic, ")."))

## 15. Create Figure 6 
plot_vgam_coef_func = function(var.name = "b0", tit) {
  var  = gam_hex.sp %>% 
    st_drop_geometry() %>% 
    dplyr::select(all_of(var.name)) %>%
    unlist() %>% 
    as.vector()
  if (sign(max(var)) * sign(min(var)) == 1) flip = F
  if (sign(max(var)) * sign(min(var)) == -1) flip = T
  
  if(flip) {
    ggplot(gam_hex.sp, aes_string(fill=var.name)) + 
      geom_sf(col = NA) + 
      scale_fill_continuous_c4a_div(palette="scico.vik", name= tit, mid = 0, reverse = T ) + 
      theme_bw() +
      theme(legend.position = "bottom", 
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            text=element_text(size=8))   
  } else {
    ggplot(gam_hex.sp, aes_string(fill=var.name)) + 
      geom_sf(col = NA) + 
      scale_fill_continuous_c4a_seq(palette="scico.nuuk", name= tit) + 
      theme_bw() +
      theme(legend.position = "bottom", 
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            text=element_text(size=8))
  }
}

p1 = plot_vgam_coef_func("b0", tit = "Intercept")
p2 = plot_vgam_coef_func("bpct_black", tit = "pct_black")
p3 = plot_vgam_coef_func("bpct_65_over", "pct_65_over")
p4 = plot_vgam_coef_func("bpct_age_18_29", "pct_age_18_29")
p5 = plot_vgam_coef_func("bpct_fb", "pct_fb")
p6 = plot_vgam_coef_func("bpct_uninsured", "pct_uninsured")
p7 = plot_vgam_coef_func("bpct_bach", "pct_bach")
p8 = plot_vgam_coef_func("bln_pop_den", "ln_pop_den")
if (.Platform$GUI == "AQUA") {
  quartz(w=9,h=11) } else  {
    x11(w=9,h=11) } 
plot_grid(p1, p2, p3, p4, p5,p6,p7,p8, ncol = 4)
