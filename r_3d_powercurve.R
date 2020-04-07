setwd("~/Desktop/3d_powercurve/")

wtg=read.table("windfarm.csv", sep = ";",
              stringsAsFactors = F, header = T)
wtg %>% filter(Device == "WTG01")
nemicin=readxl::read_xlsx("hamveriler/asos_iowa.xlsx")
nemicin$Temperature = (nemicin$tmpf-32)/1.8
nemicin$dwpc = (nemicin$dwpf-32)/1.8

plot(nemicin$Temperature,nemicin$relh)

m_nem = lm(relh~Temperature, data = nemicin)
tahmininem = predict(m_nem, nemicin)

plot(tahmininem, type = "l")
lines(nemicin$relh,type = "l", col="red")


library(weathercan)
basinc_icin = read.table("hamveriler/basinc.csv",header = T,skip = 1,stringsAsFactors = F, sep = ";")
colnames(basinc_icin) = c("tarih","press","Temperature")

m_basinc = lm(press~Temperature,data = basinc_icin)
tahminibas = predict(m_basinc, basinc_icin)

plot(tahminibas, type = "l")
lines(basinc_icin$press,type = "l", col="red")

wtg$Relh = predict(m_nem, wtg)
wtg$Pressure = predict(m_basinc, wtg)
wtg$Pressure=wtg$Pressure

wtg$Relh=pmin(wtg$Relh, 100)
plot(wtg$Pressure)

write.table(wtg,"windfarm_raw.csv",col.names = T, row.names = F,sep = ",")

wtg = read.csv("windfarm_raw.csv",header = T, stringsAsFactors = F)

#Make sure to use Celcius units for Temperature, hPa or mb units for air Pressure and
# % for relative humidity.
air_dens_calc = function(temp,press,rh) {
  
  if (missing(rh)) {

      warning("You didn't specify relative humidity, so dry air density is calculated.",call. = F)
      rho = (press*100)/(287.058*(temp+273.15))
      return(rho)
      
    } else {
    
      warning("Moist air density is calculated!",call. = F)
      p1 = 6.1078 * 10^(7.5*temp/((temp+273.15)+237.3))
      pv = p1*rh
      pd = press*100 - pv
      rho = (pd/(287.058*(temp+273.15)))+(pv/(461.495*(temp+273.15)))
      return(rho)
      
  }
}

air_dens_calc(press = wtg$Pressure,temp = wtg$Temperature)
air_dens_calc(press = wtg$Pressure,temp = wtg$Temperature, rh = wtg$Relh)

  wtg$AirDensity = air_dens_calc(press = wtg$Pressure,temp = wtg$Temperature, rh = wtg$Relh)

#Now make some IEC adjustments to the raw wind speed data from nacelle mounted anemometer of wind
#turbine. First function which is iec_ad makes air density correction to the wind speed data by 
#using wind speed and air density.
#Second function which is iec_turb makes turbulence correction to the wind speed
#data by using wind speed and standart deviation of the record.
#Third function combines two IEC correction functions


#IEC air density correction function
iec_ad = function(ws,rho) {
  
  ws_norm=ws*(rho/1.225)^1/3
  return(ws_norm)
}

#IEC turbulence correction function
iec_turb = function(ws, ws_std) {
  
  ws_corr = ws*(1+3*(ws_std/ws)^2)^1/3
  return(ws_corr)
  
}

#IEC corrections which have to be applied to the raw wind turbine wind speed data.
iec_corr = function(ws,ws_std,rho) {
  ws_norm=ws*(rho/1.225)^(1/3)
  ws_corr = ws_norm*(1+3*(ws_std/ws)^2)^(1/3)
  return(ws_corr)
}



# if (ws != 0) {
#   ws_norm=ws*(rho/1.225)^(1/3)
#   ws_corr = ws_norm*(1+3*(ws_std/ws)^2)^(1/3)
#   return(ws_corr)
#   
# } else {
#   
#   ws_corr = 0
#   return(ws_corr)
#   
# }


wtg$WindSpeed!=0
wtg$WindSpeed_IEC=iec_corr(ws = wtg$WindSpeed,ws_std = wtg$WindSpeed_Std, rho = wtg$AirDensity)
summary(wtg)

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#Change class of date column to POSIXct from character.
wtg$Date = ymd_hms(wtg$Date)

wtg %>% filter(Device == "WTG01") %>% select(Date,WindSpeed,WindSpeed_IEC) %>% 
  gather(key = "Label", value = "WindSpeed", -Date) %>% filter((ceiling(day(Date)/7)==1)) %>%
  ggplot(.,aes(x=Date, y=WindSpeed, color = Label)) + geom_line() +
  scale_color_discrete(name = "Label",labels = c("Raw Wind Speed","Adjusted Wind Speed (IEC)"))

# plot(wtg$WindSpeed[1:100], type = "l")
# lines(wtg$WindSpeed_IEC[1:100], type = "l", col="red")

#Reading Power Curve Data From Manufacturer
g132 = read.table("g132_powercurve.csv",sep = ";",header = T)
colnames(g132)[1] = "WindSpeed"

#Let's have a look to create a perfectly overfit model to the manufacturers power curve
g132 %>% select(WindSpeed,ad_1225) %>% ggplot(.,aes(x=WindSpeed,y=ad_1225)) +
  geom_point() + stat_smooth(method = "loess",span=0.1, lwd = 0.7,aes(color = "0.1"),se = F) +
  stat_smooth(method = "loess", span = 0.2, aes(color="0.2"), se = F)+ 
  stat_smooth(method = "loess", span = 0.5, aes(color="0.5"), se = F, lwd = 0.7)+
  stat_smooth(method = "loess", span = 0.7, aes(color="0.7"), se = F)+
  scale_colour_manual(name="Spans", values=c("royalblue", "darkred","purple4","orange")) +
  ggtitle("Loess - Span Deciding")
  
g132 %>% select(WindSpeed,ad_1225) %>% ggplot(.,aes(x=WindSpeed,y=ad_1225)) +
  geom_point() + geom_smooth(method = "loess",span=0.1, lwd = 0.5,aes(color = "0.1"),se = F,
                             degree = 2,family = "symmetrical")  +
  geom_smooth(method = "loess",span=0.2, lwd = 0.7,aes(color = "0.2"),se = F,
              degree = 2,family = "gaussian",)


g132_gathered = g132 %>% gather(key = "AirDensity",value = "Power",-WindSpeed)
#ifelse(g132_gathered$AirDensity>1000,
library(readr)
g132_gathered$AirDensity = parse_number(g132_gathered$AirDensity)


# sapply(g132_gathered[2], function(x) {if (x>1000) {x = x/1000} else {x = x/100}})
# Convert data to real air density
g132_gathered[2]=lapply(g132_gathered[2], function(x) {ifelse(x>1000,x/1000,x/100)})

#Arrange data (sort from low to high air density.
# str(g132_gathered)
g132_gathered = g132_gathered %>% arrange(AirDensity,WindSpeed)

library(plotly)
plot_ly(x=g132_gathered$WindSpeed,y=g132_gathered$AirDensity,z=g132_gathered$Power)

library(rsm)
loess_surf <- lm(Power ~ poly(WindSpeed, AirDensity, degree = 2), data = g132_gathered)
loess_surf <- lm(Power ~ poly(WindSpeed*AirDensity,degree = 25), data = g132_gathered)
persp(loess_surf,WindSpeed~AirDensity,zlab = "Power")
predict(loess_surf,g132_gathered)

loess_surf <- loess(Power ~ WindSpeed*AirDensity, data = g132_gathered, degree = 2, span = 0.1,
                    family = "gaussian")

marigin = list(WindSpeed = seq(from = min(g132_gathered$WindSpeed), to = max(g132_gathered$WindSpeed), 
                               by = 1), 
               AirDensity = seq(from = min(g132_gathered$AirDensity), to = max(g132_gathered$AirDensity),
               by = 0.01))

loess_pred = predict(loess_surf,newdata = expand.grid(marigin), se = T)
loess_pred$fit

library(rgl)
plot3d(g132_gathered$WindSpeed, g132_gathered$AirDensity, g132_gathered$Power, 
       type="s", size=0.75, lit=FALSE,col="red")
surface3d(marigin[[1]], marigin[[2]], loess_pred[[1]],
          alpha=0.4, front="lines", back="lines")


pred_table = matrix(loess_surf$fitted, nrow=length(g132_gathered$WindSpeed), length(g132_gathered$AirDensity))

loess_surf$fitted
  open3d(useFreeType=par3d("useFreeType"))
surface3d(age,year,as.matrix(resultTable), col=col)

predict(loess_surf, data.frame(WindSpeed = 10, AirDensity = 1.06))
# dene=rsm(Power ~ FO(WindSpeed, AirDensity),data = g132_gathered)
# 
# persp(dene,AirDensity ~ WindSpeed)
persp(loess_surf,AirDensity ~ WindSpeed)

predict(loess_surf,WindSpeed = 1, AirDensity=5)

veri$TI_t1=veri$ws_std/veri$ws
veri$ws_turb=veri$ws_iec*(1+3*(veri$TI_t1)^2)^(1/3)
