# R - Dynamical Multivariate Wind Turbine Power Curve Model and IEC Corrections

This script provides creating a three dimensional dynamical wind turbine power curve model based on the power curve of the manufacturer by using air density and wind speed together to get active power. Besides, it also has IEC correction functions which should be applied to the raw wind speed data of the nacelle mounted anemometer (wind turbine).

First, read the turbine based data. Should have wind speed, standart deviation of wind speed, power, temperature, air pressure, relative humidity (if any).

```
wtg = read.csv("windfarm_raw.csv",header = T, stringsAsFactors = F)
```

Since wind turbine data doesn't have air density variable. It has to be calculated and here a function has been defined. This function uses temperature, pressure and relative humidity to calculate air density. However; if you don't bring relative humidity then, dry air density will be calculated. Make sure to use Celcius units for Temperature, hPa or mb units for air Pressure and % for relative humidity.

```
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
```
