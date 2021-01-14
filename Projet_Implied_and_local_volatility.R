library(readr)
microsoft <- read_delim("Master 2/Calcul stochastique/microsoft.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

colnames(microsoft)[3] = "market"

## Ci dessous la fonction qui détermine le prix du call ou put: 
call_bsm = function (So,K,r,T,type,sig){
  d1 = (log(So/K) + (r+ (sig*sig)/2)*T)/(sig*sqrt(T))
  d2 = d1 - sig*sqrt(T)
  if (type == "Call")
  {price <- So*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return (price)}
  else if (type == "Put")
  {price  <- -So*pnorm(-d1) + K*exp(-r*T)*pnorm(-d2)
  return (price)}
}

## Implied volatilty function. 
implied.vol <- function(So, K, T, r, market,flag){
a = 0.0001
b = 1
N = 1
tol = 1e-04  
err_func = function(So,K,r,T,flag,sigma,market)
{return(call_bsm(So,K,r,T,flag,sigma)-market) }
while (N<=10000)
{
  sg = (a+b)/2
  N= N+1
  if (abs(err_func(So,K,r,T,flag,sg,market)) < tol |((b-a)/2)<tol)
  {return(sg)}
  else if(sign(err_func(So,K,r,T,flag,sg,market)) == sign(err_func(So,K,r,T,flag,a,market)))
  {a =sg}
  else 
  {b =sg}
}
}  

# Initialisation de la volatilité implicite:
imp_vol = c()

# Calcul de la volatilité implicite pour chaque call:
So=215
for (i in (1:length(microsoft$TTM))){
  imp_vol[i] = implied.vol(So , microsoft$Strike[i],  microsoft$TTM[i],0.0009, microsoft$market[i],"Call")
}
View(imp_vol)

# Fusion des résultats avec notre dataframe
microsoft$imp_vol = imp_vol


microsoft$imp_vol = round(microsoft$imp_vol,6)
microsoft = microsoft[!(microsoft$imp_vol== 0.999939),] # On enlève les valeurs extrêmes
head(microsoft)
microsoft
library(ggplot2)
library(plotly)
packageVersion('plotly')

Implied_vol <- plot_ly(x = unique(microsoft$TTM), y = microsoft$Strike, 
             z = matrix(microsoft$imp_vol,nrow=length(unique(microsoft$TTM))))%>% add_surface()
Implied_vol <- Implied_vol %>% layout(title = "Implied volatility using Black and Scholes",
                                           scene = list(
                                             xaxis = list(title = "Maturité"),
                                             yaxis = list(title = "Strike"),
                                             zaxis = list(title = "Implied Volatility")
                                           ))


#install.packages("akima")
#install.packages("fields")
library(akima)
interpolate = interp(microsoft$TTM, microsoft$Strike, microsoft$imp_vol,duplicate="strip")
Implied_vol_interpolated <- plot_ly(x = interpolate$x, y = interpolate$y, 
             z = interpolate$z)%>% add_surface()
Implied_vol_interpolated <- Model_CEVinterpo %>% layout(title = "Implied volatility using Black and Scholes (interpolated)",
                                                       scene = list(
                                                         xaxis = list(title = "Maturité"),
                                                         yaxis = list(title = "Strike"),
                                                         zaxis = list(title = "Implied Volatility")
                                                       ))




### Local volatility using Dupire Formula, avec la méthode des différences finies

increment = 1.5
deltac_by_deltat = c()
deltac_by_deltak = c()
delta2c_by_deltak2 = c()
local_volatility = c()
for (i in (1:length(microsoft$TTM))) {
  deltac_by_deltat[i] = (call_bsm(So = 215, microsoft$Strike[i],r = 0.0009, microsoft$TTM[i] + increment, type = "Call", sig = microsoft$imp_vol[i]) - call_bsm(So = 215, microsoft$Strike[i], r = 0.0009, microsoft$TTM[i], type = "Call", sig = microsoft$imp_vol[i]))/(microsoft$TTM[i] + increment - microsoft$TTM[i])
  
  deltac_by_deltak[i] = (call_bsm(So = 215, microsoft$Strike[i] + 
                                    increment, r = 0.0009, microsoft$TTM[i], type = "Call", sig = microsoft$imp_vol[i]) - 
                           call_bsm(So = 215, microsoft$Strike[i], r = 0.0009, microsoft$TTM[i], 
                                    type = "Call", sig = microsoft$imp_vol[i]))/(microsoft$Strike[i] + 
                                                                            increment - microsoft$Strike[i])
  
  delta2c_by_deltak2[i] = (call_bsm(So = 215, microsoft$Strike[i] + 
                                      increment, r = 0.0009, microsoft$TTM[i], type = "Call", sig = microsoft$imp_vol[i]) - 
                             2 * call_bsm(So = 215, microsoft$Strike[i], r = 0.0009, microsoft$TTM[i], 
                                          type = "Call", sig = microsoft$imp_vol[i]) + call_bsm(So = 215, 
                                                                                         microsoft$Strike[i] - increment, r = 0.0009, microsoft$TTM[i], type = "Call", 
                                                                                         sig = microsoft$imp_vol[i]))/(microsoft$Strike[i] + increment - microsoft$Strike[i])
  
  local_volatility[i] = sqrt((deltac_by_deltat[i] + 0.0009 * 
                                microsoft$Strike[i] * deltac_by_deltak[i])/(0.5 * microsoft$Strike[i] * 
                                                                       microsoft$Strike[i] * delta2c_by_deltak2[i]))
  
}
microsoft$local_vol = local_volatility
microsoft = microsoft[!(microsoft$local_vol >= 1), ]
head(microsoft)



iv_cp = c() # Initialisation des prix du call pour volatilité implicite
Lv_cp = c() # Initialisation des prix du call pour volatilité locale
for (i in 1:length(microsoft$Strike)){
  iv_cp[i] = call_bsm(So=215,microsoft$Strike[i],r=0.0009,microsoft$TTM[i],
                      type="Call",sig=microsoft$imp_vol[i])
  Lv_cp[i] = call_bsm(So=215,microsoft$Strike[i],r=0.0009,microsoft$TTM[i],
                      type="Call",sig=microsoft$local_vol[i])}
microsoft$iv_cp = iv_cp
microsoft$Lv_cp = Lv_cp
tail(microsoft)


## représentation graphique vol locale sous Dupire
Local_volatility <- plot_ly(x = unique(microsoft$TTM),  y = microsoft$Strike, 
             z = matrix(microsoft$local_vol,nrow=length(unique(microsoft$TTM))))%>% add_surface() 
Local_volatility <- Local_volatility %>% layout(title = "Local volatility using Dupire Formula ",
                                  scene = list(
                                    xaxis = list(title = "Maturité"),
                                    yaxis = list(title = "Strike"),
                                    zaxis = list(title = "Local Volatility")
                                  ))
Local_volatility

library(akima)
interpolate = interp(microsoft$TTM, microsoft$Strike, microsoft$local_vol,duplicate="strip")
Local_volatility_interpo <- plot_ly(x = interpolate$x, y = interpolate$y, 
             z = interpolate$z)%>% add_surface()
Local_volatility_interpo <- Local_volatility_interpo %>% layout(title = "Local volatility using Dupire Formula (interpolated)",
                                         scene = list(
                                           xaxis = list(title = "Maturité"),
                                           yaxis = list(title = "Strike"),
                                           zaxis = list(title = "Local Volatility")
                                         ))
Implied_vol
Implied_vol_interpolated
Local_volatility
Local_volatility_interpo
View(microsoft)