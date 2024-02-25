set.seed(13)
g = rnorm(5,0.05,0.01)
g

revenue_0 = 100

revenue = vector("numeric",6)

revenue[1]=revenue_0

for (i in 2:6){
  revenue[i]=revenue[i-1]*(1+g[i-1])
}

revenue

revenue=revenue_0*cumprod(1+g)
revenue

revenue=revenue_0*cumprod(c(1,1+g))
revenue

cumprod(1+g)
cumprod(c(1,1+g))

margin=rnorm(5,0.15,0.03)
margin

nopat = revenue*margin

turnover = 1.3
assets = revenue/turnover
assets[5]=NA
assets

terminal_g = rnorm(1,0.03,0.01)
terminal_g

ROIC_5 = margin[5]*turnover
ROIC_5

inv_rate_5=terminal_g/ROIC_5
inv_rate_5

net_inv_5 = nopat[5]*inv_rate_5
net_inv_5

diff(assets)

net_inv=vector("numeric",5)
net_inv[1]=assets[1]-assets_0
net_inv[2:5]=diff(assets)
net_inv[5]=net_inv_5
net_inv

fcf = nopat - net_inv
fcf

wacc = 0.13
terminal_value = fcf[5]*(1+terminal_g)/(wacc-terminal_g)
terminal_value

discount_factors = 1/(1+wacc)^(1:5)
discount_factors

final_fcf = fcf + c(rep(0,4),terminal_value)
final_fcf

enterprise_value = sum(final_fcf*discount_factors)
enterprise_value


###s# Create a valuation function
ent_value = function(revenue_0=100,assets_0=80,g_exp=0.05,g_std=0.01,margin_exp=0.15,margin_std=0.03,
                     turnover=1.3,terminal_g_exp=0.03,terminal_g_std=0.01,wacc=0.13){
  g = rnorm(5,g_exp,g_std)
  revenue = vector("numeric",6)
  revenue[1]=revenue_0
  margin=rnorm(5,margin_exp,margin_std)
  revenue=revenue_0*cumprod(1+g)
  nopat = revenue*margin
  assets = revenue/turnover
  assets[5]=NA
  terminal_g = rnorm(1,terminal_g_exp,terminal_g_std)
  ROIC_5 = margin[5]*turnover
  inv_rate_5=terminal_g/ROIC_5
  net_inv_5 = nopat[5]*inv_rate_5
  net_inv=vector("numeric",5)
  net_inv[1]=assets[1]-assets_0
  net_inv[2:5]=diff(assets)
  net_inv[5]=net_inv_5
  fcf = nopat - net_inv
  terminal_value = fcf[5]*(1+terminal_g)/(wacc-terminal_g)
  discount_factors = 1/(1+wacc)^(1:5)
  final_fcf = fcf + c(rep(0,4),terminal_value)
  enterprise_value = max(0,sum(final_fcf*discount_factors))
  return(enterprise_value)
}

ent_value()

values = vector('numeric',1000000)
for (i in 1:1000000){
  values[i]=ent_value()
}

summary(values)

plot(density(values),col='indian red',
     main="Distribution of Enterprise Value",
     xlab="Enterprise Value",ylab="Probability")

quantile(values,0.05)

compute_return = function(equity_0=85,DE=0.5,wacc=0.13){
  debt_value = DE/(1+DE)
  enterprise_value_1=ent_value()*(1+wacc)
  equity_1 = enterprise_value_1*(1-debt_value)
  equity_return = equity_1/equity_0 -1
  return(equity_return)
}
compute_return()

returns = vector('numeric',1000000)

for (i in 1:1000000){
  returns[i]=compute_return()
}

summary(returns)

plot(density(returns),col='indian red',
     main="Distribution of Stock Returns",
     xlab="Stock Returns",ylab="Probability")



ent_value_multiple = function(revenue_0=100,assets_0=80,g_exp=0.05,g_std=0.01,margin_exp=0.15,margin_std=0.03,
                              turnover=1.3,terminal_g_exp=0.03,terminal_g_std=0.01,wacc=0.13){
  g = rnorm(5,g_exp,g_std)
  revenue = vector("numeric",6)
  revenue[1]=revenue_0
  margin=rnorm(5,margin_exp,margin_std)
  revenue=revenue_0*cumprod(1+g)
  nopat = revenue*margin
  assets = revenue/turnover
  assets[5]=NA
  terminal_g = rnorm(1,terminal_g_exp,terminal_g_std)
  ROIC_5 = margin[5]*turnover
  inv_rate_5=terminal_g/ROIC_5
  net_inv_5 = nopat[5]*inv_rate_5
  net_inv=vector("numeric",5)
  net_inv[1]=assets[1]-assets_0
  net_inv[2:5]=diff(assets)
  net_inv[5]=net_inv_5
  fcf = nopat - net_inv
  terminal_value = fcf[5]*(1+terminal_g)/(wacc-terminal_g)
  discount_factors = 1/(1+wacc)^(1:5)
  final_fcf = fcf + c(rep(0,4),terminal_value)
  enterprise_value = max(0,sum(final_fcf*discount_factors))
  ev_sales = enterprise_value/revenue[1]
  ev_nopat = enterprise_value/nopat[1]
  result = list(value=enterprise_value,ev_sales=ev_sales,ev_nopat = ev_nopat)
  return(result)
}

ent_value_multiple()

ent_value_multiple()$value
ent_value_multiple()$ev_sales
ent_value_multiple()$ev_nopat


