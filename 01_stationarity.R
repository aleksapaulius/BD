
# STATIONARITY -----------------------------------------------------------------

# Phillips-Perron unit root test
# H0 - turi unit root, H1 - neturi vienetinės šaknies (galimai stacionaru)

pp.table <- data.frame(variable = all.variables.stat$variable,
                       kintamasis = all.variables.stat$kintamasis,
                       statistika = NA, 
                       kritinė_reikšmė = NA,
                       nulinė_hipotezė = NA)

for (i in unique(pp.table$variable)) {
  pp.data <- ur.pp(alldata.m[,i], type="Z-tau", model="trend", lags="short")
  pp.table[pp.table$variable == i,'statistika'] <- pp.data@teststat
  pp.table[pp.table$variable == i,'kritinė_reikšmė'] <- pp.data@cval[2]
  if (pp.data@teststat < pp.data@cval[2]) { # jei TRUE, tai atmetam nulinę hipotezę apie vienetinę šaktį (galimai stacionaru)
    pp.table[pp.table$variable == i,'nulinė_hipotezė'] <- 'atmetama'
  } else {
    pp.table[pp.table$variable == i,'nulinė_hipotezė'] <- 'priimama'
  }
}

pp.table <- pp.table[,!(names(pp.table) %in% 'variable')]


# KPSS unit root test
# H0 - is stationarity, H1 - unit root

kpss.table <- data.frame(variable = all.variables.stat$variable,
                       kintamasis = all.variables.stat$kintamasis,
                       statistika = NA, 
                       kritinė_reikšmė = NA,
                       nulinė_hipotezė = NA)

for (i in unique(kpss.table$variable)) {
  kpss.data <- ur.kpss(alldata.m[,i], type="tau")
  kpss.table[kpss.table$variable == i,'statistika'] <- kpss.data@teststat
  kpss.table[kpss.table$variable == i,'kritinė_reikšmė'] <- kpss.data@cval[2]
  if (kpss.data@teststat > kpss.data@cval[2]) { # jei TRUE, tai atmetam nulinę hipotezę
    kpss.table[kpss.table$variable == i,'nulinė_hipotezė'] <- 'atmetama'
  } else {
    kpss.table[kpss.table$variable == i,'nulinė_hipotezė'] <- 'priimama'
  }
}

kpss.table <- kpss.table[,!(names(kpss.table) %in% 'variable')]



# The null hypothesis of the ADF test is the opposite of the KPSS test. 
# Thus, a way to proceed is to test first for the null of stationarity: 
# if the null hypothesis is rejected then conclude that the series is not stationary, 
# otherwise test for the null of a unit root by means of the ADF test. In the latter case, 
# if the null of a unit root is rejected then conclude that the series is stationary, 
# otherwise the data are not informative enough to reach a conclusion since none of the hypotheses could be rejected.


# Notice that the KPSS test is a right tailed test (the critical region is in the right tail of the distribution, 
# i.e., values of the test statistic larger than the tabulated critical value involve rejection of the null hypothesis) 
# while the ADF test is a left tailed test.


# One way to proceed is the following: Start by applying the ADF test.
# 1. If the null of a unit root is rejected we are done. The trend (if any) can be represented by a deterministic linear trend.
# 2. If the null of the ADF test is not rejected then we apply the KPSS test (where the null hypothesis is the opposite, 
# stationarity or stationarity around a linear trend).
#   a) If the null of the KPSS test is rejected then we conclude that there is a unit root 
#     and work with the first differences of the data. Upon the first differences of the series 
#     we can test the significance of other regressors or choose an ARMA model.
#   b) If the null of the KPSS test is not rejected then we would have to say that the data are 
#     not much informative because we weren't able to reject none the of the null hypotheses. 
#     In this case it may be safer to work with the first differences of the series.




