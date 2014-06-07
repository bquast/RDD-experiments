# Bastiaan Quast
# playing with Matthieu's RDD package

# load the package
library(RDDtools)

# load example data
# from this package?
data(Lee2008)

# declare as RDD-class object
Lee2008_rdd <- RDDdata(y = Lee2008$y, x = Lee2008$x, cutpoint = 0)

# inspect result
summary(Lee2008_rdd) # this should be like the lm summary
head(Lee2008_rdd)
str(Lee2008_rdd)
str(Lee2008)
attributes(Lee2008_rdd)$type
typeof(attributes(Lee2008_rdd)$type) # this could a logical (fuzzy: TRUE/FALSE)

# more inspection
plot(Lee2008_rdd)

# parametric estimation
reg_para <- RDDreg_lm(RDDobject = Lee2008_rdd, order = 4)
print(reg_para)
summary(reg_para) # this summary is nice
plot(reg_para)

# non-parametric
bw_ik <- RDDbw_IK(Lee2008_rdd)
reg_nonpara <- RDDreg_np(RDDobject = Lee2008_rdd, bw = bw_ik)
print(reg_nonpara)
plot(x = reg_nonpara)

# sensitivity tests
plotSensi(reg_nonpara, from = 0.05, to = 1, by = 0.1)

# placebo test
plotPlacebo(reg_nonpara)

# McCrary test
# disc. comes from design
dens_test(reg_nonpara)
