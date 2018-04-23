# calculate a CI for difference in Pearson's r between two independent correlations
#   using the Fisher z transformation
# code from website: https://seriousstats.wordpress.com/2012/02/05/comparing-correlations/
#   implements Zou's (2007) method for comparing confidence intervals (where CIs are calculated using Fisher's Z transform method)
#   for more "robust" alternative, the author mentions Wilcox's method for using bootstrapped CIs
#
# additional references: 
#   Baguley, T. (2012, in press). Serious stats: A guide to advanced statistics for the behavioral sciences. Basingstoke: Palgrave.
#   Zou, G. Y. (2007). Toward using confidence intervals to compare correlations. Psychological Methods, 12, 399-413.
#   Wilcox, R. R. (2009). Comparing Pearson correlations: Dealing with heteroscedascity and non-normality. Communications in Statistics - Simulation & Computation, 38, 2220-2234.


#### compare independent correlations -----
  
  # Fisher z transformation -- rz.ci is used by the r.ind.ci, which spits out the difference between independent correlations
  rz.ci <- function(r, N, conf.level = 0.95) {
    zr.se <- 1/(N - 3)^0.5
    moe <- qnorm(1 - (1 - conf.level)/2) * zr.se
    zu <- atanh(r) + moe
    zl <- atanh(r) - moe
    tanh(c(zl, zu))
  }
  
  
  #construct a CI for the difference between two independent correlations. See section 6.6.2 of Serious stats or Zou (2007) for further details and a worked example. My function from section 6.7.6 is reproduced here:
  r.ind.ci <- function(r1, r2, n1, n2=n1, conf.level = 0.95) {
    L1 <- rz.ci(r1, n1, conf.level = conf.level)[1]
    U1 <- rz.ci(r1, n1, conf.level = conf.level)[2]
    L2 <- rz.ci(r2, n2, conf.level = conf.level)[1]
    U2 <- rz.ci(r2, n2, conf.level = conf.level)[2]
    lower <- r1 - r2 - ((r1 - L1)^2 + (U2 - r2)^2)^0.5
    upper <- r1 - r2 + ((U1 - r1)^2 + (r2 - L2)^2)^0.5
    c(lower, upper)
  }
  


#### compare dependent overlapping correlations -----
# The r.dol.ci() function takes three correlations as input - the correlations of interest (e.g., rXY and rXZ) and the correlation between the non-overlapping variables (e.g., rYZ). Also required is the sample size (often identical for both correlations).

rho.rxy.rxz <- function(rxy, rxz, ryz) {
  num <- (ryz-1/2*rxy*rxz)*(1-rxy^2-rxz^2-ryz^2)+ryz^3
  den <- (1 - rxy^2) * (1 - rxz^2)
  num/den
}

r.dol.ci <- function(r12, r13, r23, n, conf.level = 0.95) {
  L1 <- rz.ci(r12, n, conf.level = conf.level)[1]
  U1 <- rz.ci(r12, n, conf.level = conf.level)[2]
  L2 <- rz.ci(r13, n, conf.level = conf.level)[1]
  U2 <- rz.ci(r13, n, conf.level = conf.level)[2]
  rho.r12.r13 <- rho.rxy.rxz(r12, r13, r23)
  lower <- r12-r13-((r12-L1)^2+(U2-r13)^2-2*rho.r12.r13*(r12-L1)*(U2- r13))^0.5
  upper <- r12-r13+((U1-r12)^2+(r13-L2)^2-2*rho.r12.r13*(U1-r12)*(r13-L2))^0.5
  c(lower, upper)
} 

# input from example 2 of Zou (2007, p.409)
r.dol.ci(.396, .179, .088, 66)


#### compare dependent NONoverlapping correlations -----

rho.rab.rcd <- function(rab, rac, rad, rbc, rbd, rcd) {
  num <- 1/2*rab*rcd * (rac^2 + rad^2 + rbc^2 + rbd^2) + rac*rbd + rad*rbc - (rab*rac*rad + rab*rbc*rbd + rac*rbc*rcd + rad*rbd*rcd)
  den <- (1 - rab^2) * (1 - rcd^2)
  num/den
}

r.dnol.ci <- function(r12, r13, r14, r23, r24, r34, n12, n34=n12, conf.level=0.95) {
  L1 <- rz.ci(r12, n12, conf.level = conf.level)[1]
  U1 <- rz.ci(r12, n12, conf.level = conf.level)[2]
  L2 <- rz.ci(r34, n34, conf.level = conf.level)[1]
  U2 <- rz.ci(r34, n34, conf.level = conf.level)[2]
  rho.r12.r34 <- rho.rab.rcd(r12, r13, r14, r23, r24, r34)
  lower <- r12 - r34 - ((r12 - L1)^2 + (U2 - r34)^2 - 2 * rho.r12.r34 * (r12 - L1) * (U2 - r34))^0.5
  upper <- r12 - r34 + ((U1 - r12)^2 + (r34 - L2)^2 - 2 * rho.r12.r34 * (U1 - r12) * (r34 - L2))^0.5
  c(lower, upper)
} 

# from example 3 of Zou (2007, p.409-10)

r.dnol.ci(.396, .208, .143, .023, .423, .189, 66)