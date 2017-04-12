# use admb with R
# install.packages("R2admb")

library(R2admb)

X <- "admb/polio"

setup_admb()
compile_admb(X, re = TRUE)
run_admb(X)


#################
setup_admb()
file.copy(system.file("tplfiles","ReedfrogSizepred0.tpl",package="R2admb"),"tadpole.tpl")
tadpoledat <-
  data.frame(TBL = rep(c(9,12,21,25,37),each=3),
    Kill = c(0,2,1,3,4,5,0,0,0,0,1,0,0,0,0L),
    nexposed=rep(10,15))
m1 <- do_admb("tadpole",
  data=c(list(nobs=15),tadpoledat),
  params=list(c=0.45,d=13,g=1),
  bounds=list(c=c(0,1),d=c(0,50),g=c(-1,25)),
  run.opts=run.control(checkparam="write",
    checkdata="write",clean="all"))
unlink("tadpole.tpl")
