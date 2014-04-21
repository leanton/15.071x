#!/usr/bin/Rscript

fac.train = train
int.train = train

for (i in names(fac.train)) {
  int.train[,i] = as.numeric(fac.train[,i]) - 1
}

fac.test = test
int.test = test

for (i in names(fac.test)) {
  int.test[,i] = as.numeric(fac.test[,i]) - 1
}