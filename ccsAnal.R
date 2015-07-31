ccs1Cats = names(table(tbdat$CCS.Level.1))
ccs2CatCount = 0
for (cname in ccs1Cats) {
  ccs2CatCount = ccs2CatCount + dim(table(droplevels(tbdat$CCS.Level.2[tbdat$CCS.Level.1==cname])))
}
print(ccs2CatCount)
print(dim(table(tbdat$CCS.Level.2)))

ccs2Cats = names(table(tbdat$CCS.Level.2))
ccs3CatCount = 0
for (cname in ccs2Cats) {
  ccs3CatCount = ccs3CatCount + dim(table(droplevels(tbdat$CCS.Level.3[tbdat$CCS.Level.2==cname])))
}
print(ccs3CatCount)
print(dim(table(tbdat$CCS.Level.3)))
