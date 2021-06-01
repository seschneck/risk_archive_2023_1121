mTD = matrix(c(1,1,1,1,1,0,0,0,0,0,0,
               1,0,2,1,0,1,1,1,1,1,0,
               1,0,0,0,1,1,1,0,0,0,1),nrow=11,ncol=3,
             dimnames=(list(c("error","invalid","message","file","format","unable",
                           "to","open","using","path","variable"),
                      c("doc1","doc2","doc3"))))

mDT = t(mTD)

demo_svd <- svd(mDT)

U <- demo_svd$u   #singular vectors for rows (docs)

D <- demo_svd$d   #Singular values

V <- demo_svd$v   #Singular vectors for cols (Terms)

k = 2
V_k = V[,1:k]

SVDs = mDT %*% V_k
SVDs
