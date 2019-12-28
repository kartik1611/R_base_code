rm(list=ls(all=TRUE))
setwd("E:/20160701_Academics/Batch18/CSE7219c/Sims")

reference=readLines("warandpeace.txt")
reference=toupper(reference)

trans.mat=matrix(0,27,27)
rownames(trans.mat)=colnames(trans.mat)=
  c(toupper(letters),"")
lastletter=""

for (ln in 1:length(reference)) {
  if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
  
  for (pos in 1:nchar(reference[ln])) {
    curletter=substring(reference[ln],pos,pos)
    if (curletter %in% toupper(letters)) {
      trans.mat[rownames(trans.mat)==lastletter,
                colnames(trans.mat)==curletter]=
        trans.mat[rownames(trans.mat)==lastletter,
                  colnames(trans.mat)==curletter]+1
      lastletter=curletter
    } 
    else {
      if (lastletter!="") {
        trans.mat[rownames(trans.mat)==lastletter,27]=
          trans.mat[rownames(trans.mat)==lastletter,27]+1
        lastletter=""
      }
    }
  }
  curletter=""
  if (lastletter!="") {
    trans.mat[rownames(trans.mat)==lastletter,27]=
      trans.mat[rownames(trans.mat)==lastletter,27]+1
  }
  lastletter=""
}

trans.prob.mat=sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/")

decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letters)) {
      substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
    }
  }
  decoded
}


log.prob <- function(mapping,decoded) {
  logprob=0
  
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letters)) {
      logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,
                                         colnames(trans.mat)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
        lastletter=""
      }
    }
  }
  
  if (lastletter!="") {
    logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
    lastletter=""
  }
  logprob
}

coded="WGTG IJRAQJA IAAEI TS UIA GMM GBGRMGOMA GQW HAMABGQT WGTG 
TS ALLAJTRBAMP TAMM G ITSHP TNGT JGQ OA AGIRMP UQWAHITSSW OP 
QSQ-CHGJTRTRSQAHI.  NAQJA, VRTNSUT WSUOT, RT RI TNA IAXRAIT KSO 
SL TNA QAXT WAJGWA.  KUDC RQ QSV GQW OAJSDA G CGHT  SL TNRI 
AXJRTRQZ KSUHQAP.  ZSSW WGTG IJRAQTRITI VRMM QST KUIT GWWHAII 
OUIRQAII CHSOMADI, TNAP VRMM CRJE TNA HRZNT CHSOMADI TNGT NGBA 
TNA DSIT BGMUA TS TNA SHZGQRFGTRSQ."

#coded <- "hQIMA1tQlAt53r41ARAAsCwY0VverVliy5i29NafjAEhFpmwDDAHVdzOYtnGbOHL
#Hi1t1hgRPe5NBD+AnDENmUbJf4hNxH88Uh4qTqy8ja4qAWyRSJXENijZs2Pjhv+8
#ovJhDSDK3N8bGDcM7XS7o1FGrLJtpV2CqP4DP4rSr4fcQz1ZnRWrnBP9XI6FAbEp
#XXRtW6mbtPWTLfgvn91Ka3aJGegXl6rFYeqmXgmZiPYrnmNSAgFGSKg+Er2Kz+jE
#sl4tS/hqP9vhAAWWCOvT7U5LMuDGjawsBXjHTPA9FokP07euxRPxMraz5FmrtZYb
#erFhkMlW5IV5zG1BEO5TetyM66hAZid/QwdFzlDW3wHQoYJdJWcZEYY0tGWbL3+h
#mfgcNX9gwnn0o0fU6xqpn/cApv3uZUkNIFPxQXGzHqrs/Vv215ut8zwLI17G/FIF
#McuOAP"

mapping=sample(toupper(letters)) # initialize a random mapping
i=1
iters=2000
cur.decode=decode(mapping,coded)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2) # select 2 letters to switch
  prop.mapping=mapping
  prop.mapping[proposal[1]]=mapping[proposal[2]]
  prop.mapping[proposal[2]]=mapping[proposal[1]]
  
  prop.decode=decode(prop.mapping,coded)
  prop.loglike=log.prob(prop.mapping,prop.decode)
  
  
  if (runif(1)<exp(prop.loglike-cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike>max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    cat(i,cur.decode,"\n")
    i=i+1
  }
}
