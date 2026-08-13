

## code to create `LEMURS_dupe_df` dataset

## Izzy Smith, 2026
## version 20260813



## making = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

val_n <- c(1, 22, NA)



perm.val <- expand.grid(variablC = "a",
                        variablY = val_n,
                        variablX = val_n,

                        stringsAsFactors=FALSE)


perm.val <- perm.val[, c("variablX","variablY","variablC")]
# format(perm.val)


## + + + + + + +


nr <- nrow(perm.val)


set.seed(77)
recordID <- sample(state.abb, nr+1, replace=FALSE) ## nr+1 gives extra ID (used later)


perm.val2 <- cbind(recordID=recordID[1:nr],
                   rec_date=rep(as.POSIXct("2000-01-01"), nr),
                   perm.val)
# format(perm.val2)


## + + + + + + +


nr2 <- 3*nr + 2


perm.val2.t <- perm.val2
perm.val2.t[,"variablC"] <- "txt"
perm.val2.t[,"rec_date"] <- perm.val2[,"rec_date"] + (24*60*60)*1


perm.val2.1 <- perm.val2
perm.val2.1[,c("variablX","variablY")] <- 1
perm.val2.1[,"variablC"] <- NA
perm.val2.1[,"rec_date"] <- perm.val2[,"rec_date"] - (24*60*60)*1

perm.val2.N <- data.frame(recordID=recordID[nr+1], ## extra ID used here
                          variablX=NA,
                          variablY=NA,
                          variablC=NA,
                          rec_date=as.POSIXct(c("2000-01-01","2000-02-03")))


set.seed(777)
var_R <- paste0(sample(as.hexmode(160:319), nr2),
                round(rnorm(nr2, sd=5),2),
                sample(LETTERS, nr2, replace=TRUE),
                rpois(nr2, lambda=10))


perm.val3 <- cbind(surveyID=3,
                   rbind(perm.val2.1, perm.val2, perm.val2.t, perm.val2.N),
                   variablR=var_R)
# format(perm.val3)


## + + + + + + +


perm.val4 <- cbind(perm.val3,
                   finished = ifelse(rowSums(is.na(perm.val3[,c("variablX", "variablY", "variablC")]))==0, 1, 0),
                   progress = round((3-rowSums(is.na(perm.val3[,c("variablX", "variablY", "variablC")])))/3, 2)*100)
# format(perm.val4)





## sorting = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

order.ID <- unique(perm.val4$recordID)
order.vC <- c(     NA,  "txt",    "a")


order.sort <- order(factor(perm.val4$recordID, levels=order.ID),
                    perm.val4$variablX,
                    perm.val4$variablY,
                    -factor(perm.val4$variablC, levels=order.vC)) ## ignore warning; no alternative


order.cols <- c("surveyID", "recordID", "rec_date",
                "finished", "progress", "variablR",
                "variablX", "variablY", "variablC")

perm.val4.sort <- perm.val4[order.sort, order.cols, drop = FALSE]
rownames(perm.val4.sort) <- NULL
# format(perm.val4.sort)





## numbering = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

set.seed(7777)
perm.val4.sort.num <- cbind(perm.val4.sort,
                            orig_row = sample(1:100, nrow(perm.val4.sort)))

# format(perm.val4.sort.num)





## exporting = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

LEMURS_dupe_df <- perm.val4.sort.num

usethis::use_data(LEMURS_dupe_df, overwrite=TRUE)



