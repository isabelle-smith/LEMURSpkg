

## code to prepare `LEMURS_qualtrics_csv_...` datasets

## Izzy Smith, 2026



## sources = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

## Survey:
##    format  = see LEMURS_Test_Survey.qsf for Qualtrics details (can be imported)
##    data    = 5/7 = 'Generate test responses' feature (with 'Allow unanswered questions')
##    data    = 2/7 = 'Generate test responses' feature (with no 'Allow unanswered questions')

## Directory:
##    names   = Frequently Occurring Names from Census 1990
##    https://www.census.gov/topics/population/genealogy/data/1990_census/1990_census_namefiles.html





## reading = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

df_qsur <- readr::read_csv(file="data-raw/LEMURS+Test_July+22%2C+2026_13.24.csv",        ## automatic Qualtrics output (all test responses)
                           progress=FALSE, show_col_types=FALSE) |> as.data.frame()
df_qdir <- readr::read_csv(file="data-raw/LEMURS_Test_Directory.csv",                    ## columns: FirstName, LastName, Email, record_id, PID, uvmid
                           progress=FALSE, show_col_types=FALSE) |> as.data.frame()





## combining = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

## adding contact data to test responses:

df <- df_qsur

df[3:6, c("RecipientFirstName", "RecipientLastName", "RecipientEmail")] <- df_qdir[   , c("FirstName", "LastName", "Email")]
df[8:9, c("RecipientFirstName", "RecipientLastName", "RecipientEmail")] <- df_qdir[1:2, c("FirstName", "LastName", "Email")]

df$record_id  <- c('record_id',  '{"ImportId":"record_id"}',  df_qdir$record_id,  NA,  df_qdir[1:2, ]$record_id)
df$PID        <- c('PID',        '{"ImportId":"PID"}',        df_qdir$PID,        NA,  df_qdir[1:2, ]$PID)
df$uvmid      <- c('uvmid',      '{"ImportId":"uvmid"}',      df_qdir$uvmid,      NA,  df_qdir[1:2, ]$uvmid)





## fixing = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

## columns Finished and Progress are inaccurate:

ckb_cols <- paste0("P7_C1_TIM_activ_", 1:9)
val_cols <- setdiff(names(df)[startsWith(names(df), "P7_C1_")],
                    c(ckb_cols,"P7_C1_TIM_activ_9_TEXT"))

df[3:9, "Progress"]  <- round( ( rowSums(!is.na(df[3:9,val_cols])) + (rowSums(!is.na(df[3:9,ckb_cols])) > 0) ) / ( length(val_cols) + 1) * 100 )
df[3:9, "Finished"]  <- round( df[3:9, "Progress"] == "100" )



## column Duration (in seconds) needs values:

set.seed(77777)
df[3:9, "Duration (in seconds)"] <- round( rgamma(n=7, shape=7.5, scale=1) * 14 )



## column IP address needs values:

IP_paste = function(m_row) { paste(paste0(m_row[1:3], collapse=""),
                                   paste0(m_row[4:6], collapse=""),
                                   m_row[7],
                                   paste0(m_row[8:9], collapse=""),
                                   sep=".") }
set.seed(77777)
df[c(3:6,8:9), "IPAddress"] <- apply(matrix(sample(0:9, 6*9, replace=TRUE), nrow=6, byrow=TRUE), 1, IP_paste)



## column UserLanguage needs values:

df[3:9, "UserLanguage"] <- "EN"



## changing values in DistributionChannel and Status columns:

df[3:6, "DistributionChannel"] <- "email"
df[7,   "DistributionChannel"] <- "preview"
df[8:9, "DistributionChannel"] <- "anonymous"

df[3:6, "Status"] <- 0
df[7,   "Status"] <- 1
df[8:9, "Status"] <- 0



## columns LocationLatitude and LocationLongitude need values:

set.seed(77777)
df[3:7, c("LocationLatitude","LocationLongitude")] <- t(rbind(state.center$y, state.center$x))[sample(1:50, 5), 1:2]





## exporting = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

LEMURS_qualtrics_file_P <- subset(df, select=-c(record_id, uvmid))
LEMURS_qualtrics_file_R <- subset(df, select=-c(PID, uvmid))
LEMURS_qualtrics_file_U <- subset(df, select=-c(record_id, PID))

write.csv(LEMURS_qualtrics_file_P, "inst/extdata/LEMURS_qualtrics_file_P.csv")
write.csv(LEMURS_qualtrics_file_R, "inst/extdata/LEMURS_qualtrics_file_R.csv")
write.csv(LEMURS_qualtrics_file_U, "inst/extdata/LEMURS_qualtrics_file_U.csv")

