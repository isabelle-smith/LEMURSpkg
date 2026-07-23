

## code to create `LEMURS_key_df` dataset

## Izzy Smith, 2026



## reading = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

df_qdir <- readr::read_csv(file="data-raw/LEMURS_Test_Directory.csv",                    ## columns: FirstName, LastName, Email, record_id, PID, uvmid
                           progress=FALSE, show_col_types=FALSE) |> as.data.frame()





## exporting = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

LEMURS_key_df <- subset(df_qdir, select=c(record_id, uvmid))

usethis::use_data(LEMURS_key_df)

