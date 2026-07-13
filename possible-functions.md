
# Possible Functions

## CASEMAKER

used: p1  

```
## GOAL: survey sections with missing timestamps become NAs

casemaker <- function(prefix){
 
  case <- glue("across(starts_with('{prefix}') & !ends_with('_timestamp'),
               ~case_when(is.na(`{prefix}_timestamp`)~NA, TRUE~.))")
  
  return(list(parse_expr(case)))
  
}
```





&nbsp;

## P3 FIND DUPLICATES

used: p3

```
p3df_find_duplicates <- function(df) {
  
  if (length(unique(df$uvmSurveyID))==1){ ## only 1 SurveyID value
    
    ## finding duplicated IDs
    df_di <- df |>
      count(uvmid, record_id) |>
      filter(n>1) |>
      mutate(di = uvmid)
    
    ## keeping only the duplicated IDs
    df_dr <- df[df$uvmid %in% df_di$di,]
    
    
  } else { ## multiple SurveyID values
    
    ## finding duplicated survey/uvmid combinations
    df_di <- df |>
      group_by(uvmSurveyID) |>
      count(uvmid, record_id) |>
      filter(n>1) |>
      ungroup() |>
      mutate(di = paste(uvmSurveyID, uvmid, sep="_"))
    
    ## keeping only the duplicated combinations
    df_dr <- df[paste(df$uvmSurveyID, df$uvmid, sep="_") %in% df_di$di,]
    
    
  }
  
  ## sorting/ordering the rows
  df_dr_s <- df_dr |>
    arrange(uvmSurveyID, record_id, uvmid, desc(Finished), desc(Progress), desc(Duration))
  
  ## returning the final dataframe
  return(df_dr_s)
  
}
```





&nbsp;

##  READING IN DATA

1. made `fn_read_qualtrics_data` [2026.07.13]

2. need `fn_output_qualtrics_data`:

```
#
# assign(paste0("p0df_", p3_names[i],"_0"),       ## assign df
#        df)
#
# p0df_0_list[[i]] <- df                          ## append to list
#
#
# nm <- names(df)
#
# assign(paste0("nm_", p0_names[i]),              ## names array
#        nm)
# assign(paste0("nm_", p0_names[i], "_txt"),      ## text names array
#        grep("(_TEXT)$", nm, value=TRUE))
#
```

3. SCRATCH:

p3 version

```
for (i in 1:length(p3_files)){

  df <- read.csv(paste0(path_to_masswearables,
                        path_to_phase3,
                        "raw/",
                        p3_files[i])) |>
    slice(-c(1,2)) |>                                                      ## drop rows 1+2 (Qualtrics info)
    
    filter(uvmid!="" & !is.na(uvmSurveyID)) |>                             ## drop rows where uvmid="" (NAs) or uvmSurveyID=NA
    
    select(-c(Status, IPAddress,                                           ## drop columns (Qualtrics info)
              RecipientLastName, RecipientFirstName,
              RecipientEmail, ExternalReference,                           ## kept:
              LocationLatitude, LocationLongitude,                         ##       StartDate, EndDate, Progress, Duration..in.seconds., Finished,
              DistributionChannel, UserLanguage)) |>                       ##       RecordedDate, ResponseId, ResponseID, SurveyID, uvmid, uvmSurveyID
    
    mutate(across(.cols=all_of(numvars_allp3), .fns=as.numeric)) |>        ## change listed columns to numeric (from string)
    
    mutate(DateSt=as.POSIXct(StartDate, format="%Y-%m-%d %H:%M:%S"),       ## change date columns from strings to POSIXct
           DateEn=as.POSIXct(EndDate, format="%Y-%m-%d %H:%M:%S"),
           .keep="unused") |>
    
    rename(Duration=`Duration..in.seconds.`) |>                             ## rename column to remove periods
    
    full_join(record_to_uvm_id_df, by=join_by(uvmid)) |>                    ## adding `record_id` (full_join keeps all rows)
    
    relocate(c(uvmSurveyID, record_id, uvmid,                               ## move columns to the front of the dataframe
               DateSt, DateEn,
               Finished, Progress, Duration))
  
  
  ## output:
  assign(paste0("p3df_", p3_names[i],"_0"),       ## assign df
         df)
  
  p3df_0_list[[i]] <- df                          ## append to list
  
  
  nm <- names(df)
  
  assign(paste0("nm_", p3_names[i]),              ## names array
         nm)
  assign(paste0("nm_", p3_names[i], "_txt"),      ## text names array
         grep("(_TEXT)$", nm, value=TRUE))
  
  
}
```

p4 version
```
for (i in 1:length(p4_files)){
  
  df <- read.csv(paste0(path_to_masswearables,
                        path_to_phase4,
                        "raw/",
                        p4_files[i])) |>
    
    slice(-c(1,2)) |>                                                      ## drop rows 1+2 (Qualtrics info)
    
    filter(PID!="") |>                                                     ## drop rows where PID="" (NAs)
    
    select(-c(Status, IPAddress,                                           ## drop columns (Qualtrics info)
              RecipientLastName, RecipientFirstName,
              RecipientEmail, ExternalReference,                           ## kept:
              LocationLatitude, LocationLongitude,                         ##       StartDate, EndDate, Progress, Duration..in.seconds., Finished,
              DistributionChannel, UserLanguage)) |>                       ##       RecordedDate, ResponseId, ResponseID, SurveyID, uvmid, uvmSurveyID
    
    mutate(DateSt=as.POSIXct(StartDate, format="%Y-%m-%d %H:%M:%S"),       ## change date columns from strings to POSIXct
           DateEn=as.POSIXct(EndDate, format="%Y-%m-%d %H:%M:%S"),
           .keep="unused") |>
    
    rename(record_id=PID,                                                  ## rename columns (for clarity & to remove periods)
           Duration=`Duration..in.seconds.`) |>
    
    mutate(across(.cols=all_of(numvars_allp4), .fns=as.numeric)) |>        ## change listed columns to numeric (from string)
    
    relocate(c(record_id, DateSt, DateEn,                                  ## move columns to the front of the dataframe
               Finished, Progress, Duration))
  
  
  ## output:
  assign(paste0("p4df_", p4_names[i],"_0"),       ## assign df
         df)
  
  
  p4df_0_list[[i]] <- df                          ## append to list
  p4df_r_list[[i]] <- unique(df$record_id)        ## record_id
  
  
  nm <- names(df)
  
  assign(paste0("nm_", p4_names[i]),              ## names array
         nm)
  assign(paste0("nm_", p4_names[i], "_txt"),      ## text names array
         grep("(_TEXT)$", nm, value=TRUE))
  
  
}
```






&nbsp;

## NEXT













