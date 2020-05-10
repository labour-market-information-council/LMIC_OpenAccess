#' ==================================================
#' 
#' Labour Market Information Council (LMIC)
#' 
#'  Load and Combine Historic Unemployment 
#'     including pre-1976 annual data.
#'     
#'  Converts annual data to monthly estimates
#' 
#' **************************************************
#' 
#' All data from Statistics Canada sources:
#' 
#'  Post-war / pre-1976 annual unemployment rate
#'   available here: 
#'   https://www150.statcan.gc.ca/n1/pub/75-005-m/75-005-m2016001-eng.htm
#'   
#'   
#'  Pre-war annual unemployment rate is 
#'   available in figure 1 here: 
#'   https://www150.statcan.gc.ca/n1/en/pub/75-001-x/1992003/87-eng.pdf?st=bw4sBvDI
#'   
#'   To extract data points from the image it was 
#'    fed into this online system and manually adjusted
#'    to get a fine grid of points along the blue line
#'    https://apps.automeris.io/wpd/
#'    
#'  These fine points are loaded in this script and then
#'    converted to annual (month of June) points by taking
#'    the y-cooridnate closest to a whole number x-coordinate.
#'   
#'   Post-1976 data is from the standard LFS table publicly
#'    available here: 
#'    https://www150.statcan.gc.ca/n1/en/tbl/csv/14100287-eng.zip
#'  
#' 
#' ======================================================

rm(list=ls()); gc() 

lfs_month <- "2020-04" #(YYYY-MM)

data_dir <- '~/DATA/Codar/LFS' #' folder in which you've downloaded
                               #' table 14-10-0287 from here:
    # "https://www150.statcan.gc.ca/n1/en/tbl/csv/14100287-eng.zip"


# Initialization ----------------------------------------------------------

#' required packages
req_pack <- c('data.table', 'zoo', 'tempdisagg',  'rstudioapi')
#' list of required packages not installed. Install if missing.
to_install <- req_pack[!req_pack %in% rownames(installed.packages())] 
if(length(to_install) > 0) install.packages(to_install)

# load R packages
library(rstudioapi)  # relative paths - only if you are using Rstudio
library(zoo)         # for year-month date type
library(tempdisagg)  # temporary disaggregation
library(data.table)

#' set working directory (works in Rstudio only) - set your own otherwise
local_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(local_dir)



#' **********************************************************************
#     Source 1: Pre-War UER data from Image    --------------------------
#' **********************************************************************

#' load
uer_prewar <- fread('UER_data_ExtractImage_preWar.csv')
setnames(uer_prewar, 1:2, c('x', 'y'))

#' Find closest year and deviation from it
uer_prewar[, c('div', 'year') := .(abs(x - round(x)), round(x))   ]
setkey(uer_prewar, year, div)

#' Find x-y coordinates closest to each year
min_div <- uer_prewar[, min(div), by = year]
setnames(min_div, 'V1', 'div')
setkey(min_div, year, div)

#' Left join on distance minimiziz points
uer_prewar_ann <- uer_prewar[min_div]
setnames(uer_prewar_ann, c("x", "y"), c('x_orig', 'uer'))


#' **********************************************************************
#     Source 2: Post-War Tables ------------------------------------------
#' **********************************************************************

#' load
uer_pre70s <- fread('UER_table_STC_postWar.csv')
setnames(uer_pre70s, 1:4, c('year', 'uer', 'uer_men', 'uer_women'))

#' clean up table
uer_pre70s[, V5 := NULL]
uer_pre70s <- uer_pre70s[!is.na(uer)]
uer_pre70s[, year := as.numeric(year)]

#' Convert to decimal values.
uer_pre70s[, c('uer', 'uer_men', 'uer_women') := lapply(.SD, function(x)x/100), 
           .SDcols = c('uer', 'uer_men', 'uer_women') ]


# Export Annual Data ------------------------------------------------------

uer_historic_annual <- rbindlist(list(
                        uer_prewar_ann[,.(year,uer)],
                        uer_pre70s[,.(year,uer)]), 
                idcol = 'source', use.names = TRUE)

fwrite(uer_historic_annual, 'Annual_Historic_Unemployment.csv')


#' **********************************************************************
#     Source 3: Official LFS Tables ------------------------------------------
#' **********************************************************************

#' Key tables 
lfs_sa_table <- c (  '14100287' )

#' Source 1: Official LFS data - monthly, seasonally adjusted.
lfs_sa <-  fread(file.path(data_dir, paste0(lfs_sa_table, '.csv')))

lfs_sa <- lfs_sa[ `Labour force characteristics` ==  "Unemployment rate" &
                    Statistics == 'Estimate' &
                    `Data type` == 'Seasonally adjusted',
                  .(REF_DATE, GEO, `Labour force characteristics`,
                    Sex, `Age group`,  `Data type`, VALUE) ]

setnames(lfs_sa, 
         c("REF_DATE", "GEO", "Labour force characteristics", "Sex", "Age group", "VALUE"),
         c('month', 'geo', 'lf_char', 'sex', 'age', 'value'))
# sapply(une_sa,class)

une_sa = lfs_sa[geo == 'Canada' & 
                  age == '15 years and over' & 
                  sex == 'Both sexes', 
                .(geo, month, value/100)] #' Can someon PLEASE ask StatsCan
                                          #' to release percents as decimal values...
                                          #' .. what ae we, children? 
setnames(une_sa, 'V3', 'uer')
une_sa[, month := as.yearmon(paste0(month, '-01'))]

setkey(une_sa, geo, month)





# Convert Annual Historic Data to Monthly Estimates -----------------------
#' Assumes June to be the representative month in annual estimates

#' Function to be called in data.table's "j" argument
#' Converts annual data to monthly via the Denton-Cholette method
#' without use of an indicator series.  The resulting dissaggregated
#' series tends therefore to be smoother than a true (seasonally-volatile)
#' quarterly series.  
#'  See:
#'      Sax and Steiner (2013) "Temporal Disaggreation of Time Series",
#'       The R Journal, Vol 5/2, December.
by_annual2month = function(dt){
  
  
  #' needed for output consistency across BY groups
  name.order =  names(dt)
  
  #' remove columns that are all missing (==NA)
  dt = dt[,colSums(is.na(dt)) < nrow(dt), with =F]
  
  #' store NA-columns removed and non-empty columns kept
  empty.check = setdiff(name.order, names(dt))
  convert.var = setdiff(names(dt), c('year', empty.check))
  
  
  if(length(dt) > 1){   #' at least one variable is non-emtpy.
    
    dt[, year := as.Date(paste0(year, '-06-01'))]
    
    #' preallocate output data.table
    dt_out = data.table(month = as.yearmon(seq(dt[,year][1], tail(dt[, year],1),
                                               by = '1 month')), key = 'month')
    
    #' loop over non-empty variables
    for(var in convert.var){
      #' Conver to time series object (ts)
      ts_temp    = ts(dt[, get(var)], start = c(year(dt[, min(year)])))
      
      #' Disaggretation fails if internal NA's present. Check and interpolate.
      non.na.idx <- min(which(!is.na(ts_temp))):max(which(!is.na(ts_temp)))
      if(any(is.na(ts_temp)[non.na.idx])){
        ts_temp[non.na.idx] <-  na.approx(ts_temp[non.na.idx]) # linear interpolation.
      }
      
      #' Temporal disaggregation call:
      ts_disagg  =
        predict(td(ts_temp ~ 1, to = 'monthly',
                   method = "denton-cholette", conversion = "average"))
      
      #' Store result in data.table
      dt_temp = data.table(month  = as.yearmon(index(ts_disagg)),
                           as.vector(ts_disagg), key='month' )
      setnames(dt_temp, 2, var)
      
      #' Right Outer Join to pre-allocated table in each iteration
      dt_out = dt_temp[dt_out]
      
    }
    #' Add NA columns for missing variables.
    if(length(empty.check) > 0){
      dt_out[, (empty.check) := as.numeric(NA)]
    }
    
  }else{
    #' Zero non-emtpy columns: return data.table with nrow(dt) == 0
    dt_out = data.table(qtr = as.yearqtr(NULL))
    dt_out[, (empty.check)  := numeric(0)]
  }
  
  name.order = gsub('(year)', 'month', name.order)
  setcolorder(dt_out, name.order) #' ensure column order for implicit rbind
  
  return(dt_out) #' Return result.
}

#' Convert Annual UER data to monthly observations.
uer_historic_month =  uer_historic_annual[, by_annual2month(.SD), by = source]



# Combine and export all data ---------------------------------------------

une_sa[, source := 3]

uer_all_monthly <- rbindlist(list(
                      uer_historic_month, une_sa[,.(source, month, uer)]
                    ), use.names = TRUE)
setkey(uer_all_monthly, source, month)

fwrite(uer_all_monthly, paste0('Combined_HisotricUER_', lfs_month, '.csv'))
#' fwrite(uer_all_monthly, file.path(data_dir, 'Combined_HisotricUER.csv'))



# Visualize ---------------------------------------------------------------
library(ggplot2)



ggplot() +
  geom_line(data=uer_all_monthly, 
            aes(x = month, y = uer, colour = as.character(source)),
            size = 1.5)  +
  scale_x_yearmon('') +
  scale_y_continuous('Unemployment Rate', 
                     labels = scales::percent, 
                     limits = c(0,.2)) +
  scale_color_manual('', values = c('black', 'red', 'blue'),
                     labels = c('Image extracted', 
                                'Historic Tables',
                                'Modern LFS')) +
  theme_bw() + theme(legend.position  = "bottom")    
  


