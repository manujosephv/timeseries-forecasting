start = Sys.time()

library(data.table)
library(dplyr)
library(feather)
library(xts)
library(zoo)
library(readxl)
library(tidyr)
library(forecast)
library(ggfortify)
library(ggplot2)

WORKING_DIRECTORY = 'C:\\Users\\310228580\\OneDrive - Philips\\Work\\Tradestock\\Forecasting'
DATA_LOAD = FALSE
OUTPUT_FOLDER = 'Forecast_Test'
FEATHER_FOLDER = 'Feather_Test'
INPUT_FOLDER = 'input'
PLOT_FOLDER = 'plots_Test'
FORCE_RELOAD_FLAG = FALSE
# POS_DATA_FOLDER = "POS Data with Sell-in"
POS_DATA_FOLDER = "POS-2015-Date"
PRODUCT_MASTER_FILE = 'Product Master.csv'
LEVEL_VAR_NAME = 'AG'
LEVEL_VAR_NAME_DESC = ifelse((LEVEL_VAR_NAME == 'MAG'),LEVEL_VAR_NAME, paste(LEVEL_VAR_NAME,"_Desc",sep=""))
# LEVEL_VAR_NAME = 'Material - MAG'
# LEVEL_VAR_NAME = 'RCA_reporting_level'
FORECAST_OUTPUT = paste(LEVEL_VAR_NAME,"_Forecast.csv",sep="")
FORECAST_OUTPUT_FEATHER = paste(LEVEL_VAR_NAME,"_Forecast.feather")

dir.create(OUTPUT_FOLDER, showWarnings = FALSE)
dir.create(INPUT_FOLDER, showWarnings = FALSE)
dir.create(PLOT_FOLDER, showWarnings = FALSE)
dir.create(FEATHER_FOLDER, showWarnings = FALSE)

#loding the functions for forecasting
source('forecast_functions.R')

##########################   Functions   ########################


############ Data Load Functions #############

####### Loads a raw excel query and formats it and returns the dataframe############

read_format_xl_query <- function(filename){
  
  col_names = c('Sales_Organization_Desc','Sales_Organization','BG_Desc','BU','BU_Desc','MAG','AG','AG_Desc','Calendar_Year_Week','Calendar_Year_Month','CTV','Material','Stock_QTY','Sell_Out_QTY','Sell in QTY','Store_Stock','Display_Stock','On_Order_Stock')
  col_types= c('text','text','text','text','text','text','text','text','text','text','text','text','numeric','numeric','numeric','numeric','numeric','numeric')
  raw_file = read_excel(filename,skip = 2,sheet = 2, col_names = col_names, col_types = col_types,trim_ws = TRUE)
  raw_file$CTV = gsub("C0_", "",raw_file$CTV)
  raw_file$Stock_QTY[is.na(raw_file$Stock_QTY)] <- 0
  raw_file$Sell_Out_QTY[is.na(raw_file$Sell_Out_QTY)] <- 0
  raw_file$Store_Stock[is.na(raw_file$Store_Stock)] <- 0
  raw_file$Display_Stock[is.na(raw_file$Display_Stock)] <- 0
  raw_file$On_Order_Stock[is.na(raw_file$On_Order_Stock)] <- 0
  raw_file%>%
    na.locf()%>%
    separate(Calendar_Year_Week, into = c('Week','Year'))%>%
    mutate(MonthDate=as.Date(paste("01",`Calendar_Year_Month`),format = '%d %b %Y'))-> wip_df
  wip_df$week_date = as.Date(paste(wip_df$Year,"01","01",sep="-")) + as.numeric(wip_df$Week) * 7 - as.numeric(format(as.Date(paste(wip_df$Year,"01","01",sep="-")), "%w"))
  return(wip_df)
}


##### Loads data from all CSV in a folder or corresponding Feather fle
load_data_from_folder <- function(folder_name, reload=FALSE, file_ext='csv'){
  feather_file_name <- paste(file.path(FEATHER_FOLDER,folder_name),'.feather',sep='')
  
  if (!reload && file.exists(feather_file_name)){ #Feather Exists
    print("Loading from Feather file")
    df = read_feather(feather_file_name)
  } else{ #Feather not created yet or force reload - Loading from CSV
    print(paste("Feather does not exist or forced reload. Loading from",file_ext))
    file_names <- list.files(path = paste("./",folder_name, sep=""), pattern=paste("*.",file_ext,sep=""), full.names = TRUE)
    if (file_ext == 'csv'){
      df <- do.call(rbind,lapply(file_names,fread))  
    } else if(file_ext == 'xls' | file_ext == 'xlsx'){
      df = do.call(rbind,lapply(file_names,read_format_xl_query))  
    } else{
      stop('Check file extention. Should be "csv" or "xls" or "xlsx"')
    }
    write_feather(df, feather_file_name)
  }
  return(df)
}


#######################   Loading and Grouping/Reducing Data #########################

setwd(WORKING_DIRECTORY)
##### Load Data

if (DATA_LOAD){
  df = load_data_from_folder(POS_DATA_FOLDER, reload = FORCE_RELOAD_FLAG, file_ext = 'xlsx')
  
  #Initial Cleaning Up
  
  df%>%
    rename(Product_ID= `CTV`,
           sales_org=`Sales_Organization`,
           sell_out_qty=`Sell_Out_QTY`,
           forecast_level = UQ(LEVEL_VAR_NAME),
           forecast_level_desc = UQ(LEVEL_VAR_NAME_DESC))%>%
    mutate(sell_out_qty = as.numeric(sell_out_qty))->df
  
  
  #Mapping AG and Grouping at AG Level
  df%>%
    select(Product_ID,forecast_level,forecast_level_desc,sell_out_qty,sales_org,MonthDate)%>%
    na.omit()%>% #Need to omit na before grouping else grouping will not work correctly
    group_by(forecast_level,forecast_level_desc,sales_org,MonthDate)%>%
    summarise(sell_out_qty = sum(sell_out_qty))-> forecast_level_df
  
  write_feather(forecast_level_df,file.path(FEATHER_FOLDER,FORECAST_OUTPUT_FEATHER))
  rm(df, forecast_level_df)
  gc()
  
}




#######################   Generating Forecast #########################

#### Level Forecast

forecast_level_df= read_feather(file.path(FEATHER_FOLDER,FORECAST_OUTPUT_FEATHER))

start_date = min(as.Date(forecast_level_df$MonthDate),na.rm = TRUE)
last_date = max(as.Date(forecast_level_df$MonthDate),na.rm = TRUE)


method_list = c('snaive_growth','ets','arima','seasonal_ets','seasonal_arima','nnet','theta','seasonal_nnet','seasonal_theta')

method = 'nnet'

##### To evaluate time required for running each method 
# for (i in 1:length(method_list)){
#   start = Sys.time()
#   method = method_list[i]
#   forecast_level_df %>%
#     group_by(forecast_level,forecast_level_desc) %>%
#     do(rbind(do_forecast_level(., start_date=start_date,
#                                current_date = last_date,
#                                method = method)))->forecast_level_calc_df
  
#   print (paste(method,Sys.time()-start,sep=" : "))
#   # write.csv(forecast_level_calc_df,file.path(OUTPUT_FOLDER,paste(method,FORECAST_OUTPUT,sep="_")))
# }

  forecast_level_df %>%
    group_by(forecast_level,forecast_level_desc) %>%
    do(rbind(do_forecast_level(., start_date=start_date,
                                   current_date = last_date,
                                   method = method)))->forecast_level_calc_df

write.csv(forecast_level_calc_df,file.path(OUTPUT_FOLDER,paste(method,FORECAST_OUTPUT,sep="_")))

end = Sys.time()
elapsed = end-start
elapsed


###Performance Summary - Visualization

library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# cutoff=100

forecast_level_df%>%
  select(forecast_level,MAPE)%>%
  unique()->mape_dist

cutoff = round(quantile(mape_dist$MAPE,probs=.85,na.rm=TRUE),digits = 0)

rm(mape_dist)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL,export=TRUE) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    if (export){
      png(paste(LEVEL_VAR_NAME,"plot.png"))
    }
    
    
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
      
      
    }
    
    if (export){
      dev.off()
    }
    
  }
}


forecast_level_df%>%
  filter(MAPE>cutoff)%>%
  select(forecast_level,MAPE)%>%
  unique()%>%
  nrow()->forecast_levels_above_limit


#Exclude >100 MAPE
forecast_level_df%>%
  filter(MAPE<cutoff)%>%
  select(forecast_level,MAPE)%>%
  unique()->forecast_level_plot  # Overlay with transparent density plot

cutoff_plot <- ggplot(forecast_level_plot,aes(x=MAPE))+
  geom_histogram(binwidth=10,colour=cbPalette[1], fill=cbPalette[3])+
  stat_bin(binwidth=10, geom="text", colour="white", size=4,aes(label=..count..), position=position_stack(vjust=0.5))+
  geom_vline(aes(xintercept=mean(MAPE, na.rm=T)), color=cbPalette[4], linetype="dashed", size=1)+
  labs(title=paste(LEVEL_VAR_NAME,"Level MAPE Distribution"),
       subtitle="Excluding Outliers")

cutoff_plot <- cutoff_plot+
  annotate(geom="text", hjust = 0, x=(cutoff/2), y=max(ggplot_build(cutoff_plot)$data[[1]]$count)*0.8, size=5,label=paste(forecast_levels_above_limit, LEVEL_VAR_NAME,"with MAPE greater than", cutoff),color=cbPalette[7])

full_plot <-ggplot(forecast_level_df%>%
                     select(forecast_level,MAPE)%>%
                     unique(),aes(x=MAPE))+
  geom_histogram(aes(y=..density..),binwidth=10,colour=cbPalette[1], fill=cbPalette[3])+
  geom_density(alpha=.2, fill=cbPalette[5], color=cbPalette[1])+
  labs(title=paste(LEVEL_VAR_NAME,"Level MAPE Density"))

#Export not working
multiplot(full_plot,cutoff_plot,cols=1,export = FALSE)
ggsave(file.path(PLOT_FOLDER,paste(LEVEL_VAR_NAME,"plot.png")),plot = multiplot(full_plot,cutoff_plot,cols=1,export = FALSE))

