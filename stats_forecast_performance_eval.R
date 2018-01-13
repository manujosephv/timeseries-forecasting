
library(data.table)
library(dplyr)
library(feather)
library(tidyr)
library(forecast)
library(ggfortify)
library(ggplot2)
library(readxl)

WORKING_DIRECTORY = 'xxxx'
PLOT_FOLDER = 'plots_Test'
FORECAST_FOLDER = "Forecast_Test"
FORCE_RELOAD_FLAG = FALSE
FEATHER_FOLDER = 'Feather_Test'

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


setwd(WORKING_DIRECTORY)


df = load_data_from_folder(FORECAST_FOLDER, reload = FORCE_RELOAD_FLAG, file_ext = 'csv')

df %>% 
copy()-> error_df

error_df = error_df[!is.na(error_df$Fitted),]

error_df %>% 
  group_by(forecast_level,forecast_level_desc, method_arg) %>% 
  summarise(actuals = mean(Data),method = max(method), MAPE=max(MAPE), RMSE = max(RMSE),MASE=max(MASE)) -> forecast_level_grouped_df

forecast_level_grouped_df %>% 
  group_by(forecast_level) %>% 
  summarise(actuals = max(actuals)) %>% 
  mutate(weight = actuals/sum(actuals))->weight_df

forecast_level_grouped_df %>% 
  merge(weight_df,by = 'forecast_level') -> forecast_level_grouped_df

order_names = c('snaive_growth', 'theta', 'ets', 'arima', 'seasonal_arima', "seasonal_theta", 'seasonal_ets', 'nnet', 'seasonal_nnet')

order_labels_full = c('Seasonal Naive (Growth)', 'Theta Forecast', 'Exponential State Space (ETS)', 'ARIMA', 'Seasonal Decomposition (ARIMA)', "Seasonal Decomposition (Theta Forecast)", 'Seasonal Decomposition (ETS)', 'Neural Network', 'Seasonal Decomposition (Neural Network)')

order_labels_short = c('Seas. Naive', 'Theta', 'Exponential', 'ARIMA', 'Seas.+ ARIMA', "Seas. + Theta", 'Seas. + ETS', 'Neural Network', 'Seas. + Neural Net')

forecast_level_grouped_df %>% merge(as.data.frame(cbind(method_arg = order_names,order_labels_full, order_labels_short)), by='method_arg') -> forecast_level_grouped_df



forecast_level_grouped_df %>%
  mutate(weightedMAPE = weight * MAPE) %>%
  filter(!is.infinite(weightedMAPE),!is.na(weightedMAPE)) %>% 
  group_by(method_arg,order_labels_short,order_labels_full) %>% 
  summarise(wMAPE = sum(weightedMAPE)) %>% 
  merge(
    forecast_level_grouped_df %>%
      filter(!is.infinite(MAPE),!is.na(MAPE)) %>% 
      group_by(method_arg) %>% 
      summarise(meanMAPE = mean(MAPE), medianMAPE = median(MAPE)),
    by = 'method_arg'
  ) -> agg_measures_df

# names = c('GroupBlue3','BrightAqua','BrightGreen','BrightOrange','BrightPink','GroupBlue2','DarkAqua','DarkGreen','DarkOrange','LightAqua','LightGreen','LightOrange')
# my_palette = c('#005A8B','#1E9D8B','#5B8F22','#E98300','#EC4371','#0089C4','#156570','#00693C','#983222','#5BBBB7','#B6BF00','#EEAF00')
# names(my_palette) = names


names = c('BrightBlue','BrightAqua','BrightGreen','BrightOrange','BrightPink','BrightPurple','DarkBlue','DarkAqua','DarkGreen','DarkOrange','DarkPink','DarkPurple')
my_palette = c('#0066a1','#1e9d8b','#5b8f22','#e98300','#ec4371','#7d0063','#003478','#156570','#00693c','#983222','#91004b','#631d76')
# names(my_palette) = names


# my_palette_complete = c('#0b5ed7','#0b2265','#0039a6','#005a8b','#0089c4','#72b5cc','#0f204b','#003478','#0066a1','#7ba4d9','#b3c8e6','#22505f','#156570','#1e9d8b','#5bbbb7','#cae3e9','#024731','#00693c','#5b8f22','#b6bf00','#e0e96e','#772432','#983222','#e98300','#eeaf00','#fbd476','#91004b','#ec4371','#e59aaa','#e9c5cb','#42145f','#631d76','#7d0063','#b390bb','#dbcfe9','#000000','#252525','#5d5d5d','#888888','#b9b9b9','#cccccc','#e0e0e0','#efefef','#ffffff','#cd202c','#252525','#888888','#cccccc','#00f010')
# names_complete = c('colorPWordmarkBlue','colorPGroupBlue5','colorPGroupBlue4','colorPGroupBlue3','colorPGroupBlue2','colorPGroupBlue1','colorPVeryDarkBlue','colorPDarkBlue','colorPBrightBlue','colorPLightBlue','colorPVeryLightBlue','colorPVeryDarkAqua','colorPDarkAqua','colorPBrightAqua','colorPLightAqua','colorPVeryLightAqua','colorPVeryDarkGreen','colorPDarkGreen','colorPBrightGreen','colorPLightGreen','colorPVeryLightGreen','colorPVeryDarkOrange','colorPDarkOrange','colorPBrightOrange','colorPLightOrange','colorPVeryLightOrange','colorPDarkPink','colorPBrightPink','colorPLightPink','colorPVeryLightPink','colorPVeryDarkPurple','colorPDarkPurple','colorPBrightPurple','colorPLightPurple','colorPVeryLightPurple','colorPBlack','colorPEnricher1','colorPEnricher2','colorPEnricher3','colorPEnricher4','colorPEnricher5','colorPEnricher6','colorPEnricher7','colorPWhite','colorPEnricherRed','colorPDark','colorPMedium','colorPLight','colorPQlik')
# names(my_palette_complete) = names_complete


# Density plots
ggplot(forecast_level_grouped_df, aes(x=MAPE, fill=order_labels_short)) + geom_density(alpha=0.3)+
  scale_x_continuous(name="MAPE", breaks = c(25,50,100), limits = c(0,100))+
  labs(x="MAPE", y="% of AGs", title="Density Plot MAPE - AG")+
  # scale_color_manual(values = my_palette)+
  # scale_fill_manual(values = my_palette) +
  theme(legend.title=element_blank())+
  guides(fill=guide_legend(title=NULL))
  
  

library(ggridges)
## Need to check

png(filename="RidgePlot.png", 
    type="cairo",
    units="px", 
    width=850, 
    height=550, 
    pointsize=12, 
    res=96)



ggplot(forecast_level_grouped_df %>% filter(!is.infinite(MAPE),!is.na(MAPE)), aes(x = MAPE, y = order_labels_short, fill = order_labels_short)) + 
  geom_density_ridges(scale=2) + 
  scale_x_continuous(name="MAPE", breaks = c(0,25,50,75,100), limits = c(0,100))+
  labs(x="MAPE", y="", title="Ridgeline Plot MAPE - AG")+
  scale_y_discrete(limits=order_labels_short)+
  theme(legend.position="none")+
  scale_color_manual(values = my_palette)+
  scale_fill_manual(values = my_palette) +
  theme(legend.position = 'none',
        text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=14, hjust=0.5, vjust=0.5, lineheight = 2),
        axis.title = element_text(size=10),
        axis.text = element_text(family='Lato Semibold'),
        axis.text.y = element_text(size=12),
        # axis.ticks = element_line(linetype = 'blank'),
        panel.grid = element_line(linetype = 'blank'),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        plot.margin = margin(1, 1, .5, .5, "cm")
  )

dev.off()

agg_measures_df %>% 
  arrange(desc(medianMAPE)) %>% 
  select(method_arg) %>% as.character() -> ordered_args


png(filename="BoxPlot.png", 
    type="cairo",
    units="px", 
    width=850, 
    height=550, 
    pointsize=12, 
    res=96)


# With flipped axes
ggplot(forecast_level_grouped_df, aes(x=order_labels_short, y=MAPE, fill=order_labels_short)) + geom_boxplot() + 
  guides(fill=FALSE) + 
  scale_y_continuous(name="MAPE", breaks = c(25,50,100), limits = c(0,100)) +
  scale_x_discrete(limits=order_labels_short)+
  coord_flip()+
  stat_summary(fun.y=mean, geom="point", shape=17, size=2)+
  labs(x='',title='Box Plot of AG level MAPE(s)')+
  scale_color_manual(values = my_palette)+
  scale_fill_manual(values = my_palette)+
  theme(legend.position = 'none',
        text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=14, hjust=0.5, vjust=0.5, lineheight = 2),
        axis.title = element_text(size=10),
        axis.text = element_text(family='Lato Semibold'),
        axis.text.y = element_text(size=12),
        # axis.ticks = element_line(linetype = 'blank'),
        # panel.grid = element_line(linetype = 'blank'),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        plot.margin = margin(1, 1, .5, .5, "cm")
  )

dev.off()


#mean plot
ggplot(agg_measures_df, aes(y=order_labels_short, x=100-meanMAPE, label = paste0(round(100-meanMAPE, 0), "%"),color=order_labels_short)) +
  geom_segment(aes(x = 0, y = order_labels_short, xend = 100-meanMAPE, yend = order_labels_short)) +
  geom_point() +
  geom_text(nudge_x = 5)+
  scale_y_discrete(limits=order_labels_short)+
  theme(legend.position="none")

library(stringr)

png(filename="Accuracy Stat.png", 
    type="cairo",
    units="px", 
    width=850, 
    height=550, 
    pointsize=12, 
    res=96)



#accuracy plot
#wMape Accuracy plot vertical
ggplot(agg_measures_df, aes(x=order_labels_short, y=100-wMAPE, label = paste0(round(100-wMAPE, 0), "%"),color=order_labels_short)) +
  geom_segment(aes(y = 50, x = order_labels_short, yend = 100-wMAPE, xend = order_labels_short),size=1) +
  geom_point(size=25) +
  geom_point(aes(y=50.5,x=order_labels_short),size=2) +
  geom_text(color="#ffffff",nudge_y = 0.5, size = 4, family='Lato Semibold')+
  # geom_text(nudge_y = 5)+
  scale_x_discrete(limits=order_labels_short, labels = str_wrap(order_labels_short, width = 5))+
  scale_y_continuous(limits = c(50,100),expand = c(0, 0))+
  labs(x="", y="", title="Accuracy (wMAPE) of different Statistical Forecast Methods")+
  scale_color_manual(values = my_palette)+
  theme(legend.position = 'none',
        text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=14, hjust=0.5, vjust=0.5, lineheight = 2),
        axis.title = element_text(size=10),
        axis.text = element_text(family='Lato Semibold', size=12),
        axis.text.y = element_blank() ,
        axis.ticks = element_line(linetype = 'blank'),
        panel.grid = element_line(linetype = 'blank'),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        plot.margin = margin(1, 1, .5, .5, "cm")
        )


dev.off()

#wmean plot
ggplot(agg_measures_df, aes(y=method_arg, x=wMAPE, label = paste0(round(wMAPE, 0), "%"),color=method_arg)) +
  geom_segment(aes(x = 0, y = method_arg, xend = wMAPE, yend = method_arg)) +
  geom_point(size=3) +
  geom_text(nudge_x = 5)+
  scale_y_discrete(limits=c('snaive_growth', 'theta', 'ets', 'arima', 'seasonal_arima', "seasonal_theta", 'seasonal_ets', 'nnet', 'seasonal_nnet'))

#median plot
ggplot(agg_measures_df, aes(y=method_arg, x=medianMAPE, label = paste0(round(medianMAPE, 0), "%"),color=method_arg)) +
  geom_segment(aes(x = 0, y = method_arg, xend = medianMAPE, yend = method_arg)) +
  geom_point() +
  geom_text(nudge_x = 5)+
  scale_y_discrete(limits=c('snaive_growth', 'theta', 'ets', 'arima', 'seasonal_arima', "seasonal_theta", 'seasonal_ets', 'nnet', 'seasonal_nnet'))


complexity_df = read_excel('methods.xlsx',sheet = 1, trim_ws = TRUE)


library(ggrepel)
################################

order_labels_short_1 = c('Seas. Naïve', 'Theta', 'Exponential', 'ARIMA', 'Seas. + ARIMA', "Seas. + Theta", 'Seas. + Exp', 'Neural Net', 'Seas. + Neural Net')
t=my_palette
names(t) = names
scatter_palette = c(t['DarkBlue'],t["DarkGreen"], t["BrightAqua"], t["BrightBlue"],t["DarkAqua"],t["DarkPink"],t["BrightOrange"],t["BrightGreen"],t["BrightPink"])
names(scatter_palette) = order_labels_short_1


png(filename="Stat FC Matrix.png", 
    type="cairo",
    units="px", 
    width=850, 
    height=550, 
    pointsize=12, 
    res=96)


ggplot(complexity_df, aes(x=Complexity, y=Accuracy, color=`Method Short`)) +
  geom_point(shape=19,size=12.5, color='grey50')+
  geom_point(shape=19,size=12)+
  
  # scale_size(range=c(15,20))+
  geom_text_repel(aes(label=`Method Short`), size=4,color="black")+
  scale_x_continuous(limits = c(0,100),breaks=c(10,89),labels=c('Simple Statistical Models','Advanced Machine Learning Models'))+
  scale_y_continuous(limits = c(50,100),breaks=c(62.5,87.5),labels=c('Less','More'))+
  geom_hline(yintercept=75, color = 'grey50') + geom_vline(xintercept=50,color = 'grey50')+
  labs(x="Complexity", y="Accuracy", title="Statistical Forecast Methods")+
  scale_color_manual(values = scatter_palette)+
  scale_fill_manual(values = scatter_palette)+
  theme(legend.position = 'none',
        text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=14, hjust=0.5, vjust=0.5, lineheight = 2),
        axis.title = element_text(size=10),
        axis.text = element_text(family='Lato Semibold'),
        # axis.text.y = element_blank() ,
        axis.ticks = element_line(linetype = 'blank'),
        panel.grid = element_line(linetype = 'blank'),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        plot.margin = margin(1, 1, .5, .5, "cm")
  )

dev.off()
######################################




order_labels_short_1 = c('Seas. Naïve', 'Theta', 'Exponential', 'ARIMA', 'Seas. + ARIMA', "Seas. + Theta", 'Seas. + Exp', 'Neural Net', 'Seas. + Neural Net')
t=my_palette
names(t) = names
t['Grey'] = '#e5e3e3'
hightlight_palette = c(t['Grey'],t["Grey"], t["Grey"], t["Grey"],t["Grey"],t["Grey"],t["BrightOrange"],t["Grey"],t["BrightPink"])
text_palette = c(t['Grey'],t["Grey"], t["Grey"], t["Grey"],t["Grey"],t["Grey"],"black",t["Grey"],'black')
names(hightlight_palette) = order_labels_short_1
names(text_palette) = order_labels_short_1


png(filename="Stat FC Matrix_focus.png", 
    type="cairo",
    units="px", 
    width=850, 
    height=550, 
    pointsize=12, 
    res=96)

ggplot(complexity_df, aes(x=Complexity, y=Accuracy, color=`Method Short`)) +
  geom_point(shape=19,size=12)+
  
  # scale_size(range=c(15,20))+
  # geom_text_repel(aes(label=`Method Short`), size=4,color="black")+
  annotate('text',x=c(30,90), y=c(86,91),label=c('Seas. + Exp','Seas. + Neural Net'))+
  geom_segment(aes(x=0, y=82.8, xend=30, yend=82.8), colour=t['BrightOrange'], linetype='dotted')+
  geom_segment(aes(x=0, y=88.2, xend=90, yend=88.2), colour=t['BrightPink'], linetype='dotted')+
  geom_segment(aes(x=30, y=82.8, xend=30, yend=100), colour=t['BrightOrange'], linetype='dotted')+
  geom_segment(aes(x=90, y=88.2, xend=90, yend=100), colour=t['BrightPink'], linetype='dotted')+

  scale_x_continuous(limits = c(0,100),breaks=c(10,89),labels=c('Simple Statistical Models','Advanced Machine Learning Models'))+
  scale_y_continuous(limits = c(50,100),breaks=c(62.5,87.5),labels=c('Less','More'))+
  geom_hline(yintercept=75, color = 'grey50') + geom_vline(xintercept=50,color = 'grey50')+
  labs(x="Complexity", y="Accuracy", title="Statistical Forecast Methods")+
  scale_color_manual(values = hightlight_palette)+
  scale_fill_manual(values = hightlight_palette)+
  theme(legend.position = 'none',
        text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=14, hjust=0.5, vjust=0.5, lineheight = 2),
        axis.title = element_text(size=10),
        axis.text = element_text(family='Lato Semibold'),
        # axis.text.y = element_blank() ,
        axis.ticks = element_line(linetype = 'blank'),
        panel.grid = element_line(linetype = 'blank'),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        plot.margin = margin(1, 1, .5, .5, "cm")
  )


dev.off()



#########################################

df %>%
  select(forecast_level, Index, `Point Forecast`, method, method_arg) %>%
  filter(Index > '2017-10-31')-> missing_df


missing_df$value = 'Present'
missing_df$value[is.na(missing_df$`Point Forecast`)] = 'Missing'

missing_df %>%
  select(method_arg) %>%
  unique()-> args
# 
# missing_df %>% 
#   filter(method_arg =='theta') %>% 
#   select(method) %>% 
#   unique() -> unique_methods


missing_df[(missing_df$method_arg=='arima') & !(grepl("ARIMA", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$method_arg=='ets') & (grepl("Random", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$method_arg=='nnet') & !(grepl("NNAR", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$method_arg=='seasonal_nnet') & !(grepl("STL", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$method_arg=='seasonal_ets') & !(grepl("STL", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$method_arg=='seasonal_arima') & !(grepl("STL", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$method_arg=='seasonal_theta') & !(grepl("STL", missing_df$method)),'value'] = 'Alternate'

#Commented cause in the regular method no backup method is there.
# missing_df[(missing_df$method_arg=='snaive_growth') & !(grepl("Seasonal", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$method_arg=='theta') & !(grepl("Theta", missing_df$method)),'value'] = 'Alternate'

missing_df[(missing_df$value=='Present'),'value_num'] = 0

missing_df[(missing_df$value=='Missing'),'value_num'] = 1

missing_df[(missing_df$value=='Alternate'),'value_num'] = 1000000000000

missing_df %>% 
  group_by(forecast_level,method_arg) %>% 
  summarise(value_count = sum(value_num)) -> forecast_level_missing_df

forecast_level_missing_df[(forecast_level_missing_df$value_count == 0),'category'] = 'Present'

forecast_level_missing_df[(forecast_level_missing_df$value_count > 0) & (forecast_level_missing_df$value_count<100),'category'] = 'Missing'

forecast_level_missing_df[(forecast_level_missing_df$value_count > 100) ,'category'] = 'Alternate'


ggplot(data = missing_df %>% arrange(desc(value)),
       aes(x = as.Date(Index),
           y = forecast_level)) +
  geom_raster(aes(fill = value)) +
  facet_grid(. ~ method_arg)+
  labs(x = "AGs",
       y = "Months") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


library(waffle)
library(extrafont)
font_import()

extrafont::loadfonts(device="win")

forecast_level_missing_df %>% 
  group_by(method_arg,category) %>% 
  summarise(n=n())->sum_missing_df


colors_waffle = c('#1e9d8b','#e98300','#ec4371')

sum_missing_df %>% filter(method_arg=='snaive_growth') %>% ungroup()-> sng
de<-data.frame("snaive_growth","Alternate",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
snaive_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="Seasonal Naive with Growth") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(1, 1, 1, 0.2, "cm")
  )

# waffle(whatyouwant, rows=9,use_glyph="arrow-circle-o-up",legend_pos="top",title="Seasonal Naive with Growth", size = 0.5)

sum_missing_df %>% filter(method_arg=='arima') %>% ungroup() -> sng
de<-data.frame("arima","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
arima_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="ARIMA") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))

      

#################
sum_missing_df %>% filter(method_arg=='ets') %>% ungroup() -> sng
de<-data.frame("ets","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
ets_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="Exponential State Space") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))

#################
sum_missing_df %>% filter(method_arg=='nnet') %>% ungroup() -> sng
de<-data.frame("nnet","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
# names(whatyouwant) <- paste(sng$category,percent, sep="\n")
names(whatyouwant) <- sng$category
nnet_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="Neural Network") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))

#################
sum_missing_df %>% filter(method_arg=='theta') %>% ungroup() -> sng
de<-data.frame("theta","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
theta_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="Theta Forecast") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))





iron(snaive_waffle, arima_waffle, ets_waffle, nnet_waffle,theta_waffle)


#####################################
#################
sum_missing_df %>% filter(method_arg=='seasonal_arima') %>% ungroup() -> sng
de<-data.frame("seasonal_arima","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
stl_arima_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="Seasonal Decomposition + ARIMA") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))


#########################

sum_missing_df %>% filter(method_arg=='seasonal_ets') %>% ungroup() -> sng
de<-data.frame("seasonal_ets","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
stl_ets_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="Seasonal Decomposition + ETS") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))



#########################

sum_missing_df %>% filter(method_arg=='seasonal_nnet') %>% ungroup() -> sng
de<-data.frame("seasonal_nnet","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
stl_nnet_waffle <- waffle(whatyouwant, rows=5,legend_pos="none", colors = colors_waffle,title="Seasonal Decomposition + Neural Network") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))



#########################

sum_missing_df %>% filter(method_arg=='seasonal_theta') %>% ungroup() -> sng
de<-data.frame("seasonal_theta","Missing",0)
names(de)=names(sng)
rbind(sng,de) %>% arrange(factor(category, levels = c('Present','Alternate', 'Missing'))) -> sng

whatyouwant <- as.numeric(unlist(sng$n))
percent = paste("(",round((sng$n/sum(sng$n))*100,digits=1),"%",")",sep="")
names(whatyouwant) <- sng$category
stl_theta_waffle <- waffle(whatyouwant, rows=5,legend_pos="bottom", colors = colors_waffle,title="Seasonal Decomposition Methods") +
  theme(text = element_text(family ='Lato Light', color='black'), 
        title = element_text(size=12, hjust=0.5, vjust=0.5, lineheight = 2),
        plot.margin = margin(0.2, 1, 1, 0.2, "cm"))



iron(stl_ets_waffle,stl_arima_waffle,stl_nnet_waffle,stl_theta_waffle)



png(filename="Coverage_short.png", 
    type="cairo",
    units="px", 
    width=763,
    height=1000, 
    pointsize=12, 
    res=96)



iron(snaive_waffle, arima_waffle, ets_waffle, nnet_waffle,theta_waffle,stl_theta_waffle)

dev.off()
