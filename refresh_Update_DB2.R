print(paste("district is: ",input$district_dB2))
#print(names(dat_ou$df))
p<-input$district_dB2

## select data from sql database

con_pros <- dbConnect(SQLite(),"prospective_data.sqlite")
runs_c<-dB2_pars()$curr_Run %>% filter(district==input$district_dB2)
tim_stamp<-str_remove(runs_c$model_run_Name,"model_Run_")

dat_nam<-paste('pros_Dat_',input$district_dB2,'_',tim_stamp,sep='')
print(dat_nam)

## decide whether to update or insert new record
ds_cu1<-dbGetQuery(con_pros, paste(paste('SELECT distinct * FROM ',dat_nam,sep=''),collapse =''))
output$data_dis<-renderDataTable(datatable(ds_cu1,
                                           options = list(autoWidth = TRUE,
                                                          searching = FALSE)))
dbDisconnect(con_pros)

val_in_up<-reactiveValues(dat=ds_cu1)

observeEvent(input$week,{
  al_ev<-paste("al_var1<-as.character( dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[1]",sep='')
  print(names(val_in_up$dat))
  eval(parse(text=al_ev))
  al_var<- c("Cases","Population",as.character(str_split(al_var1,pattern=",",simplify =T)))
  al_var11<-as.character(str_split(al_var1,pattern=",",simplify =T))
  fa_f<-val_in_up$dat %>% filter(week==input$week)
  print("this is where it changed")
  print(val_in_up$dat)
  
  check_t<-sum(sapply(al_var11,FUN =function(x) as.numeric(x %in% names(fa_f))))
  print(paste("total is::",check_t))
  if(check_t==length(al_var11)){
    rev<-as.numeric(fa_f[,al_var])
    print(c(input$district_dB2,input$week))
    print(rev)
    up_st<-paste("updateNumericInput(session,'",al_var,"'",",value=",rev,')',sep='')
    print(up_st)
    eval(parse(text=up_st))
  }
})
# model data and evaluation
d_str<-paste("dat_eval<-dB2_pars()$all_forDBS","[[",paste("'data_eval_",p,"'",sep=''),"]]",sep='')
m_str<-paste("mod<-dB2_pars()$all_forDBS","[[",paste("'gam_model_",p,"'",sep=''),"]]",sep='')
eval(parse(text=d_str))
eval(parse(text=m_str))

##get year 
yrs_ch<-min(dat_eval$year)
dat_eval<-dat_eval %>% filter(year==yrs_ch)


tem.d.a<-expand.grid(District=p,year=input$year,week=2:52)
tem.d<-merge(tem.d.a,ds_cu1,by=c("District","week"),sort=T,all.x=T)

temp.d_check<<-tem.d
ds_c1_check<<-ds_cu1
## merge with entered values for prediction
cc_d<-paste("cc_v<-dB2_pars()$all_forDBS$param_values_",input$district_dB2,sep='')
eval(parse(text=cc_d))
a_n<-which(cc_v$parameter=="alarm threshold")
print(cc_v$value[a_n])
tem.d<-tem.d %>% mutate(outbreak=(Cases/Population)*1000,
                        alarm_threshold=as.numeric(as.character(cc_v$value[a_n])),
                        outbreak_moving=round(dat_eval$outbreak_moving,6),
                        outbreak_moving_sd=dat_eval$outbreak_moving_sd,
                        outbreak_moving_limit=round(dat_eval$outbreak_moving_limit,6),
                        endemic_chanel=round(dat_eval$outbreak_moving_limit,6),
                        season=dat_eval$season)
##map the alarm indicators

al_ev<-paste("al_var1<-as.character( dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[1]",sep='')

eval(parse(text=al_ev))
al_var<- str_split(al_var1,pattern=",",simplify =T)
n_alarm_indicators<-length(al_var)


##create alarm indicators

#alarm_ind<-paste(paste(paste('mean_alarm',1:n_alarm_indicators,sep=''),'=',input$alarm_indicators),collapse =',')
#create_alarm_inds<-paste('tem.d %>%mutate(',alarm_ind,')',sep='')

##create correctly
#alarm_window<-3

for_mean1<-paste('mean_alarm',1:n_alarm_indicators,sep='')

al_w<-paste("alarm_window<-as.numeric( dB2_pars()$all_forDBS$param_values_",input$district_dB2,"$value)[5]",sep='')

eval(parse(text=al_w))

for_mean2<-paste("rollapply(",as.character(al_var),',FUN=mean,width=list(-',alarm_window-1,':0),
                                          align = "center",
                                          fill = NA,na.rm = T,
                                          partial=T)',sep='')

for_mean3<-paste('tem.d %>% mutate(',paste(for_mean1,'=',for_mean2,collapse =','),')',sep='')

cat(for_mean3)
#parse(text=for_mean3)

tem.d<-eval(parse(text=for_mean3))

#tem.d<-eval(parse(text=create_alarm_inds))

tem.d$Year_F<-yrs_ch-1
## compute outbreak probability
out_prob<-predict(mod,tem.d,'response')
tem.d$outbreak_probability<-round(as.numeric(out_prob),5)
## replace with missing unobserved weeks


rep_pro<-which(!tem.d$week %in% ds_cu1$week)
tem.d$outbreak_probability[rep_pro]<-NA
##compute the other indicators
tem.d<-tem.d %>% mutate(outbreak_period=case_when(outbreak>endemic_chanel~1,
                                                  TRUE~0),
                        alarm_signal=case_when(outbreak_probability>alarm_threshold~1,
                                               is.na(outbreak_probability)~as.double(NA),
                                               TRUE~0))

tem.d<-tem.d %>% mutate(lag0=dplyr::lag(alarm_signal,0),
                        lag1=dplyr::lag(alarm_signal,1),
                        lag2=dplyr::lag(alarm_signal,2),
                        lag3=dplyr::lag(alarm_signal,3),
                        lag4=dplyr::lag(alarm_signal,4)) %>% 
  mutate(response_cat=case_when(lag0==1 & lag1==1 & lag2 %in% c(0,NA) ~1,
                                lag0==1 & lag1==1 & lag2==1 & lag3 %in% c(0,NA) ~1.5,
                                lag0==1 & lag1==1 & lag2==1  & lag3==1 ~2,
                                is.na(alarm_signal)~ as.double(NA),
                                TRUE~0.5))
tem.d$year<-as.numeric(as.character(tem.d$year.x))

## Reduce the variables to display
vars.left<-c("District","year","week","Cases","Population","outbreak",
             "endemic_chanel","alarm_threshold",
             "outbreak_probability","alarm_signal",as.character(al_var))


eval(parse(text=paste("sel.dat<-tem.d %>% dplyr::select(",paste(vars.left,collapse =','),")",sep='')))

output$pred_dis<-renderDataTable(datatable(sel.dat,
                                           options = list(autoWidth = F,
                                                          searching = FALSE)) )

##send the plots



dat_lab<-data.frame(response_cat=c("No response",
                                   "Initial response",
                                   "Early response",
                                   "Late/emergency response"),
                    x=-20,y=seq(0.65,2.5,0.5))
plot1<-ggplot(aes(x=week,y=outbreak_moving_limit),data=tem.d)+
  geom_area(aes(fill="Endemic channel"))+
  geom_line(aes(y=outbreak,col="Confirmed cases"),lwd=0.3)+
  geom_point(aes(y=outbreak,col="Confirmed cases"),size=2.5,pch=15)+
  theme_bw()+
  scale_fill_manual(values =c("Endemic channel"=grey(0.7)))+
  scale_color_manual(values =c("Confirmed cases"='red1'))+
  scale_x_continuous(breaks=2:52,limits =c(2,52))+
  theme(panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.y =element_line(linetype=2),
        panel.grid.minor.y =element_blank(),
        axis.line.x.top =element_blank(),
        panel.border =element_blank(),
        axis.line.y =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
        axis.line.x =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
        legend.position ="top",
        axis.title.y =element_blank(),
        legend.text =element_text(size=14)
  )+
  guides(fill=guide_legend(title =NULL),
         color=guide_legend(title =NULL))+
  xlab("Epidemiological week")

plot2<-ggplot()+
  
  geom_line(aes(x=week,y=outbreak_probability,col="Outbreak probability"),lwd=0.3,data=tem.d)+
  geom_point(aes(x=week,y=outbreak_probability,col="Outbreak probability"),size=2.5,pch=15,data=tem.d)+
  geom_line(aes(x=week,y=alarm_threshold,col="Alarm threshold"),lwd=0.7,data=tem.d,lty=2)+
  
  theme_bw()+
  scale_color_manual(values =c("Outbreak probability"='dark blue',
                               "Alarm threshold"="forest green"))+
  scale_x_continuous(breaks=2:52,limits =c(2,52))+
  theme(panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.y =element_line(linetype=2),
        panel.grid.minor.y =element_blank(),
        axis.line.x.top =element_blank(),
        panel.border =element_blank(),
        axis.line.y =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
        axis.line.x =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
        legend.position ="top",
        axis.title.y =element_blank(),
        legend.text =element_text(size=14)
  )+
  guides(fill=guide_legend(title =NULL),
         color=guide_legend(title =NULL))+
  xlab("Epidemiological week")


plot3<-ggplot(aes(x=week,y=outbreak_moving_limit),data=tem.d)+
  geom_area(aes(fill="Endemic channel"))+
  geom_line(aes(y=outbreak,col="Confirmed cases"),lwd=0.3)+
  geom_point(aes(y=outbreak,col="Confirmed cases"),size=2.5,pch=15)+
  geom_line(aes(x=week,y=outbreak_probability,col="Outbreak probability"),lwd=0.3,data=tem.d)+
  geom_point(aes(x=week,y=outbreak_probability,col="Outbreak probability"),size=2.5,pch=15,data=tem.d)+
  geom_line(aes(x=week,y=alarm_threshold,col="Alarm threshold"),lwd=0.7,data=tem.d,lty=2)+
  theme_bw()+
  scale_fill_manual(values =c("Endemic channel"=grey(0.7)))+
  scale_color_manual(values =c("Confirmed cases"='red1',
                               "Outbreak probability"='blue',
                               "Alarm threshold"="forest green"))+
  scale_x_continuous(breaks=2:52,limits =c(2,52))+
  theme(panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank(),
        panel.grid.major.y =element_line(linetype=2),
        panel.grid.minor.y =element_blank(),
        axis.line.x.top =element_blank(),
        panel.border =element_blank(),
        axis.line.y =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
        axis.line.x =element_line(linetype=1,colour="grey",size=0.4,lineend="butt"),
        legend.position ="top",
        axis.title.y =element_blank(),
        legend.text =element_text(size=14)
        
  )+
  guides(fill=guide_legend(title =NULL),
         color=guide_legend(title =NULL))+
  xlab("Epidemiological week")

plot4<-ggplot(aes(x=week,y=response_cat),data=tem.d)+geom_point(pch=21,size=2.5)+
  geom_hline(yintercept =0.5,col="yellowgreen",lwd=0.8)+
  geom_hline(yintercept =1,col="orange",lwd=0.8)+
  geom_hline(yintercept =1.5,col="brown",lwd=0.8)+
  geom_hline(yintercept =2,col="red",lwd=0.8)+
  geom_text(aes(x=x,y=y,label=response_cat,col=response_cat),data=dat_lab,
            show.legend =F,hjust=0,nudge_x =0.2)+
  theme_bw()+
  scale_x_continuous(breaks=seq(2,52,2))+
  
  scale_color_manual(values=c("No response"='yellowgreen',
                              "Initial response"='orange',
                              "Early response"='brown',
                              "Late/emergency response"='red'))+
  
  theme(panel.grid.minor.y =element_blank(),
        panel.grid.major.y =element_blank(),
        panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank(),
        panel.border =element_blank(),
        axis.line.x =element_line(linetype=1,
                                  colour="grey",
                                  size=0.4,
                                  lineend="butt"),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank(),
        legend.text =element_text(size=14))+
  coord_fixed(6,ylim =c(0.3,3),xlim = c(-20,52))+
  xlab("Epidemiological week")

output$db2_plot1<-renderPlot(plot1)
output$db2_plot2<-renderPlot(plot2)
output$db2_plot3<-renderPlot(plot3)
output$db2_plot4<-renderPlot(plot4)


cc_d<-paste("cc_v<-dB2_pars()$all_forDBS$param_values_",input$district_dB2,sep='')
eval(parse(text=cc_d))

output$a_vars<-renderPrint({
  cat(as.character(al_var),sep ='\n')
})

s1<-nrow(cc_v)
s2<-s1-1
sp_n<-which(cc_v$parameter=="Spline")

output$alr_vars<-renderTable(cc_v[2:s2,])

output$s_vars<-renderPrint({
  if(cc_v$value[sp_n]=="TRUE"){
    s_c<-as.character(str_split(cc_v$value[s1],',',simplify =T))
    cat(as.character(s_c),sep ='\n')
  }
  
})
