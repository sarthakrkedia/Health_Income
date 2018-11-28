library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)

mortality_data <- read.csv("mortality.csv")
life_expectancy_data <- read.csv("life_expectancy.csv")
death_causes_data <- read.csv("causes_of_death.csv")

shinyServer(
  
  function(input, output){
    
    #Years and Region
    output$plot_mort <- renderPlot({
      
      filtered_mor <- filter(mortality_data,Region == input$Region)
      
      if (input$Region == "All Region")
      {
 
        grp_mort <- group_by(mortality_data,Year,Region)
        summ_mort <- summarise(grp_mort,Mort=mean(Mortality))
        ggplot(summ_mort,aes(Year,Mort)) + geom_line(size=1.5,color="#F5B041") + facet_grid(~Region) +
          theme_pander()  + xlab("Years") + ylab("Mortality Rate per 1000 people") +
          ggtitle("Mortality Rate") +
          theme(legend.position = "left",axis.text=element_text(size=10),legend.text=element_text(size=14))
      }
      else
      {
      fil_grp_mort <- group_by(filtered_mor,Year,Region)
      ggplot(fil_grp_mort,aes(Year,Mortality)) + geom_smooth(method="lm", se=F) + geom_point(fill="#b3cde3") + theme_pander() + theme(legend.position="left",axis.text=element_text(size=18),legend.text=element_text(size=14)) +
      xlab("Years") + ylab("Mortality Rate per 1000 people") + ggtitle("Mortality Rate")
      }
    })
    
    output$plot_life <- renderPlot({
      filtered_exp <- filter(life_expectancy_data,Region == input$Region)
      
      if (input$Region == "All Region")
      {

        grp_life <- group_by(life_expectancy_data,Year,Region)
        summ_life <- summarise(grp_life,Life_mean=mean(Life))
        ggplot(summ_life,aes(Year,Life_mean)) + geom_line(size=1.5,color="#8A2C02") + facet_grid(~Region) + theme_pander() + theme(legend.position="none",axis.text=element_text(size=10)) +
        xlab("Years") + ylab("Life Expectancy at Birth") + ggtitle("Life Expectancy")

      }
      else
      {
        
        fil_grp_life <- group_by(filtered_exp,Year,Region)
        ggplot(fil_grp_life,aes(Year,Life)) + geom_point(fill="#ccebc5") + geom_smooth(method="lm", se=F)+ theme_pander() + theme(legend.position="none",axis.text=element_text(size=18)) +
        xlab("Years") + ylab("Life Expectancy at Birth") + ggtitle("Life Expectancy")
      }
    })
    
    output$plot_cau <- renderPlot({
      
      filtered_cau <- filter(death_causes_data,Region == input$Region)
      
      if (input$Region == "All Region")
      {
        grp_cause <- group_by(death_causes_data,Cause)
        summ_cause <- summarise(grp_cause,death_mean=sum(Deaths,na.rm = TRUE))
        order_cause <- summ_cause[order(-summ_cause$death_mean),]
        ord <- order_cause[1:7,]
        ggplot(ord,aes(reorder(Cause,death_mean),death_mean)) + geom_bar(stat="identity",fill="#F08B33") + coord_flip() +
          geom_text(aes(label = Cause),size = 6, vjust="inward",hjust="inward") +
          theme(axis.text.y=element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust = 0.5)) + theme_pander() +
          ggtitle("Top 7 Causes of Death in the World") + ylab("Number of deaths in Thousands") + xlab("Major Causes of Death") + 
          theme(axis.text=element_text(size=18))
      }
      else
      {
        fil_grp_cause <- group_by(filtered_cau,Cause)
        fil_summ_cause <- summarise(fil_grp_cause,death_mean=sum(Deaths,na.rm = TRUE))
        fil_order_cause <- fil_summ_cause[order(-fil_summ_cause$death_mean),]
        fil_ord <- fil_order_cause[1:7,]
        ggplot(fil_ord,aes(reorder(Cause,death_mean),death_mean)) + geom_bar(stat="identity",fill="#F08B33") + coord_flip() +
          geom_text(aes(label = Cause),size = 6, vjust="inward",hjust="inward") +
          theme(axis.text.y=element_blank(),
                axis.ticks = element_blank(),
                panel.border = element_blank()) + theme_pander() +
          ggtitle("Top 7 Causes of Death in" , input$Region) + ylab("Number of deaths in Thousands") + xlab("Major Causes of Death") +
          theme(axis.text=element_text(size=18))
      }
    })
    
    output$plot_mort_inc <- renderPlot({
      
      filtered_mor_inc <- filter(mortality_data,Income == input$Income)
      
      if (input$Income == "All Income Groups")
      {
        grp_mort_inc <- group_by(mortality_data,Year,Income)
        summ_mort_inc <- summarise(grp_mort_inc,Mort=mean(Mortality))
        ggplot(summ_mort_inc,aes(Year,Mort)) + geom_line(size=1.5) + aes(color=Income)  + theme_pander() +theme(legend.position="bottom",axis.text=element_text(size=18),,legend.text=element_text(size=14)) + 
          xlab("Years") + ylab("Mortality Rate per 1000 people") + ggtitle("Mortality Rate")
      }
      else
      {
        fil_grp_mort_inc <- group_by(filtered_mor_inc,Year,Income)
        fil_summ_mort_inc <- summarise(fil_grp_mort_inc,Mort=mean(Mortality))
        ggplot(fil_summ_mort_inc,aes(Year,Mort)) + geom_area(fill="#67BCDB") + theme_pander() + theme(legend.position="left",axis.text=element_text(size=18),,legend.text=element_text(size=14)) +
        xlab("Years") + ylab("Mortality Rate per 1000 people") + ggtitle("Mortality Rate")
      }
    })
    
    output$plot_life_inc <- renderPlot({
      
      filtered_exp_inc <- filter(life_expectancy_data,Income == input$Income)
      
      if (input$Income == "All Income Groups")
      {
        
        grp_life_inc <- group_by(life_expectancy_data,Year,Income)
        summ_life_inc <- summarise(grp_life_inc,Life_mean=mean(Life))
        ggplot(summ_life_inc,aes(Year,Life_mean)) + geom_line(size=1.5) + aes(color=Income) + theme_pander()  + 
          theme(legend.position="none",axis.text=element_text(size=18)) + xlab("Years") + ylab("Life Expectancy at Birth") + ggtitle("Life Expectancy")
        
      }
      else
      {
        
        fil_grp_life_inc <- group_by(filtered_exp_inc,Year,Income)
        fil_summ_life_inc <- summarise(fil_grp_life_inc,Life_mean=mean(Life))
        ggplot(fil_summ_life_inc,aes(Year,Life_mean)) + geom_area(fill="#AA2C02") + theme_pander() + theme(legend.position="none",axis.text=element_text(size=18)) +
          xlab("Years") + ylab("Life Expectancy at Birth") + ggtitle("Life Expectancy")
        
      }
    })
    
    output$plot_cau_inc <- renderPlot({
      
      filtered_cau <- filter(death_causes_data,Income == input$Income)
      
      if (input$Income == "All Income Groups")
      {
        grp_cause_inc <- group_by(death_causes_data,Cause)
        summ_cause_inc <- summarise(grp_cause_inc,death_mean=sum(Deaths))
        order_cause_inc <- summ_cause_inc[order(-summ_cause_inc$death_mean),]
        ord <- order_cause_inc[1:7,]
        ggplot(ord,aes(reorder(Cause,death_mean),death_mean)) + geom_bar(stat="identity",fill="#F5B041") + coord_flip() +
          geom_text(aes(label = Cause),size = 6, vjust="inward",hjust="inward") +
          theme(axis.text.y=element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust = 0.5)) + theme_pander() +
        ggtitle("Top 7 Causes of Death in the World") + ylab("Number of deaths in Thousands") + xlab("Major Causes of Death") + 
          theme(axis.text=element_text(size=18))
      }
      else
      {
        fil_grp_cause_inc <- group_by(filtered_cau,Cause)
        fil_summ_cause_inc <- summarise(fil_grp_cause_inc,death_mean=sum(Deaths))
        fil_order_cause_inc <- fil_summ_cause_inc[order(-fil_summ_cause_inc$death_mean),]
        fil_ord <- fil_order_cause_inc[1:7,]
        ggplot(fil_ord,aes(reorder(Cause,death_mean),death_mean)) + geom_bar(stat="identity",fill="#F5B041") + coord_flip() +
          geom_text(aes(label = Cause),size = 6, vjust="inward",hjust="inward") +
          theme(axis.text.y=element_blank(),
                axis.ticks = element_blank()) + theme_pander() +
        ggtitle("Top 7 Causes of Death in", input$Income) + ylab("Number of deaths in Thousands") + xlab("Major Causes of Death") + 
          theme(axis.text=element_text(size=18))
      }
    })
    
    output$plot_life_mor_cou1 <- renderPlot({
      
      filtered_cou1_life <- filter(life_expectancy_data,Country == input$Country1)
      
      grp_cou1_life <- group_by(filtered_cou1_life,Year,Country)
      summ_cou1_life <- summarise(grp_cou1_life,life_mean=mean(Life))
      
      filtered_cou2_life <- filter(life_expectancy_data,Country == input$Country2)
      
      grp_cou2_life <- group_by(filtered_cou2_life,Year,Country)
      summ_cou2_life <- summarise(grp_cou2_life,life_mean=mean(Life))
      
      
      ggplot() + 
        geom_line(data = summ_cou1_life, aes(x = Year, y = life_mean, color = input$Country1),size=1.1) + theme_pander() +
        geom_line(data = summ_cou2_life, aes(x = Year, y = life_mean, color = input$Country2),size=1.1) + theme_pander() +
        theme(legend.position="none",axis.text=element_text(size=18)) + xlab("Years") + ylab("Life Expectancy")
    })
    
    output$plot_life_mor_cou2 <- renderPlot({
      
      filtered_cou1_mort <- filter(mortality_data,Country == input$Country1)
      
      grp_cou1_mor <- group_by(filtered_cou1_mort,Year,Country)
      summ_cou1_mor <- summarise(grp_cou1_mor,mort_mean=mean(Mortality))
      
      filtered_cou2_mort <- filter(mortality_data,Country == input$Country2)
      
      grp_cou2_mor <- group_by(filtered_cou2_mort,Year,Country)
      summ_cou2_mor <- summarise(grp_cou2_mor,mort_mean=mean(Mortality))
      
      ggplot() + 
        geom_line(data = summ_cou1_mor, aes(x = Year, y = mort_mean, color = input$Country1),size=1.1) + theme_pander() +
        geom_line(data = summ_cou2_mor, aes(x = Year, y = mort_mean, color = input$Country2),size=1.1) + theme_pander() +
        theme(legend.position="right",axis.text=element_text(size=18),legend.title=element_blank()) + xlab("Years") + ylab("Mortality Rate")
    })
    
    output$plot_life_mor_cou3 <- renderPlot({
      
      filtered_cou1_cau <- filter(death_causes_data,Country == input$Country1)
      
      fil_grp_cause_cou1 <- group_by(filtered_cou1_cau,Cause)
      fil_summ_cause_cou1 <- summarise(fil_grp_cause_cou1,death_mean=sum(Deaths))
      fil_order_cause_cou1 <- fil_summ_cause_cou1[order(-fil_summ_cause_cou1$death_mean),]
      fil_ord_cou1 <- fil_order_cause_cou1[1:5,]
      ggplot(fil_ord_cou1,aes(reorder(Cause,death_mean),death_mean)) + geom_bar(stat="identity",fill="#F08B33") + coord_flip() +
        geom_text(aes(label = Cause),size = 6, vjust="inward",hjust="inward") +
        theme(axis.text.y=element_blank(),
              axis.ticks = element_blank()) + theme_pander() +
        ggtitle("Top 5 Causes of Death in",input$Country1) + ylab("Number of deaths in Thousands") + xlab("Major Causes of Death") + 
        theme(axis.text=element_text(size=18))
    })
    
    output$plot_life_mor_cou4 <- renderPlot({
      
      filtered_cou2_cau <- filter(death_causes_data,Country == input$Country2)
      
      fil_grp_cause_cou2 <- group_by(filtered_cou2_cau,Cause)
      fil_summ_cause_cou2 <- summarise(fil_grp_cause_cou2,death_mean=sum(Deaths))
      fil_order_cause_cou2 <- fil_summ_cause_cou2[order(-fil_summ_cause_cou2$death_mean),]
      fil_ord_cou2 <- fil_order_cause_cou2[1:5,]
      ggplot(fil_ord_cou2,aes(reorder(Cause,death_mean),death_mean)) + geom_bar(stat="identity",fill="#F49F05") + coord_flip() +
        geom_text(aes(label = Cause),size = 6, vjust="inward",hjust="inward") +
        theme(axis.text.y=element_blank(),
              axis.ticks = element_blank()) + theme_pander() +
        ggtitle("Top 5 Causes of Death in",input$Country2) + ylab("Number of deaths in Thousands") + xlab("Major Causes of Death") +
        theme(axis.text=element_text(size=18))
    })
  }
)