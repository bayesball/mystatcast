statcast_graphs <- function(df){
  name <- df$Batter
  df <- mutate(df,
               Outcome=ifelse(Event %in% 
                        c("Double", "Single", "Triple", "Home Run"), 
                              "Hit", "Out"),
               outcome=ifelse(Outcome=="Hit", 1, 0))
  
  # initial plot
  
  p1 <- ggplot(df, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
    geom_point() + ggtitle(paste(name, "- In-Play Outcomes")) 
  print(p1)
  
  # fit gam
  
  fit2 <- gam(outcome ~ s(Launch_Angle, Exit_Velocity),
              family=binomial, data=df)
  
  df <- mutate(df,
          Prob_Hit = exp(predict(fit2)) / (1 + exp(predict(fit2))))
  
  # second plot showing fitted hit probabilities
  
  p2 <- ggplot(df, aes(x=Launch_Angle, y=Exit_Velocity, color=Prob_Hit)) +
    geom_point() +
    scale_colour_gradient(limits=c(0, 1), low="blue", high="red") +
    geom_vline(xintercept = 0, color="blue") +
    ggtitle(paste(name, "- Hit Probabilities"))
  print(p2)
  
  v <- round(mean(df$Exit_Velocity) * c(.9, 1, 1.1), 1)
  la <- seq(-10, 40, length=100)
  data.predict <- rbind(data.frame(Exit_Velocity=v[1], 
                                   Launch_Angle=la),
                        data.frame(Exit_Velocity=v[2], 
                                   Launch_Angle=la),
                        data.frame(Exit_Velocity=v[3], 
                                   Launch_Angle=la)) 
  
  lp <- predict(fit2, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  data.predict$Exit_Velocity <- factor(data.predict$Exit_Velocity)
  
  # third plot showing probability of hit as function of
  # launch angle
  
  p3 <- ggplot(data.predict, 
               aes(Launch_Angle, Probability, 
                   group=Exit_Velocity, color=Exit_Velocity)) + 
    geom_line() + ggtitle(name) +
    ylab("Probability of Hit")
  print(p3)
}
