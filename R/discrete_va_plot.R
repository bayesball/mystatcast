discrete_va_plot <- function(df, 
                             br=seq(0, 1, length.out=10),
                             pal="YlOrBr", method="hit"){
  df <- mutate(df,
              Outcome=ifelse(Event %in% 
              c("Double", "Single", "Triple", "Home Run"), 
                                  "Hit", "Out"),
              outcome=ifelse(Outcome=="Hit", 1, 0),
              lw=ifelse(Event=="Single", 0.70, 
                        ifelse(Event=="Double", 1.00,
                        ifelse(Event=="Triple", 1.27,
                        ifelse(Event=="Home Run", 1.65, 0)))))
 
if(method=="hit"){ 
  fit2 <- gam(outcome ~ s(Launch_Angle, Exit_Velocity),
              family=binomial, data=df)
  df <- mutate(df,
          Prob_Hit = exp(predict(fit2)) / (1 + exp(predict(fit2))),
          prob_Hit = cut(Prob_Hit, breaks=br))
  p4 <- ggplot(df, 
               aes(x=Launch_Angle, y=Exit_Velocity, color=prob_Hit)) +
    geom_point() +
    geom_vline(xintercept = 0, color="black") +
    scale_color_brewer(palette=pal) +
    ggtitle("GAM Probability of Hit") +
    xlim(-50, 70) +
    ylim(50, 125)
}
if(method=="lw"){
  fit2 <- gam(lw ~ s(Launch_Angle, Exit_Velocity),
                data=df)
  df <- mutate(df,
               Linear_Weight = cut(lw, breaks=br))
  p4 <- ggplot(df, 
      aes(x=Launch_Angle, y=Exit_Velocity, color=Linear_Weight)) +
    geom_point()  +
    scale_color_brewer(palette=pal) +
    ggtitle("GAM Linear Weight") +
    xlim(-50, 70) +
    ylim(50, 125)
}
  print(p4)
}
