# code to create the plot for ATE weights

#- weights already created using the WeightIt package
#- data = dfum3


p = ggplot(data = dfum3, aes(x = ps, y = weights, 
color = factor(snh) )) + geom_point()

p2 = p + scale_x_continuous(breaks = seq(0,1,0.1)) + 
  theme_Publication() + scale_y_continuous(breaks = seq(0,30,5))


p3 = p2 + xlab("Propensity Score") + ylab("ATE Weights")

