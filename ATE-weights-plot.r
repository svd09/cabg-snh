# code to create the plot for ATE weights

#- weights already created using the WeightIt package
#- data = dfum3


p = ggplot(data = dfum3, aes(x = ps, y = weights, 
color = factor(snh) )) + geom_point()

p2 = p + xlim(0,1,by = 0.1)