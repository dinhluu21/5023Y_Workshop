
darwin_summary <- darwin %>% group_by(type) %>% summarise(mean=mean(height))

darwin_total <- left_join(darwin, darwin_summary)

darwin_total %>% 
  mutate(difference=mean-height) %>% 
  mutate(squaredE=difference^2) %>% 
  mutate(squaredT=(height-18.9)^2) %>% 
  mutate(squaredR=squaredT-squaredE) %>% 
  summarise(sumE=sum(squaredE), sumR=sum(squaredR), sumT=sum(squaredT))



lsmodel1

# residual standard error
sqrt(242/28)

Calculate F from sum of squares 
#SSR/k / SSE/df

F <- (51.4/1)/(242/28)


# Estimate of Standard Error
Sum of Errors
 sqrt((242/14))/sqrt(30)
[1] 0.7590721
 
# Estimate of Standard Error of Difference
 
 > sqrt((242/14))/sqrt(15)
 [1] 1.07349
 

 darwin_total %>% 
   mutate(difference=mean-height) %>% 
   mutate(squaredE=difference^2) %>% 
   mutate(squaredT=(height-20.1917)^2) %>% # Intercept is first treatment
   mutate(squaredR=squaredT-squaredE) %>% 
   summarise(sumE=sum(squaredE), sumR=sum(squaredR), sumT=sum(squaredT))
 
 
 F <- (103/2)/(242/28) 
 
 
 
 
 # Bee data
 
 
 hive_a = rt(df=62, mean=31.4, sd=15)
 hive_b = rt(n=67, mean=36.73, sd= 13.63818)
 t.test(hive_b,hive_a, var.equal=T)
 
 # t distribution
 diff <- 36.7-31.4
 n <- 129
 
 error <- qt(0.975,df=n-2)*2.53084
 a-error
 a+error
 