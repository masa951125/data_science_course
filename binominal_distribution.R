#binominal distribution

x <-c(0:5)
data.frame(x,y=dbinom(x,5,1/2))%>% ggplot(aes(x,y))+geom_bar(stat="identity")

x <-c(0:20)
data.frame(x,y=pbinom(x,20,1/5))%>% ggplot(aes(x,y))+geom_line()
data.frame(x,y=dbinom(x,20,1/5))%>% ggplot(aes(x,y))+geom_line()
data.frame(x,y=1-pbinom(x,20,1/5))%>% ggplot(aes(x,y))+geom_line()