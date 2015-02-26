library(ggplot2)
x=c(0,2,5)
y=c(1,4,29)
xout=seq(0,5,0.5)
s1=spline(x,y,xout=xout,method='natural')
s2=spline(x,y,xout=xout,method='fmm')
s3=spline(x,y,xout=xout,method='hyman')
q <- ggplot(as.data.frame(s1),aes(x,y)) +
  geom_line(size=1.2,col='red') +
  geom_line(data=as.data.frame(s2),size=1.2,col='blue') +
  geom_line(data=as.data.frame(s3),size=1.2,col='green') +
  geom_point(data=data.frame(x=x,y=y),size=4)
print(q)