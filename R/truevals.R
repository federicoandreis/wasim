trueval <- c(N=length(y),X=sum(x),mean=mean(y),mean2=mean(y^2),mean3=mean(y^3),mean4=mean(y^4),
             q1=quantile(y,type=1,probs=.25),q2=quantile(y,type=1,probs=.50),q3=quantile(y,type=1,probs=.75),
             gamma1=mean((y-mean(y))^3)/mean((y-mean(y))^2)^1.5,
             gamma2=mean((y-mean(y))^4)/mean((y-mean(y))^2)^2,
             galton=(quantile(y,type=1,.25)+quantile(y,type=1,.75)-2*quantile(y,type=1,.5))/
               (quantile(y,type=1,.75)-quantile(y,type=1,.25)),
             qsr=sum(y[y<=quantile(y,.2,type=1)])/
               sum(y[y>=quantile(y,.8,type=1)]))
