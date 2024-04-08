Poisson=function(n,landa)
{
  x=0:(n-1)
  y=dpois(x,landa)
  barplot(y,space=1, main='Poisson', sub="ex 10",xlab="axa x", ylab="axa y",col='purple')
}
