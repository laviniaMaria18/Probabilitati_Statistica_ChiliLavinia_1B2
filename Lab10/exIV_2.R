binomial = function(n, p, k) 
{
  exp = n*p;
  var = n*p*(1 - p);
  s = sqrt(var);
  q = (k + 0.5)/s;
  return(1 - pnorm(q));
}

binomial(60, 0.3, 8)
