data {
	int<lower=0> N; 
	real<lower=0> xi[N]; 
	real<lower=0> obs[N];
	int I;
	int<lower=0,upper = N> from[I];
	int<lower=0,upper = N> to[I];
	#matrix[N,N] adja;
}
parameters {
	real theta[N];
	real<lower=0> s_t;
	real<lower=0> s_Y;
}

model {
  // for (j in 1:N)
  //   for (i in 1:N)
  //     if(adja[i,j]!=0)
  //       theta[i] ~ normal(theta[j],s_t);
  target += normal_lpdf(theta[from] | theta[to], s_t);
	for (i in 1:N)
	  target += normal_lpdf(obs[i]|theta[i]*xi[i],s_Y);
		//obs[i] ~ normal(theta[i]*xi[i],s_Y); 
	s_Y ~ normal(0,0.1);
  s_t ~ normal(0,1);
}
