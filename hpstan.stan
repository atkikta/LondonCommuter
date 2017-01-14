data {
	int<lower=0> N; 
	real<lower=0> xi[N]; 
	real<lower=0> obs[N];
}
parameters {
	real theta[N];
	real sigma;
}

model {
	for(i in 1:N)
		obs[i] ~ normal(theta[i]*xi[i],sigma); 
	//for (i in 1:N)
		//theta[i]~normal(0,1); 
}
