data {
  int<lower=0> N; 
  int<lower=0> X;
  array[X] real input;
  int<lower=0> T; 
  array[T] real ts; 
  array[N,X] real z_init;
  array[N,5] real<lower=0> phi; 
  array[N,5] real<lower=0> phi_sd; 
  real<lower=0> uplim;
  real<lower=0> downlim;
}

parameters {
  array[N,5] real<lower=0> theta; 
  array[N,X] real<lower=0> yt;
}
model {

  for(n in 1:N) {
    for(k in 1:5) {
      theta[n,k] ~ normal(phi[n,k], phi_sd[n,k]);
  }
    for(i in 1:X) {
        yt[n,i] ~ normal( ((pow(theta[n,3], theta[n,4])/(pow(theta[n,3], theta[n,4])+pow(input[i], theta[n,4])) + theta[n,1]) * theta[n,2], 0.1) ;
    }
  }
  
}
generated quantities {
  array[N,X,T] real<lower=0> z;
  
    for(n in 1:N) {
    for (i in 1 : X) {
      for (t in 1 : T) {
        z[n,i,t] = (z_init[n,i] - yt[n,i]/theta[n,5])*exp(-theta[n,5]*t)+yt[n,i]/theta[n,5];
      }
    }
    }
  
  array[N,X,T] real<lower=0> probup;
  
  for(n in 1:N) {
    for (i in 1 : X) {
      for (t in 1 : T) {
      probup[n,i,t] = z[n,i,t] >= uplim;
    }
    }
  }
  
  array[N,X,T] real<lower=0> probdown; 
  
  for(n in 1:N) {
    for (i in 1 : X) {
    for (t in 1 : T) {
      probdown[n,i,t] = z[n,i,t] <= downlim;
    }
    }
  }

}
