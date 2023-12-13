functions {
  array[] real dz_dt(real t, // time
                     array[] real z,
                     array[] real theta, // parameters
                     array[] real x_r, // unused data
                     array[] int x_i) {
    
    real alpha3 = theta[1];
    real k3 = theta[2];
    real K3 = theta[3];
    real n3 = theta[4];
    real d = theta[5];
    
    int D = x_i[1];
    vector[D] dR_dt;
    real R;
    
    for(i in 1:D) {
     R = z[i];
     real yt =(pow(K3, n3)/(pow(K3, n3)+pow(x_r[i], n3))+alpha3)*k3;
     dR_dt[i] = yt-d*R;
    
    }
    
     return to_array_1d(dR_dt);
  }
}
data {
  int<lower=0> N; // number of measurement times
  int<lower=0> X;
  array[N] real t; 
  array[X] real y_init;
  array[X] real input;
  array[5] real<lower=0> phi; 
  array[5] real<lower=0> phi_sd; 
}
parameters {
  array[5] real<lower=0> theta; 
//  array[2] real<lower=0> z_init; 
  array[X] real<lower=0> sigma; // measurement errors
}
transformed parameters {
    array[N, X] real z = integrate_ode_rk45(dz_dt, y_init, 0.0, t, theta,
                                          input, {X});
}
model {
  for(k in 1:5) {
    theta[k] ~ normal(phi[k], phi_sd[k]);
  }
  sigma ~ normal(0, 1);
}
generated quantities {
  real r_obs[N,X];
  
  for(i in 1:X) {
    for (n in 1:N) {
      r_obs[n,i] = normal_rng(z[n,i], sigma[i]);
    }
  }
}

