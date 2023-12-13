functions {
  array[] real dz_dt(real t, // time
                     array[] real z,
                     array[] real theta, // parameters
                     array[] real x_r, // unused data
                     array[] int x_i) {
    real M = z[1];
    real N = z[2];
    
    real sq = pow(M+N+1/theta[1], 2) - 4*M*N;
    real R = (-pow(sq, 0.5) + M + N + 1/theta[1])/2;
    
    real dM_dt =  theta[2] - theta[4]*(M-R);
    real dN_dt = theta[3] - theta[4]*(N-R);
    
    return {dM_dt, dN_dt};
  }
}
data {
  int<lower=0> N; // number of measurement times
  array[N] real t; // measurement times > 0
  array[2] real y_init; // initial measured populations
  array[4] real<lower=0> phi; 
  array[4] real<lower=0> phi_sd; 
}
parameters {
  array[4] real<lower=0> theta; 
//  array[2] real<lower=0> z_init; 
  array[3] real<lower=0> sigma; // measurement errors
}
transformed parameters {
  array[N, 2] real<lower=0> z = integrate_ode_rk45(dz_dt, y_init, 0, t, theta,
                                          rep_array(0.0, 0), rep_array(0, 0));
}
model {

  for(k in 1:4) {
    theta[k] ~ normal(phi[k], phi_sd[k]);
  }
  sigma ~ normal(0, 1);
}
generated quantities {
  real m_obs[N, 1];
  real n_obs[N, 1];
  real<lower=0> r[N, 1];
  real r_obs[N, 1];
  
  for (n in 1:N) {
  real e = theta[1];
  
  real sq = pow(z[n,1]+z[n,2]+1/e,2) - 4*z[n,1]*z[n,2];
  real sqroot = pow(sq, 0.5);
  r[n, 1] = (-sqroot + z[n,1] +z[n,2] + 1/e)/2;
  r_obs[n, 1] = normal_rng((-sqroot + z[n,1] +z[n,2] + 1/e)/2, sigma[3]);
  }
  
  for (n in 1:N) {
  m_obs[n, 1] = normal_rng(z[n, 1]-r[n, 1], sigma[1]);
  }
  
  for (n in 1:N) {
  n_obs[n, 1] = normal_rng(z[n, 2]-r[n, 1], sigma[2]);
  }
  
}

