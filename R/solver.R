solver = function(comp_input_data){
  ## Solve the problems
  ##
  ## comp_input_data  compound style input data for a particular neptun code
  ##
  ## take care of vectorized solution!
  ## !! shinyjs is not working with 'id' containing '.' thus they are replaced by '-'
  ## solved, the developer modified the source, we should wait for CRAN update
  
  source('R/add2dataframe.R', local = T)
  
  solu_data = as.data.frame(row.names(comp_input_data))
  names(solu_data) = 'Neptun'
  
  ## MODIFY THIS PART $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ## Problem 1 ----------------------
  prob_n = 1
  # (a)
  subprob_l = 'a'
  
  F = comp_input_data$p1_a_F
  l = comp_input_data$p1_a_l
  L_0 = comp_input_data$p1_a_L_0
  A = comp_input_data$p1_a_A
  E=200000
  
  f_0=sqrt(L_0^2-l^2)
  K_0=(L_0*F)/(2*f_0)
  sigma_0=K_0/A
  epsilon_0=sigma_0/E
  deltaL_0=L_0*epsilon_0
  f_1=sqrt((L_0-deltaL_0)^2-l^2)
  w_1=f_1-f_0
  
  solu_data = add2dataframe(K_0, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(epsilon_0, dim = '', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(w_1, dim = 'm', prob_n, subprob_l, solu_data, point = 0.1)
  
  # (b)
  subprob_l = 'b'
  
  L_1=L_0-deltaL_0
  K_1=(L_1*F)/(2*f_1)
  sigma_1=K_1/A
  epsilon_1=sigma_1/E
  deltaL_1=L_1*epsilon_1
  f_2=sqrt((L_1-deltaL_1)^2-l^2)
  w_2=f_2-f_1
  
  L_2=L_1-deltaL_1
  K_2=(L_2*F)/(2*f_2)
  sigma_2=K_2/A
  epsilon_2=sigma_2/E
  deltaL_2=L_2*epsilon_2
  f_3=sqrt((L_2-deltaL_2)^2-l^2)
  w_3=f_3-f_2
  
  L_3=L_2-deltaL_2
  K_3=(L_3*F)/(2*f_3)
  sigma_3=K_3/A
  epsilon_3=sigma_3/E
  deltaL_3=L_2*epsilon_3
  f_4=sqrt((L_3-deltaL_3)^2-l^2)
  w_4=f_4-f_3
  
  solu_data = add2dataframe(K_3, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(epsilon_3, dim = '', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(w_4, dim = 'm', prob_n, subprob_l, solu_data, point = 0.1)
  
  ## Problem 2 ----------------------
  prob_n = 2
  subprob_l = ''
  
  a = comp_input_data$p2__a
  b = comp_input_data$p2__b
  phi = comp_input_data$p2__phi
  sigma_c=20
  sigma_s=500
  
  
  A_c=a*b
  A_s=4*phi^2*3.14/4
  
  N_c=A_c*sigma_c
  N_s=A_s*sigma_s
  
  N_RA=N_c+N_s*2/5
  N_RB=N_c+N_s
  
  solu_data = add2dataframe(N_RA, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.2)
  solu_data = add2dataframe(N_RB, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.2)

  ## Problem 3 ----------------------
  prob_n = 3
  #(a)
  subprob_l = 'a'
  
  h = comp_input_data$p3__h
  p_1 = comp_input_data$p3__p_1
  p_2 = comp_input_data$p3__p_2
  y = comp_input_data$p3__y
  
  f=200
  f_tau=90
  A=2*100*5+140*5
  A_w=140*5
  A_f=100*5
  I=(100*150^3-95*140^3)/12
  
  M_fajl1=3*p_1*h^2/6
  M_fajl2=(p_2-p_1)*h^2/6
  M_fajl=M_fajl1+M_fajl2
  M=M_fajl*y
  z=0.145
  N=M/z
  sigma=N/A_f
  
  solu_data = add2dataframe(M, dim = 'kNm', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(sigma, dim = 'N/mm^2', prob_n, subprob_l, solu_data, point = 0.1)
  
  #(b)
  subprob_l = 'b'

  V_fajl=(p_1+p_2)*h/2
  V=V_fajl*y
  tau=V/A_w
  
  solu_data = add2dataframe(V, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(tau, dim = 'N/mm^2', prob_n, subprob_l, solu_data, point = 0.1)
  
  #(c)
  subprob_l = 'c'
 
  sigma_max=f
  y_max1=f*z*A_f/M_fajl
  
  tau_max=f_tau
  y_max2=f_tau*A_w/V_fajl

  solu_data = add2dataframe(y_max1, dim = 'm', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(y_max2, dim = 'm', prob_n, subprob_l, solu_data, point = 0.1)
  
  ## Problem 4 ----------------------
  prob_n = 4
  subprob_l = ''
  
  y = comp_input_data$p3__y
  p_2 = comp_input_data$p3__p_2
  
  M_ed=p_2*y^2/8
  f=18
  N=M_ed/z
  b=sqrt(6*M_ed*1000/f)
  
  solu_data = add2dataframe(M_ed, dim = 'kNm/m', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(b, dim = 'mm', prob_n, subprob_l, solu_data, point = 0.1)

  
  ## Problem 5 ----------------------
  prob_n = 5
  
  F_w = comp_input_data$p5__F_w
  l = comp_input_data$p5__l
  a = comp_input_data$p5__a
  d = comp_input_data$p5__d
  h = comp_input_data$p5__h
  
  F_3=0
  F_1=F_w*l/(2*(l-a))
  F_2=F_w*(l/2-a)/(l-a)
  M_1=F_1*h
  
  solu_data = add2dataframe(F_1, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(F_2, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(F_3, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(M_1, dim = 'kNm', prob_n, subprob_l, solu_data, point = 0.1)
  
 
  ## Problem 6 ----------------------
  prob_n = 6
  
  Q = comp_input_data$p6__Q
  l = comp_input_data$p5__l
  a = comp_input_data$p5__a
  d = comp_input_data$p5__d
  h = comp_input_data$p5__h
  
  F_1=Q*d/(2*(l-a))
  F_2=F_1
  F_3=Q
  M_1=F_1*h
  
  solu_data = add2dataframe(F_1, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(F_2, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(F_3, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(M_1, dim = 'kNm', prob_n, subprob_l, solu_data, point = 0.1)
  
  
  ## Problem 7 ----------------------
  
  
  ## Problem 8 ----------------------
  prob_n = 8
  
  R   = comp_input_data$p8__R
  rho = comp_input_data$p8__varrho
  t = comp_input_data$p8__t
  f = comp_input_data$p8__f
  p = comp_input_data$p8__p
  
  sigma=p*rho/(2*t*1000)
  
  solu_data = add2dataframe(sigma, dim = 'kN/mm^2', prob_n, subprob_l, solu_data, point = 0.2)
  
  ## Problem 9 ----------------------
  prob_n = 9
  R   = comp_input_data$p8__R
  varrho = comp_input_data$p8__varrho
  t = comp_input_data$p8__t
  f = comp_input_data$p8__f
  p = comp_input_data$p8__p
  
  F=p*R^2*pi/1000
  a=F*1000/(2*R*pi)
  alpha=asin(R/varrho)
  N=a/(sin(alpha))
  p_p=N*cos(alpha)
  H_ny=R*p_p/1000
    
  solu_data = add2dataframe(F, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(N, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(p_p, dim = 'N/m', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(H_ny, dim = 'kN', prob_n, subprob_l, solu_data, point = 0.1)
  
  ## Problem 10 ----------------------
  prob_n = 10
  D = comp_input_data$p10__D
  rho = comp_input_data$p10__rho
  t = comp_input_data$p10__t
  p = comp_input_data$p10__p
  a = comp_input_data$p10__a
  b = comp_input_data$p10__b
  mu=0.5
  gamma_g=0.9
  gamma_vb=25
  sigma=p*rho/(1000*t)
  R=D/2
  
  F=p*R*2/10
  a=p*R
  alpha=asin(R/rho)
  N=a/sin(alpha)
  F_destab=N*cos(alpha)
  H_ny=R*F_destab
  
  F_vb=gamma_g*a*b*gamma_vb
  F_stab=mu*F_vb
  
  solu_data = add2dataframe(sigma, dim = 'N/mm^2', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(F, dim = 'kN/m', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(N, dim = 'kN/m', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(F_destab, dim = 'kN/m', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(H_ny, dim = 'kN/m', prob_n, subprob_l, solu_data, point = 0.1)
  solu_data = add2dataframe(F_stab, dim = 'kN/m', prob_n, subprob_l, solu_data, point = 0.1)
  
  ## MODIFY THIS PART $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  # save RData for furher use
  save(solu_data, file = 'data/solu_data.RData')
  
  message('solu_data.RData has been succesfully generated!')
  
  return(solu_data)
  
}

