using DifferentialEquations
using DataFrames
using Gadfly

a_=1.5
b_=1.0
c_=3.0
d_=1.0

f = @ode_def LotkaVolterra begin
  dx = a*x - b*x*y
  dy = -c*y + d*x*y
end a=a_ b=b_ c=c_ d=d_


function goLotka(u0=[1.0, 1.0], tspan=(0.0, 100.0), method=RK4)

  prob = ODEProblem(f, u0, tspan)
  sol = solve(prob, method(), dt=0.01)
  df=DataFrame(t=sol.t, x1=map(x->x[1],sol.u), y = map(x->x[2], sol.u), experiment="exp1")
  DataFrames.writetable("lotka.csv", df)

  plot(df,  x="x1", y="y")
end
