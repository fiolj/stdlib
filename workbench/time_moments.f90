program time_moments
  USE timing_moments, only: moment, mmoment
  implicit none

  integer, parameter :: N = 200000000
  real(8) :: x(N)
  real(8) :: y1(N/5), y(N/5,5)
  real(8) :: z
  real(8) :: t1, t2
  integer :: dim, order

  order = 2
  call random_number(x)


  print '("One dimensional cases, length =", I12)', size(x)

  call cpu_time(t1)
  z = moment(x, order, 1)
  call cpu_time(t2)
  print '("Internal sum Time = ",f6.3," seconds.")', t2-t1

  call cpu_time(t1)
  z = mmoment(x, order, 1)
  call cpu_time(t2)
  print '("Call to mean Time = ",f6.3," seconds.")', t2-t1



  ! y = reshape(x,[N/5,5])


  ! print '("Two dimensional cases, shape =", I12,I2)', shape(y)
  ! dim = 1
  ! print '("Along the axis=",I1, " dimension=", I8)', dim, size(y,dim)

  ! call cpu_time(t1)
  ! y1 = moment_2_rdp_rdp(y, order, dim)
  ! call cpu_time(t2)
  ! ! print '("Internal sum =", g0)', y1(1:5)
  ! print '("Internal sum Time = ",f6.3," seconds.")', t2-t1
  ! ! print "(5(g0))", y1(5:10)
  ! call cpu_time(t1)
  ! y1 = moment_2m_rdp_rdp(y, order, dim)
  ! call cpu_time(t2)
  ! ! print '("Call to mean =", g0)', y1(1:5)
  ! print '("Call to mean Time = ",f6.3," seconds.")', t2-t1

  ! dim = 2
  ! print '("Along the axis=",I1, " dimension=", I2)', dim, size(y,dim)

  ! call cpu_time(t1)
  ! y1 = moment_2_rdp_rdp(y, order, dim)
  ! call cpu_time(t2)
  ! ! print '("Internal sum =", g0)', y1(1:5)
  ! print '("Internal sum Time = ",f6.3," seconds.")', t2-t1
  ! ! print "(5(g0))", y1(5:10)
  ! call cpu_time(t1)
  ! y1 = moment_2m_rdp_rdp(y, order, dim)
  ! call cpu_time(t2)
  ! ! print '("Call to mean =", g0)', y1(1:5)
  ! print '("Call to mean Time = ",f6.3," seconds.")', t2-t1


end program time_moments



