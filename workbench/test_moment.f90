program time_moments
  use stdlib_experimental_error, only: assert
  use stdlib_experimental_kinds, only: sp, dp, int32, int64
  use stdlib_experimental_stats, only: moment, mmoment
  implicit none

  integer, parameter :: N = 100000000
  real(8) :: x(N)
  real(8) :: y1(N/5), y(N/5,5)
  real(8) :: z
  real(8) :: t1, t2
  integer :: dim, order

  order = 2
  call random_number(x)


  print '("One dimensional cases, length =", I12)', size(x)

  call cpu_time(t1)
  z = moment_1_rdp_rdp(x, order, 1)
  call cpu_time(t2)
  ! print '("Internal sum =", g0)', z
  print '("Internal sum Time = ",f6.3," seconds.")', t2-t1

  call cpu_time(t1)
  z = moment_1m_rdp_rdp(x, order, 1)
  call cpu_time(t2)
  ! print '("Call to mean =", g0)', z
  print '("Call to mean Time = ",f6.3," seconds.")', t2-t1



  y = reshape(x,[N/5,5])


  print '("Two dimensional cases, shape =", I12,I2)', shape(y)
  dim = 1
  print '("Along the axis=",I1, " dimension=", I8)', dim, size(y,dim)

  call cpu_time(t1)
  y1 = moment_2_rdp_rdp(y, order, dim)
  call cpu_time(t2)
  ! print '("Internal sum =", g0)', y1(1:5)
  print '("Internal sum Time = ",f6.3," seconds.")', t2-t1
  ! print "(5(g0))", y1(5:10)
  call cpu_time(t1)
  y1 = moment_2m_rdp_rdp(y, order, dim)
  call cpu_time(t2)
  ! print '("Call to mean =", g0)', y1(1:5)
  print '("Call to mean Time = ",f6.3," seconds.")', t2-t1

  dim = 2
  print '("Along the axis=",I1, " dimension=", I2)', dim, size(y,dim)

  call cpu_time(t1)
  y1 = moment_2_rdp_rdp(y, order, dim)
  call cpu_time(t2)
  ! print '("Internal sum =", g0)', y1(1:5)
  print '("Internal sum Time = ",f6.3," seconds.")', t2-t1
  ! print "(5(g0))", y1(5:10)
  call cpu_time(t1)
  y1 = moment_2m_rdp_rdp(y, order, dim)
  call cpu_time(t2)
  ! print '("Call to mean =", g0)', y1(1:5)
  print '("Call to mean Time = ",f6.3," seconds.")', t2-t1


end program time_moments



! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! The results obtained in this very limited case were:
!
! > gfortran mod_moment.f90 test_moment.f90 -o test_moment && ./test_moment
! One dimensional cases, length =   100000000
! Internal sum Time =  0.525 seconds.
! Call to mean Time =  0.470 seconds.
! Two dimensional cases, shape =    20000000 5
! Along the axis=1 dimension=20000000
! Internal sum Time =  0.571 seconds.
! Call to mean Time =  0.614 seconds.
! Along the axis=2 dimension= 5
! Internal sum Time =  0.775 seconds.
! Call to mean Time =  0.758 seconds.

! > gfortran -O0 mod_moment.f90 test_moment.f90 -o test_moment0 && ./test_moment0
! One dimensional cases, length =   100000000
! Internal sum Time =  0.526 seconds.
! Call to mean Time =  0.468 seconds.
! Two dimensional cases, shape =    20000000 5
! Along the axis=1 dimension=20000000
! Internal sum Time =  0.571 seconds.
! Call to mean Time =  0.617 seconds.
! Along the axis=2 dimension= 5
! Internal sum Time =  0.773 seconds.
! Call to mean Time =  0.756 seconds.

! > gfortran -O1 mod_moment.f90 test_moment.f90 -o test_moment1 && ./test_moment1
! One dimensional cases, length =   100000000
! Internal sum Time =  0.409 seconds.
! Call to mean Time =  0.410 seconds.
! Two dimensional cases, shape =    20000000 5
! Along the axis=1 dimension=20000000
! Internal sum Time =  0.431 seconds.
! Call to mean Time =  0.439 seconds.
! Along the axis=2 dimension= 5
! Internal sum Time =  0.564 seconds.
! Call to mean Time =  0.505 seconds.

! > gfortran -O2 mod_moment.f90 test_moment.f90 -o test_moment2 && ./test_moment2
! One dimensional cases, length =   100000000
! Internal sum Time =  0.410 seconds.
! Call to mean Time =  0.411 seconds.
! Two dimensional cases, shape =    20000000 5
! Along the axis=1 dimension=20000000
! Internal sum Time =  0.433 seconds.
! Call to mean Time =  0.451 seconds.
! Along the axis=2 dimension= 5
! Internal sum Time =  0.573 seconds.
! Call to mean Time =  0.522 seconds.

! > gfortran -O3 mod_moment.f90 test_moment.f90 -o test_moment3 && ./test_moment3
! One dimensional cases, length =   100000000
! Internal sum Time =  0.409 seconds.
! Call to mean Time =  0.412 seconds.
! Two dimensional cases, shape =    20000000 5
! Along the axis=1 dimension=20000000
! Internal sum Time =  0.430 seconds.
! Call to mean Time =  0.419 seconds.
! Along the axis=2 dimension= 5
! Internal sum Time =  0.535 seconds.
! Call to mean Time =  0.481 seconds.

! > gfortran -O3 mod_moment.f90 test_moment.f90 -o test_moment3 && ./test_moment3
! One dimensional cases, length =   100000000
! Internal sum Time =  0.408 seconds.
! Call to mean Time =  0.411 seconds.
! Two dimensional cases, shape =    20000000 5
! Along the axis=1 dimension=20000000
! Internal sum Time =  0.431 seconds.
! Call to mean Time =  0.417 seconds.
! Along the axis=2 dimension= 5
! Internal sum Time =  0.536 seconds.
! Call to mean Time =  0.479 seconds.
