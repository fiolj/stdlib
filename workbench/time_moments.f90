program time_moments
  USE timing_moments, only: moment, mmoment
  implicit none

  integer, parameter :: N = 100*1000*1000
  integer, parameter :: Nloop=3

  real(8) :: x(N), x1
  real(8) :: y(N/5,5), z(N/1000,5,5,2,2)
  real(8) :: y1(N/5), z1(N/1000,5,5,2)
  real(8) :: t1, t2
  real(8), dimension(Nloop) :: ts, tm
  integer :: dim, order, Nmax
  integer :: i

  order = 2
  call random_number(x)


  print '("One dimensional cases, length =", I12)', size(x)
  print '("#  Size    tsum  tmean")'
  Nmax = N/100
  do i=1, Nloop
    call cpu_time(t1)
    x1 = moment(x(:Nmax), order, 1)
    call cpu_time(t2)
    ts(i) = t2-t1

    call cpu_time(t1)
    x1 = mmoment(x(:Nmax), order, 1)
    call cpu_time(t2)
    tm(i)=t2-t1
    print "(I10, f6.3, f6.3)", Nmax, ts(i), tm(i)
    Nmax = Nmax*10
  end do

  print *,''

  print '("Two dimensional cases, shape =", I12,I2)', shape(y)

  y = reshape(x,[N/5,5])
  dim = 1

  print '("Along the axis=",I1, " mean on ",I1, " arrays of dimension =", I8)', dim, size(y,2), size(y,dim)

  call cpu_time(t1)
  y1 = moment(y, order, dim)
  call cpu_time(t2)
  print '("Internal sum Time = ",f6.3," seconds.")', t2-t1
  call cpu_time(t1)
  y1 = mmoment(y, order, dim)
  call cpu_time(t2)
  print '("Call to mean Time = ",f6.3," seconds.")', t2-t1

  dim = 2
  print '("Along the axis=",I1, " mean on ",I8, " arrays of dimension =", I1)', dim, size(y,1), size(y,dim)
  call cpu_time(t1)
  y1 = moment(y, order, dim)
  call cpu_time(t2)

  print '("Internal sum Time = ",f6.3," seconds.")', t2-t1
  call cpu_time(t1)
  y1 = mmoment(y, order, dim)
  call cpu_time(t2)
  print '("Call to mean Time = ",f6.3," seconds.")', t2-t1

  print *,''

  print *,"Five dimensional cases, shape =", shape(y)
  ! z = reshape(x, [N/1000,10,10,5,2])
  call random_number(z)

  do dim = 1, 5

    print '("Along the axis = ",I1)', dim

    call cpu_time(t1)
    z1 = moment(z, order, dim)
    call cpu_time(t2)
    print '("Internal sum Time = ",f6.3," seconds.")', t2-t1
    call cpu_time(t1)
    z1 = mmoment(z, order, dim)
    call cpu_time(t2)
    print '("Call to mean Time = ",f6.3," seconds.")', t2-t1
  end do


end program time_moments
