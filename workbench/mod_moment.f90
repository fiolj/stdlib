module timing_moments
  use iso_fortran_env, only: dp=>real64
  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan

  interface mean
    module procedure :: mean_1_rdp_rdp
    module procedure :: mean_2_rdp_rdp
    module procedure :: mean_5_rdp_rdp
  end interface mean
  interface moment
    module procedure :: moment_1_rdp_rdp
    module procedure :: moment_2_rdp_rdp
    module procedure :: moment_5_rdp_rdp
  end interface moment
  interface mmoment
    module procedure :: moment_1m_rdp_rdp
    module procedure :: moment_2m_rdp_rdp
    module procedure :: moment_5m_rdp_rdp
  end interface mmoment

  public mean, moment, mmoment

contains
  pure elemental function optval(x, default) result(y)
    logical, intent(in), optional :: x
    logical, intent(in) :: default
    logical :: y

    if (present(x)) then
      y = x
    else
      y = default
    end if
  end function optval

  function mean_1_rdp_rdp(x, dim, mask) result(res)
    real(dp), intent(in) :: x(:)
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res

    if (.not.optval(mask, .true.)) then
      res = ieee_value(real(res, kind=dp), ieee_quiet_nan)
      return
    end if

    if (dim >= 1 .and. dim <= 1) then
      res = sum(x, dim) / real(size(x, dim), dp)
    else
      stop "ERROR (mean): wrong dimension"
    end if

  end function mean_1_rdp_rdp
  function mean_2_rdp_rdp(x, dim, mask) result(res)
    real(dp), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

    if (.not.optval(mask, .true.)) then
      res = ieee_value(real(res, kind=dp), ieee_quiet_nan)
      return
    end if

    if (dim >= 1 .and. dim <= 2) then
      res = sum(x, dim) / real(size(x, dim), dp)
    else
      stop "ERROR (mean): wrong dimension"
    end if

  end function mean_2_rdp_rdp

  function mean_5_rdp_rdp(x, dim, mask) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
    & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

    if (.not.optval(mask, .true.)) then
      res = ieee_value(real(res, kind=dp), ieee_quiet_nan)
      return
    end if

    if (dim >= 1 .and. dim <= 5) then
      res = sum(x, dim) / real(size(x, dim), dp)
    else
      stop "ERROR (mean): wrong dimension"
    end if

  end function mean_5_rdp_rdp

  function moment_1m_rdp_rdp(x, order, dim, mask) result(res)
    real(dp), intent(in) :: x(:)
    integer, intent(in) :: order
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res

    integer :: i
    real(dp) :: n
    real(dp) :: mMean

    if (.not.optval(mask, .true.)) then
      res = ieee_value(1._dp, ieee_quiet_nan)
      return
    end if

    n = size(x, dim)
    mMean = mean(x, dim)

    res = 0
    select case(dim)
     case(1)
      do i = 1, size(x, dim)
        res = res + (x(i) - mMean)**order
      end do
     case default
      stop "ERROR (moment): wrong dimension"
    end select
    res = res / n

  end function moment_1m_rdp_rdp
  function moment_2m_rdp_rdp(x, order, dim, mask) result(res)
    real(dp), intent(in) :: x(:,:)
    integer, intent(in) :: order
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

    integer :: i
    real(dp) :: n
    real(dp) :: mMean(merge(size(x, 1), size(x, 2), mask=1<dim))

    if (.not.optval(mask, .true.)) then
      res = ieee_value(1._dp, ieee_quiet_nan)
      return
    end if

    n = size(x, dim)
    mMean = mean(x, dim)

    res = 0
    select case(dim)
     case(1)
      do i = 1, size(x, dim)
        res = res + (x(i, :) - mMean)**order
      end do
     case(2)
      do i = 1, size(x, dim)
        res = res + (x(:, i) - mMean)**order
      end do
     case default
      stop "ERROR (moment): wrong dimension"
    end select
    res = res / n
  end function moment_2m_rdp_rdp

  function moment_5m_rdp_rdp(x, order, dim, mask) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: order
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
    & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

    integer :: i
    real(dp) :: n
    real(dp) :: mMean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
    & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

    if (.not.optval(mask, .true.)) then
      res = ieee_value(1._dp, ieee_quiet_nan)
      return
    end if

    n = size(x, dim)
    mMean = mean(x, dim)

    res = 0
    select case(dim)
     case(1)
      do i = 1, size(x, dim)
        res = res + (x(i, :, :, :, :) - mMean)**order
      end do
     case(2)
      do i = 1, size(x, dim)
        res = res + (x(:, i, :, :, :) - mMean)**order
      end do
     case(3)
      do i = 1, size(x, dim)
        res = res + (x(:, :, i, :, :) - mMean)**order
      end do
     case(4)
      do i = 1, size(x, dim)
        res = res + (x(:, :, :, i, :) - mMean)**order
      end do
     case(5)
      do i = 1, size(x, dim)
        res = res + (x(:, :, :, :, i) - mMean)**order
      end do
     case default
      stop "ERROR (moment): wrong dimension"
    end select
    res = res / n

  end function moment_5m_rdp_rdp

  ! Original functions


  function moment_1_rdp_rdp(x, order, dim, mask) result(res)
    real(dp), intent(in) :: x(:)
    integer, intent(in) :: order
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res

    integer :: i
    real(dp) :: n
    real(dp) :: mean

    if (.not.optval(mask, .true.)) then
      res = ieee_value(1._dp, ieee_quiet_nan)
      return
    end if

    n = real(size(x, dim), dp)
    mean = sum(x, dim) / n

    res = 0
    select case(dim)
     case(1)
      do i = 1, size(x, dim)
        res = res + (x(i) - mean)**order
      end do
     case default
      stop "ERROR (moment): wrong dimension"
    end select
    res = res / n

  end function moment_1_rdp_rdp

  function moment_2_rdp_rdp(x, order, dim, mask) result(res)
    real(dp), intent(in) :: x(:,:)
    integer, intent(in) :: order
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

    integer :: i
    real(dp) :: n
    real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

    if (.not.optval(mask, .true.)) then
      res = ieee_value(1._dp, ieee_quiet_nan)
      return
    end if

    n = size(x, dim)
    mean = sum(x, dim) / n

    res = 0
    select case(dim)
     case(1)
      do i = 1, size(x, dim)
        res = res + (x(i, :) - mean)**order
      end do
     case(2)
      do i = 1, size(x, dim)
        res = res + (x(:, i) - mean)**order
      end do
     case default
      stop "ERROR (moment): wrong dimension"
    end select
    res = res / n

  end function moment_2_rdp_rdp

  function moment_5_rdp_rdp(x, order, dim, mask) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: order
    integer, intent(in) :: dim
    logical, intent(in), optional :: mask
    real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
    & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

    integer :: i
    real(dp) :: n
    real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
    & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

    if (.not.optval(mask, .true.)) then
      res = ieee_value(1._dp, ieee_quiet_nan)
      return
    end if

    n = size(x, dim)
    mean = sum(x, dim) / n

    res = 0
    select case(dim)
     case(1)
      do i = 1, size(x, dim)
        res = res + (x(i, :, :, :, :) - mean)**order
      end do
     case(2)
      do i = 1, size(x, dim)
        res = res + (x(:, i, :, :, :) - mean)**order
      end do
     case(3)
      do i = 1, size(x, dim)
        res = res + (x(:, :, i, :, :) - mean)**order
      end do
     case(4)
      do i = 1, size(x, dim)
        res = res + (x(:, :, :, i, :) - mean)**order
      end do
     case(5)
      do i = 1, size(x, dim)
        res = res + (x(:, :, :, :, i) - mean)**order
      end do
     case default
      stop "ERROR (moment): wrong dimension"
    end select
    res = res / n

  end function moment_5_rdp_rdp

end module timing_moments
