# Descriptive statistics


## Implemented
<!-- vim-markdown-toc GFM -->

* [`mean` - mean of array elements](#mean---mean-of-array-elements)
	* [Description](#description)
	* [Syntax](#syntax)
	* [Arguments](#arguments)
	* [Return value](#return-value)
	* [Example](#example)
* [`moment` - central moment of array elements](#moment---central-moment-of-array-elements)
	* [Description](#description-1)
	* [Syntax](#syntax-1)
	* [Arguments](#arguments-1)
	* [Return value](#return-value-1)
	* [Example](#example-1)
* [`var` - variance of array elements](#var---variance-of-array-elements)
	* [Description](#description-2)
	* [Syntax](#syntax-2)
	* [Arguments](#arguments-2)
	* [Return value](#return-value-2)
	* [Example](#example-2)

<!-- vim-markdown-toc -->

## `mean` - mean of array elements

### Description

Returns the mean of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.

### Syntax

`result = mean(array [, mask])`

`result = mean(array, dim [, mask])`

### Arguments

`array`: Shall be an array of type `integer`, `real`, or `complex`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to n, where n is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either by a scalar or an array of the same shape as `array`.

### Return value

If `array` is of type `real` or `complex`, the result is of the same type as `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `dim` is absent, a scalar with the mean of all elements in `array` is returned. Otherwise, an array of rank n-1, where n equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

If `mask` is specified, the result is the mean of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

### Example

```fortran
program demo_mean
    use stdlib_experimental_stats, only: mean
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    print *, mean(x)                            !returns 3.5
    print *, mean( reshape(x, [ 2, 3 ] ))       !returns 3.5
    print *, mean( reshape(x, [ 2, 3 ] ), 1)    !returns [ 1.5, 3.5, 5.5 ]
    print *, mean( reshape(x, [ 2, 3 ] ), 1,&
                   reshape(x, [ 2, 3 ] ) > 3.)  !returns [ NaN, 4.0, 5.5 ]
end program demo_mean
```

## `moment` - central moment of array elements

### Description

Returns the _k_-th order central moment of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.

The _k_-th order central moment is defined as :

```
 moment(array) = 1/n sum_i (array(i) - mean(array))^k
```

where n is the number of elements.

### Syntax

`result = moment(array, order [, mask])`

`result = moment(array, order, dim [, mask])`

### Arguments

`array`: Shall be an array of type `integer`, `real`, or `complex`.

`order`: Shall be an scalar of type `integer`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to n, where n is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either by a scalar or an array of the same shape as `array`.

### Return value

If `array` is of type `real` or `complex`, the result is of the same type as `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `dim` is absent, a scalar with the _k_-th central moment of all elements in `array` is returned. Otherwise, an array of rank n-1, where n equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

If `mask` is specified, the result is the _k_-th central moment of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

### Example

```fortran
program demo_moment
    use stdlib_experimental_stats, only: moment
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    print *, moment(x, 2)                            !returns 2.9167
    print *, moment( reshape(x, [ 2, 3 ] ), 2)       !returns 2.9167
    print *, moment( reshape(x, [ 2, 3 ] ), 2, 1)    !returns [0.25, 0.25, 0.25]
    print *, moment( reshape(x, [ 2, 3 ] ), 2, 1,&
                     reshape(x, [ 2, 3 ] ) > 3.)     !returns [NaN, 0., 0.25]
end program demo_moment
```

## `var` - variance of array elements

### Description

Returns the variance of all the elements of `array`, or of the elements of `array` along dimension `dim` if provided, and if the corresponding element in `mask` is `true`.

Per default, the variance is defined as the best unbiased estimator and is computed as:

```
 var(array) = 1/(n-1) sum_i (array(i) - mean(array))^2
```

where n is the number of elements.

The use of the term `n-1` for scaling is called Bessel 's correction. The scaling can be changed with the logical argument `corrected`. If `corrected` is `.false.`, then the sum is scaled with `n`, otherwise with `n-1`.


### Syntax

`result = var(array [, mask [, corrected]])`

`result = var(array, dim [, mask [, corrected]])`

### Arguments

`array`: Shall be an array of type `integer`, `real`, or `complex`.

`dim`: Shall be a scalar of type `integer` with a value in the range from 1 to n, where n is the rank of `array`.

`mask` (optional): Shall be of type `logical` and either by a scalar or an array of the same shape as `array`.

`corrected` (optional): Shall be a scalar of type `logical`. If `corrected` is `.true.` (default value), the sum is scaled with `n-1`. If `corrected` is `.false.`, then the sum is scaled with `n`.

### Return value

If `array` is of type `real` or `complex`, the result is of type `real` corresponding to the type of `array`.
If `array` is of type `integer`, the result is of type `real(dp)`.

If `dim` is absent, a scalar with the variance of all elements in `array` is returned. Otherwise, an array of rank n-1, where n equals the rank of `array`, and a shape similar to that of `array` with dimension `dim` dropped is returned.

If `mask` is specified, the result is the variance of all elements of `array` corresponding to `true` elements of `mask`. If every element of `mask` is `false`, the result is IEEE `NaN`.

If the variance is computed with only one single element, then the result is IEEE `NaN` if `corrected` is `.true.` and is `0.` if `corrected` is `.false.`.

### Example

```fortran
program demo_var
    use stdlib_experimental_stats, only: var
    implicit none
    real :: x(1:6) = [ 1., 2., 3., 4., 5., 6. ]
    print *, var(x)                            !returns 3.5
    print *, var(x, corrected = .false.)       !returns 2.9167
    print *, var( reshape(x, [ 2, 3 ] ))       !returns 3.5
    print *, var( reshape(x, [ 2, 3 ] ), 1)    !returns [0.5, 0.5, 0.5]
    print *, var( reshape(x, [ 2, 3 ] ), 1,&
                  reshape(x, [ 2, 3 ] ) > 3.)  !returns [NaN, NaN, 0.5]
    print *, var( reshape(x, [ 2, 3 ] ), 1,&
                  reshape(x, [ 2, 3 ] ) > 3.,&
                  corrected=.false.)           !returns [NaN, 0., 0.25]
end program demo_var
```

