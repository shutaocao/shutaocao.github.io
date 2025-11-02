module modhbrd
  ! this code is beautified and modified by shutao cao, august 2012 and may 2018. do not remove this files
  ! in a few places I replaced if to if else then
  ! see http://www.mcs.anl.gov/~more/ for user guide and netlib.org/minpack for f77 version
  ! original f90 code was from Alan Miller
  ! Miller corrections to function enorm - 28 november 2003
  implicit none
  !integer, parameter  :: dp = selected_real_kind(14, 60)
  integer,parameter :: dp = kind(1.0d0)
  private
  public :: hbrd, hbrd2, hybrd
contains
  subroutine hbrd(fcn, n, x, fvec, epsfcn, tol, info, diag)
    ! code converted using to_f90 by alan miller
    ! date: 2003-07-15  time: 13:27:42
    implicit none
    !integer,parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: n
    real (dp), intent(in out)  :: x(n)
    real (dp), intent(in out)  :: fvec(n)
    real (dp), intent(in)      :: epsfcn
    real (dp), intent(in)      :: tol
    integer, intent(out)       :: info
    real (dp), intent(out)     :: diag(n)
    ! external fcn
    interface
       subroutine fcn(n, x, fvec, iflag)
         use modparaL
         implicit none
         integer, intent(in) :: n
         real(dp),intent(in) :: x(n)
         real(dp), intent(out)   :: fvec(n)
         integer, intent(in out)  :: iflag
       end subroutine fcn
    end interface
    !     the purpose of hbrd is to find a zero of a system of n nonlinear
    !   functions in n variables by a modification of the powell hybrid method.
    !   this is done by using the more general nonlinear equation solver hybrd.
    !   the user must provide a subroutine which calculates the functions.
    !   the jacobian is then calculated by a forward-difference approximation.
    !   the subroutine statement is
    !     subroutine hbrd(n, x, fvec, epsfcn, tol, info, wa, lwa)
    !   where
    !     fcn is the name of the user-supplied subroutine which calculates
    !       the functions.  fcn must be declared in an external statement
    !       in the user calling program, and should be written as follows.
    !       subroutine fcn(n, x, fvec, iflag)
    !       integer n,iflag
    !       real x(n),fvec(n)
    !       ----------
    !       calculate the functions at x and return this vector in fvec.
    !       ---------
    !       return
    !       end
    !       the value of iflag not be changed by fcn unless
    !       the user wants to terminate the execution of hbrd.
    !       in this case set iflag to a negative integer.
    !     n is a positive integer input variable set to the number
    !       of functions and variables.
    !     x is an array of length n. on input x must contain an initial
    !       estimate of the solution vector.  on output x contains the
    !       final estimate of the solution vector.
    !     fvec is an output array of length n which contains
    !       the functions evaluated at the output x.
    !     epsfcn is an input variable used in determining a suitable step length
    !       for the forward-difference approximation.  this approximation assumes
    !       that the relative errors in the functions are of the order of epsfcn.
    !       if epsfcn is less than the machine precision, it is assumed that the
    !       relative errors in the functions are of the order of the machine
    !       precision.
    !     tol is a nonnegative input variable.  termination occurs when the
    !       algorithm estimates that the relative error between x and the solution
    !       is at most tol.
    !     info is an integer output variable.  if the user has terminated
    !       execution, info is set to the (negative) value of iflag.
    !       see description of fcn.  otherwise, info is set as follows.
    !       info = 0   improper input parameters.
    !       info = 1   algorithm estimates that the relative error
    !                  between x and the solution is at most tol.
    !       info = 2   number of calls to fcn has reached or exceeded 200*(n+1).
    !       info = 3   tol is too small. no further improvement in
    !                  the approximate solution x is possible.
    !       info = 4   iteration is not making good progress.
    !
    !   subprograms called
    !     user-supplied ...... fcn
    !     minpack-supplied ... hybrd
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more
    ! reference:
    ! powell, m.j.d. 'a hybrid method for nonlinear equations' in numerical methods
    !      for nonlinear algebraic equations', p.rabinowitz (editor), gordon and      breach, london 1970.
    !   **********
    integer    :: maxfev, ml, mode, mu, nfev, nprint
    real (dp)  :: xtol
    real (dp), parameter  :: factor = 100.0_dp, zero = 0.0_dp
    info = 0
    !     check the input parameters for errors.
    if (n <= 0 .or. epsfcn < zero .or. tol < zero) return
    !     call hybrd.
    !maxfev = 200*(n + 1)
    ! changed by shutao
    maxfev = 500*(n + 1)
    xtol = tol
    ml = n - 1
    mu = n - 1
    mode = 2 ! for nonlinear equations
    !mode = 1 ! for least square problems
    nprint = 0
    call hybrd(fcn, n, x, fvec, xtol, maxfev, ml, mu, epsfcn, diag, mode, factor, nprint, info, nfev)
    if (info == 5) info = 4
    return
    !     last card of subroutine hbrd.
  end subroutine hbrd
  
  subroutine hybrd(fcn, n, x, fvec, xtol, maxfev, ml, mu, epsfcn, diag, mode, factor, nprint, info, nfev)
    implicit none
    !integer,parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: n
    real (dp), intent(in out)  :: x(n), fvec(n)
    real (dp), intent(in)      :: xtol
    integer, intent(in out)    :: maxfev,ml
    integer, intent(in)        :: mu
    real (dp), intent(in)      :: epsfcn
    real (dp), intent(out)     :: diag(n)
    integer, intent(in)        :: mode
    real (dp), intent(in)      :: factor
    integer, intent(in out)    :: nprint
    integer, intent(out)       :: info, nfev
    ! external fcn
    interface
       subroutine fcn(n, x, fvec, iflag)
         use modparaL
         implicit none
         !integer, parameter  :: dp = selected_real_kind(14, 60)
         integer, intent(in)      :: n
         real (dp), intent(in)    :: x(n)
         real (dp), intent(out)   :: fvec(n)
         integer, intent(in out)  :: iflag
       end subroutine fcn
    end interface
    !   **********
    !   subroutine hybrd
    !   the purpose of hybrd is to find a zero of a system of n nonlinear
    !   functions in n variables by a modification of the powell hybrid method.
    !   the user must provide a subroutine which calculates the functions.
    !   the jacobian is then calculated by a forward-difference approximation.
    !   the subroutine statement is
    !     subroutine hybrd(fcn, n, x, fvec, xtol, maxfev, ml, mu, epsfcn,
    !                      diag, mode, factor, nprint, info, nfev, fjac,
    !                      ldfjac, r, lr, qtf, wa1, wa2, wa3, wa4)
    !   where
    !     fcn is the name of the user-supplied subroutine which calculates
    !       the functions.  fcn must be declared in an external statement in
    !       the user calling program, and should be written as follows.
    !       subroutine fcn(n, x, fvec, iflag)
    !       integer n, iflag
    !       real x(n), fvec(n)
    !       ----------
    !       calculate the functions at x and
    !       return this vector in fvec.
    !       ---------
    !       return
    !       end
    !       the value of iflag should not be changed by fcn unless
    !       the user wants to terminate execution of hybrd.
    !       in this case set iflag to a negative integer.
    !     n is a positive integer input variable set to the number
    !       of functions and variables.
    !     x is an array of length n.  on input x must contain an initial
    !       estimate of the solution vector.  on output x contains the final
    !       estimate of the solution vector.
    !     fvec is an output array of length n which contains
    !       the functions evaluated at the output x.
    !     xtol is a nonnegative input variable.  termination occurs when the
    !       relative error between two consecutive iterates is at most xtol.
    !     maxfev is a positive integer input variable.  termination occurs when
    !       the number of calls to fcn is at least maxfev by the end of an
    !       iteration.
    !     ml is a nonnegative integer input variable which specifies the
    !       number of subdiagonals within the band of the jacobian matrix.
    !       if the jacobian is not banded, set ml to at least n - 1.
    !     mu is a nonnegative integer input variable which specifies the number
    !       of superdiagonals within the band of the jacobian matrix.
    !       if the jacobian is not banded, set mu to at least n - 1.
    !     epsfcn is an input variable used in determining a suitable step length
    !       for the forward-difference approximation.  this approximation
    !       assumes that the relative errors in the functions are of the order
    !       of epsfcn. if epsfcn is less than the machine precision,
    !       it is assumed that the relative errors in the functions are of the
    !       order of the machine precision.
    !     diag is an array of length n. if mode = 1 (see below),
    !       diag is internally set.  if mode = 2, diag must contain positive
    !       entries that serve as multiplicative scale factors for the
    !       variables.
    !     mode is an integer input variable. if mode = 1, the variables will be
    !       scaled internally.  if mode = 2, the scaling is specified by the
    !       input diag.  other values of mode are equivalent to mode = 1.
    !     factor is a positive input variable used in determining the
    !       initial step bound. this bound is set to the product of
    !       factor and the euclidean norm of diag*x if nonzero, or else
    !       to factor itself. in most cases factor should lie in the
    !       interval (.1,100.). 100. is a generally recommended value.
    !     nprint is an integer input variable that enables controlled
    !       printing of iterates if it is positive. in this case,
    !       fcn is called with iflag = 0 at the beginning of the first
    !       iteration and every nprint iterations thereafter and
    !       immediately prior to return, with x and fvec available
    !       for printing. if nprint is not positive, no special calls
    !       of fcn with iflag = 0 are made.
    !     info is an integer output variable. if the user has
    !       terminated execution, info is set to the (negative)
    !       value of iflag. see description of fcn. otherwise,
    !       info is set as follows.
    !       info = 0   improper input parameters.
    !       info = 1   relative error between two consecutive iterates
    !                  is at most xtol.
    !       info = 2   number of calls to fcn has reached or exceeded maxfev.
    !       info = 3   xtol is too small. no further improvement in
    !                  the approximate solution x is possible.
    !       info = 4   iteration is not making good progress, as
    !                  measured by the improvement from the last
    !                  five jacobian evaluations.
    !       info = 5   iteration is not making good progress, as measured by
    !                  the improvement from the last ten iterations.
    !     nfev is an integer output variable set to the number of calls to fcn.
    !     fjac is an output n by n array which contains the orthogonal matrix q
    !       produced by the qr factorization of the final approximate jacobian.
    !     ldfjac is a positive integer input variable not less than n
    !       which specifies the leading dimension of the array fjac.
    !     r is an output array of length lr which contains the
    !       upper triangular matrix produced by the qr factorization
    !       of the final approximate jacobian, stored rowwise.
    !     lr is a positive integer input variable not less than (n*(n+1))/2.
    !     qtf is an output array of length n which contains
    !       the vector (q transpose)*fvec.
    !     wa1, wa2, wa3, and wa4 are work arrays of length n.
    !   subprograms called
    !     user-supplied ...... fcn
    !     minpack-supplied ... dogleg,spmpar,enorm,fdjac1,
    !                          qform,qrfac,r1mpyq,r1updt
    !     fortran-supplied ... abs,max,min,min,mod
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more
    !   **********
    integer    :: i, iflag, iter, j, jm1, l, lr, msum, ncfail, ncsuc, nslow1, nslow2
    integer    :: iwa(1)
    logical    :: jeval, sing
    real (dp)  :: actred, delta, epsmch, fnorm, fnorm1, pnorm, prered, ratio, sum, temp, xnorm
    real (dp), parameter  :: one = 1.0_dp, p1 = 0.1_dp, p5 = 0.5_dp, p001 = 0.001_dp, p0001 = 0.0001_dp, zero = 0.0_dp
    ! the following were workspace arguments
    real (dp)  :: fjac(n,n), r(n*(n+1)/2), qtf(n), wa1(n), wa2(n), wa3(n), wa4(n)
    !     epsmch is the machine precision.
    epsmch = epsilon(1.0_dp)
    info = 0
    iflag = 0
    nfev = 0
    lr = n*(n+1)/2
    !     check the input parameters for errors.
    if (n > 0 .and. xtol >= zero .and. maxfev > 0 .and. ml >= 0 .and. mu >=0 .and. factor > zero ) then
       if (mode == 2) then
          diag(1:n) = one
       end if
       !     evaluate the function at the starting point and calculate its norm.
       iflag = 1
       call fcn(n, x, fvec, iflag)
       nfev = 1
       if (iflag >= 0) then
          fnorm = enormfun(n, fvec)
          !   determine the number of calls to fcn needed to compute the jacobian matrix.
          msum = min(ml+mu+1,n)
          !     initialize iteration counter and monitors.
          iter = 1
          ncsuc = 0
          ncfail = 0
          nslow1 = 0
          nslow2 = 0
          !     beginning of the outer loop.
20        jeval = .true.
          !        calculate the jacobian matrix.
          iflag = 2
          call fdjac1(fcn, n, x, fvec, fjac, n, iflag, ml, mu, epsfcn, wa1, wa2)
          nfev = nfev + msum
          if (iflag >= 0) then
             !        compute the qr factorization of the jacobian.
             call qrfac(n, n, fjac, n, .false., iwa, 1, wa1, wa2, wa3)
             !        on the first iteration and if mode is 1, scale according
             !        to the norms of the columns of the initial jacobian.
             if (iter == 1) then
                if (mode /= 2) then
                   do  j = 1, n
                      diag(j) = wa2(j)
                      if (wa2(j) == zero) diag(j) = one
                   end do
                end if
                !        on the first iteration, calculate the norm of the scaled x
                !        and initialize the step bound delta.
                wa3(1:n) = diag(1:n) * x(1:n)
                xnorm = enormfun(n, wa3)
                delta = factor * xnorm
                if (delta == zero) delta = factor
             end if
             !        form (q transpose)*fvec and store in qtf.
             qtf(1:n) = fvec(1:n)
             do  j = 1, n
                if (fjac(j,j) /= zero) then
                   sum = zero
                   do  i = j, n
                      sum = sum + fjac(i,j) * qtf(i)
                   end do
                   temp = -sum / fjac(j,j)
                   do  i = j, n
                      qtf(i) = qtf(i) + fjac(i,j) * temp
                   end do
                end if
             end do
             ! copy the triangular factor of the qr factorization into r.
             sing = .false.
             do  j = 1, n
                l = j
                jm1 = j - 1
                if (jm1 >= 1) then
                   do  i = 1, jm1
                      r(l) = fjac(i,j)
                      l = l + n - i
                   end do
                end if
                r(l) = wa1(j)
                if (wa1(j) == zero) sing = .true.
             end do
             !        accumulate the orthogonal factor in fjac.
             call qform(n, n, fjac, n, wa1)
             !        rescale if necessary.
             if (mode /= 2) then
                do  j = 1, n
                   diag(j) = max(diag(j), wa2(j))
                end do
             end if
             !        beginning of the inner loop.
             !           if requested, call fcn to enable printing of iterates.
120          if (nprint > 0) then
                iflag = 0
                if (mod(iter-1, nprint) == 0) call fcn(n, x, fvec, iflag)
                if (iflag < 0) go to 190
             end if
             !           determine the direction p.
             call dogleg(n, r, lr, diag, qtf, delta, wa1, wa2, wa3)
             !           store the direction p and x + p. calculate the norm of p.
             do j = 1, n
                wa1(j) = -wa1(j)
                wa2(j) = x(j) + wa1(j)
                wa3(j) = diag(j) * wa1(j)
             end do
             pnorm = enormfun(n, wa3)
             !           on the first iteration, adjust the initial step bound.
             if (iter == 1) delta = min(delta, pnorm)
             !           evaluate the function at x + p and calculate its norm.
             iflag = 1
             call fcn(n, wa2, wa4, iflag)
             nfev = nfev + 1
             if (iflag >= 0) then
                fnorm1 = enormfun(n, wa4)
                !           compute the scaled actual reduction.
                actred = -one
                if (fnorm1 < fnorm) actred = one - (fnorm1/fnorm) ** 2
                !           compute the scaled predicted reduction.
                l = 1
                do  i = 1, n
                   sum = zero
                   do  j = i, n
                      sum = sum + r(l) * wa1(j)
                      l = l + 1
                   end do
                   wa3(i) = qtf(i) + sum
                end do
                temp = enormfun(n, wa3)
                prered = zero
                if (temp < fnorm) prered = one - (temp/fnorm) ** 2
                !           compute the ratio of the actual to the predicted reduction.
                ratio = zero
                if (prered > zero) ratio = actred / prered
                !           update the step bound.
                if (ratio < p1) then
                   ncsuc = 0
                   ncfail = ncfail + 1
                   delta = p5 * delta
                else
                   ncfail = 0
                   ncsuc = ncsuc + 1
                   if (ratio >= p5 .or. ncsuc > 1) delta = max(delta,pnorm/p5)
                   if (abs(ratio-one) <= p1) delta = pnorm / p5
                end if
                !           test for successful iteration.
                if (ratio >= p0001) then
                   !           successful iteration. update x, fvec, and their norms.
                   do  j = 1, n
                      x(j) = wa2(j)
                      wa2(j) = diag(j) * x(j)
                      fvec(j) = wa4(j)
                   end do
                   xnorm = enormfun(n, wa2)
                   fnorm = fnorm1
                   iter = iter + 1
                end if
                !           determine the progress of the iteration.
                nslow1 = nslow1 + 1
                if (actred >= p001) nslow1 = 0
                if (jeval) nslow2 = nslow2 + 1
                if (actred >= p1) nslow2 = 0
                !           test for convergence.
                if (delta <= xtol*xnorm .or. fnorm == zero) info = 1
                if (info == 0) then
                   !           tests for termination and stringent tolerances.
                   if (nfev >= maxfev) info = 2
                   if (p1*max(p1*delta, pnorm) <= epsmch*xnorm) info = 3
                   if (nslow2 == 5) info = 4
                   if (nslow1 == 10) info = 5
                   if (info == 0) then
                      !           criterion for recalculating jacobian approximation
                      !           by forward differences.
                      if (ncfail /= 2) then
                         !           calculate the rank one modification to the jacobian
                         !           and update qtf if necessary.
                         do  j = 1, n
                            sum = zero
                            do  i = 1, n
                               sum = sum + fjac(i,j) * wa4(i)
                            end do
                            wa2(j) = (sum-wa3(j)) / pnorm
                            wa1(j) = diag(j) * ((diag(j)*wa1(j))/pnorm)
                            if (ratio >= p0001) qtf(j) = sum
                         end do
                         !           compute the qr factorization of the updated jacobian.
                         call r1updt(n, n, r, lr, wa1, wa2, wa3, sing)
                         call r1mpyq(n, n, fjac, n, wa2, wa3)
                         call r1mpyq(1, n, qtf, 1, wa2, wa3)
                         !           end of the inner loop.
                         jeval = .false.
                         go to 120
                      end if
                      !        end of the outer loop.
                      go to 20
                   end if
                end if
             end if
          end if
       end if
    end if
    !     termination, either normal or user imposed.
190 if (iflag < 0) info = iflag
    iflag = 0
    if (nprint > 0) call fcn(n, x, fvec, iflag)
    return
    !     last card of subroutine hybrd.
  end subroutine hybrd
  
  subroutine dogleg(n, r, lr, diag, qtb, delta, x, wa1, wa2)
    implicit none
    !integer, parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: n
    integer, intent(in)        :: lr
    real (dp), intent(in)      :: r(lr)
    real (dp), intent(in)      :: diag(n)
    real (dp), intent(in)      :: qtb(n)
    real (dp), intent(in)      :: delta
    real (dp), intent(in out)  :: x(n)
    real (dp), intent(out)     :: wa1(n)
    real (dp), intent(out)     :: wa2(n)
    !     **********
    !     subroutine dogleg
    !     given an m by n matrix a, an n by n nonsingular diagonal
    !     matrix d, an m-vector b, and a positive number delta, the
    !     problem is to determine the convex combination x of the
    !     gauss-newton and scaled gradient directions that minimizes
    !     (a*x - b) in the least squares sense, subject to the
    !     restriction that the euclidean norm of d*x be at most delta.
    !     this subroutine completes the solution of the problem
    !     if it is provided with the necessary information from the
    !     qr factorization of a. that is, if a = q*r, where q has
    !     orthogonal columns and r is an upper triangular matrix,
    !     then dogleg expects the full upper triangle of r and
    !     the first n components of (q transpose)*b.
    !     the subroutine statement is
    !       subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)
    !     where
    !       n is a positive integer input variable set to the order of r.
    !       r is an input array of length lr which must contain the upper
    !         triangular matrix r stored by rows.
    !       lr is a positive integer input variable not less than
    !         (n*(n+1))/2.
    !       diag is an input array of length n which must contain the
    !         diagonal elements of the matrix d.
    !       qtb is an input array of length n which must contain the first
    !         n elements of the vector (q transpose)*b.
    !       delta is a positive input variable which specifies an upper
    !         bound on the euclidean norm of d*x.
    !       x is an output array of length n which contains the desired
    !         convex combination of the gauss-newton direction and the
    !         scaled gradient direction.
    !       wa1 and wa2 are work arrays of length n.
    !     subprograms called
    !       minpack-supplied ... spmpar,enorm
    !       fortran-supplied ... abs,max,min,sqrt
    !     argonne national laboratory. minpack project. march 1980.
    !     burton s. garbow, kenneth e. hillstrom, jorge j. more
    !     **********
    integer    :: i, j, jj, jp1, k, l
    real (dp)  :: alpha, bnorm, epsmch, gnorm, qnorm, sgnorm, sum, temp, zero,one
    zero=0.0_dp
    one=1.0_dp
    !     epsmch is the machine precision.
    epsmch = epsilon(1.0_dp)
    !     first, calculate the gauss-newton direction.
    jj = (n*(n+1)) / 2 + 1
    do  k = 1, n
       j = n - k + 1
       jp1 = j + 1
       jj = jj - k
       l = jj + 1
       sum = zero
       if (n >= jp1) then
          do  i = jp1, n
             sum = sum + r(l) * x(i)
             l = l + 1
          end do
       end if
       temp = r(jj)
       if (temp == zero) then
          l = j
          do  i = 1, j
             temp = max(temp,abs(r(l)))
             l = l + n - i
          end do
          temp = epsmch * temp
          if (temp == zero) temp = epsmch
       end if
       x(j) = (qtb(j)-sum) / temp
    end do
    !     test whether the gauss-newton direction is acceptable.
    wa1(1:n) = zero
    wa2(1:n) = diag(1:n) * x(1:n)
    qnorm = enormfun(n, wa2)
    if (qnorm > delta) then
       !     the gauss-newton direction is not acceptable.
       !     next, calculate the scaled gradient direction.
       l = 1
       do  j = 1, n
          temp = qtb(j)
          do  i = j, n
             wa1(i) = wa1(i) + r(l) * temp
             l = l + 1
          end do
          wa1(j) = wa1(j) / diag(j)
       end do
       !     calculate the norm of the scaled gradient and test for
       !     the special case in which the scaled gradient is zero.
       gnorm = enormfun(n, wa1)
       sgnorm = zero
       alpha = delta / qnorm
       if (gnorm /= zero) then
          !     calculate the point along the scaled gradient
          !     at which the quadratic is minimized.
          do  j = 1, n
             wa1(j) = (wa1(j)/gnorm) / diag(j)
          end do
          l = 1
          do  j = 1, n
             sum = zero
             do  i = j, n
                sum = sum + r(l) * wa1(i)
                l = l + 1
             end do
             wa2(j) = sum
          end do
          temp = enormfun(n, wa2)
          sgnorm = (gnorm/temp) / temp
          !     test whether the scaled gradient direction is acceptable.
          alpha = zero
          if (sgnorm < delta) then
             !     the scaled gradient direction is not acceptable.
             !     finally, calculate the point along the dogleg
             !     at which the quadratic is minimized.
             bnorm = enormfun(n, qtb)
             temp = (bnorm/gnorm) * (bnorm/qnorm) * (sgnorm/delta)
             temp = temp - (delta/qnorm) * (sgnorm/delta) ** 2 + dsqrt((  &
                  temp-(delta/qnorm))**2+(one-(delta/qnorm)**2)*(one-( sgnorm/delta)**2))
             alpha = ((delta/qnorm)*(one-(sgnorm/delta)**2)) / temp
          end if
       end if
       !     form appropriate convex combination of the gauss-newton
       !     direction and the scaled gradient direction.
       temp = (one-alpha) * min(sgnorm,delta)
       do  j = 1, n
          x(j) = temp * wa1(j) + alpha * x(j)
       end do
    end if
    return
  end subroutine dogleg

  subroutine fdjac1(fcn, n, x, fvec, fjac, ldfjac, iflag, ml, mu, epsfcn, wa1, wa2)
    implicit none
    !integer,parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: n
    real (dp), intent(in out)  :: x(n)
    real (dp), intent(in)      :: fvec(n)
    integer, intent(in)        :: ldfjac
    real (dp), intent(out)     :: fjac(ldfjac,n)
    integer, intent(in out)    :: iflag
    integer, intent(in)        :: ml, mu
    real (dp), intent(in)      :: epsfcn
    real (dp), intent(in out)  :: wa1(n)
    real (dp), intent(out)     :: wa2(n)
    ! external fcn
    interface
       subroutine fcn(n, x, fvec, iflag)
         use modparaL
         implicit none
         !integer, parameter  :: dp = selected_real_kind(14, 60)
         integer, intent(in)      :: n
         real (dp), intent(in)    :: x(n)
         real (dp), intent(out)   :: fvec(n)
         integer, intent(in out)  :: iflag
       end subroutine fcn
    end interface
    !   **********
    !   subroutine fdjac1
    !   this subroutine computes a forward-difference approximation to the n by n
    !   jacobian matrix associated with a specified problem of n functions in n
    !   variables.  if the jacobian has a banded form, then function evaluations
    !   are saved by only approximating the nonzero terms.
    !   the subroutine statement is
    !     subroutine fdjac1(fcn,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,
    !                       wa1,wa2)
    !   where
    !     fcn is the name of the user-supplied subroutine which calculates
    !       the functions.  fcn must be declared in an external statement in
    !       the user calling program, and should be written as follows.
    !       subroutine fcn(n,x,fvec,iflag)
    !       integer n,iflag
    !       real x(n),fvec(n)
    !       ----------
    !       calculate the functions at x and
    !       return this vector in fvec.
    !       ----------
    !       return
    !       end
    !       the value of iflag should not be changed by fcn unless
    !       the user wants to terminate execution of fdjac1.
    !       in this case set iflag to a negative integer.
    !     n is a positive integer input variable set to the number
    !       of functions and variables.
    !     x is an input array of length n.
    !     fvec is an input array of length n which must contain the
    !       functions evaluated at x.
    !     fjac is an output n by n array which contains the
    !       approximation to the jacobian matrix evaluated at x.
    !     ldfjac is a positive integer input variable not less than n
    !       which specifies the leading dimension of the array fjac.
    !     iflag is an integer variable which can be used to terminate
    !       the execution of fdjac1.  see description of fcn.
    !     ml is a nonnegative integer input variable which specifies
    !       the number of subdiagonals within the band of the
    !       jacobian matrix. if the jacobian is not banded, set
    !       ml to at least n - 1.
    !     epsfcn is an input variable used in determining a suitable
    !       step length for the forward-difference approximation. this
    !       approximation assumes that the relative errors in the
    !       functions are of the order of epsfcn. if epsfcn is less
    !       than the machine precision, it is assumed that the relative
    !       errors in the functions are of the order of the machine precision.
    !     mu is a nonnegative integer input variable which specifies
    !       the number of superdiagonals within the band of the
    !       jacobian matrix. if the jacobian is not banded, set
    !       mu to at least n - 1.
    !     wa1 and wa2 are work arrays of length n.  if ml + mu + 1 is at
    !       least n, then the jacobian is considered dense, and wa2 is
    !       not referenced.
    !   subprograms called
    !     minpack-supplied ... spmpar
    !     fortran-supplied ... abs,max,sqrt
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more
    !   **********
    integer    :: i, j, k, msum
    real (dp)  :: eps, epsmch, h, temp
    real (dp), parameter  :: zero = 0.0_dp
    !     epsmch is the machine precision.
    epsmch = epsilon(1.0_dp)
    eps = sqrt(max(epsfcn, epsmch))
    msum = ml + mu + 1
    if (msum >= n) then
       !        computation of dense approximate jacobian.
       do  j = 1, n
          temp = x(j)
          h = eps * abs(temp)
          if (h == zero) h = eps
          x(j) = temp + h
          call fcn(n, x, wa1, iflag)
          if (iflag < 0) exit
          x(j) = temp
          do  i = 1, n
             fjac(i,j) = (wa1(i)-fvec(i)) / h
          end do
       end do
    else
       !        computation of banded approximate jacobian.
       do  k = 1, msum
          do  j = k, n, msum
             wa2(j) = x(j)
             h = eps * abs(wa2(j))
             if (h == zero) h = eps
             x(j) = wa2(j) + h
          end do
          call fcn(n, x, wa1, iflag)
          if (iflag < 0) exit
          do  j = k, n, msum
             x(j) = wa2(j)
             h = eps * abs(wa2(j))
             if (h == zero) h = eps
             do  i = 1, n
                fjac(i,j) = zero
                if (i >= j-mu .and. i <= j+ml) fjac(i,j) = (wa1(i)-fvec(i)) / h
             end do
          end do
       end do
    end if
    return
    !     last card of subroutine fdjac1.
  end subroutine fdjac1

  subroutine qform(m, n, q, ldq, wa)
    implicit none
    !integer,parameter :: dp = kind(1.0d0)
    integer, intent(in)     :: m,n,ldq
    real (dp), intent(out)  :: q(ldq,m), wa(m)
    !   **********
    !   subroutine qform
    !   this subroutine proceeds from the computed qr factorization of an m by n
    !   matrix a to accumulate the m by m orthogonal matrix q from its factored form.
    !   the subroutine statement is
    !     subroutine qform(m,n,q,ldq,wa)
    !   where
    !     m is a positive integer input variable set to the number
    !       of rows of a and the order of q.
    !     n is a positive integer input variable set to the number of columns of a.
    !     q is an m by m array. on input the full lower trapezoid in
    !       the first min(m,n) columns of q contains the factored form.
    !       on output q has been accumulated into a square matrix.
    !     ldq is a positive integer input variable not less than m
    !       which specifies the leading dimension of the array q.
    !     wa is a work array of length m.
    !   subprograms called
    !     fortran-supplied ... min
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more
    !   **********
    integer    :: i, j, jm1, k, l, minmn, np1
    real (dp)  :: sum, temp
    real (dp), parameter  :: one = 1.0_dp, zero = 0.0_dp
    !     zero out upper triangle of q in the first min(m,n) columns.
    minmn = min(m,n)
    if (minmn >= 2) then
       do  j = 2, minmn
          jm1 = j - 1
          do  i = 1, jm1
             q(i,j) = zero
          end do
       end do
    end if
    !     initialize remaining columns to those of the identity matrix.
    np1 = n + 1
    if (m >= np1) then
       do  j = np1, m
          do  i = 1, m
             q(i,j) = zero
          end do
          q(j,j) = one
       end do
    end if
    !     accumulate q from its factored form.
    do  l = 1, minmn
       k = minmn - l + 1
       do  i = k, m
          wa(i) = q(i,k)
          q(i,k) = zero
       end do
       q(k,k) = one
       if (wa(k) /= zero) then
          do  j = k, m
             sum = zero
             do  i = k, m
                sum = sum + q(i,j) * wa(i)
             end do
             temp = sum / wa(k)
             do  i = k, m
                q(i,j) = q(i,j) - temp * wa(i)
             end do
          end do
       end if
    end do
    return
    !     last card of subroutine qform.
  end subroutine qform

  subroutine qrfac(m, n, a, lda, pivot, ipvt, lipvt, rdiag, acnorm, wa)
    implicit none
    !integer, parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: m,n,lda
    real (dp), intent(in out)  :: a(lda,n)
    logical, intent(in)        :: pivot
    integer, intent(in)        :: lipvt
    integer, intent(out)       :: ipvt(lipvt)
    real (dp), intent(out)     :: rdiag(n), acnorm(n), wa(n)
    !   **********
    !   subroutine qrfac
    !   this subroutine uses householder transformations with column pivoting
    !   (optional) to compute a qr factorization of the m by n matrix a.
    !   that is, qrfac determines an orthogonal matrix q, a permutation matrix p,
    !   and an upper trapezoidal matrix r with diagonal elements of nonincreasing
    !   magnitude, such that a*p = q*r.  the householder transformation for
    !   column k, k = 1,2,...,min(m,n), is of the form
    !                         t
    !         i - (1/u(k))*u*u
    !   where u has zeros in the first k-1 positions.  the form of this
    !   transformation and the method of pivoting first appeared in the
    !   corresponding linpack subroutine.
    !   the subroutine statement is
    !     subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
    !   where
    !     m is a positive integer input variable set to the number of rows of a.
    !     n is a positive integer input variable set to the number
    !       of columns of a.
    !     a is an m by n array.  on input a contains the matrix for which the
    !       qr factorization is to be computed.  on output the strict upper
    !       trapezoidal part of a contains the strict upper trapezoidal part of r,
    !       and the lower trapezoidal part of a contains a factored form of q
    !       (the non-trivial elements of the u vectors described above).
    !     lda is a positive integer input variable not less than m
    !       which specifies the leading dimension of the array a.
    !     pivot is a logical input variable.  if pivot is set true,
    !       then column pivoting is enforced.  if pivot is set false,
    !       then no column pivoting is done.
    !     ipvt is an integer output array of length lipvt.  ipvt defines the
    !       permutation matrix p such that a*p = q*r.
    !       column j of p is column ipvt(j) of the identity matrix.
    !       if pivot is false, ipvt is not referenced.
    !     lipvt is a positive integer input variable.  if pivot is false,
    !       then lipvt may be as small as 1.  if pivot is true, then
    !       lipvt must be at least n.
    !     rdiag is an output array of length n which contains the
    !       diagonal elements of r.
    !     acnorm is an output array of length n which contains the norms of
    !       the corresponding columns of the input matrix a.
    !       if this information is not needed, then acnorm can coincide with rdiag.
    !     wa is a work array of length n. if pivot is false, then wa
    !       can coincide with rdiag.
    !   subprograms called
    !     minpack-supplied ... spmpar,enorm
    !     fortran-supplied ... max,sqrt,min
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more
    !   **********
    integer    :: i, j, jp1, k, kmax, minmn
    real (dp)  :: ajnorm, epsmch, sum, temp
    real (dp), parameter  :: one = 1.0_dp, p05 = 0.05_dp, zero = 0.0_dp
    !     epsmch is the machine precision.
    epsmch = epsilon(1.0_dp)
    !     compute the initial column norms and initialize several arrays.
    do  j = 1, n
       acnorm(j) = enormfun(m, a(1:,j))
       rdiag(j) = acnorm(j)
       wa(j) = rdiag(j)
       if (pivot) ipvt(j) = j
    end do
    !     reduce a to r with householder transformations.
    minmn = min(m,n)
    do  j = 1, minmn
       if (pivot) then
          !        bring the column of largest norm into the pivot position.
          kmax = j
          do  k = j, n
             if (rdiag(k) > rdiag(kmax)) kmax = k
          end do
          if (kmax /= j) then
             do  i = 1, m
                temp = a(i,j)
                a(i,j) = a(i,kmax)
                a(i,kmax) = temp
             end do
             rdiag(kmax) = rdiag(j)
             wa(kmax) = wa(j)
             k = ipvt(j)
             ipvt(j) = ipvt(kmax)
             ipvt(kmax) = k
          end if
       end if
       !        compute the householder transformation to reduce the
       !        j-th column of a to a multiple of the j-th unit vector.
       ajnorm = enormfun(m-j+1, a(j:,j))
       if (ajnorm /= zero) then
          if (a(j,j) < zero) ajnorm = -ajnorm
          do  i = j, m
             a(i,j) = a(i,j) / ajnorm
          end do
          a(j,j) = a(j,j) + one
          !        apply the transformation to the remaining columns and update the norms.
          jp1 = j + 1
          if (n >= jp1) then
             do  k = jp1, n
                sum = zero
                do  i = j, m
                   sum = sum + a(i,j) * a(i,k)
                end do
                temp = sum / a(j,j)
                do  i = j, m
                   a(i,k) = a(i,k) - temp * a(i,j)
                end do
                if (.not.(.not.pivot.or.rdiag(k) == zero)) then
                   temp = a(j,k) / rdiag(k)
                   rdiag(k) = rdiag(k) * sqrt(max(zero,one-temp**2))
                   if (p05*(rdiag(k)/wa(k))**2 <= epsmch) then
                      rdiag(k) = enormfun(m-j, a(jp1:,k))
                      wa(k) = rdiag(k)
                   end if
                end if
             end do
          end if
       end if
       rdiag(j) = -ajnorm
    end do
    return
    !     last card of subroutine qrfac.
  end subroutine qrfac

  subroutine r1mpyq(m, n, a, lda, v, w)
    implicit none
    !integer, parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: m,n,lda
    real (dp), intent(in out)  :: a(lda,n)
    real (dp), intent(in)      :: v(n),w(n)
    !   **********
    !   subroutine r1mpyq
    !   given an m by n matrix a, this subroutine computes a*q where
    !   q is the product of 2*(n - 1) transformations
    !         gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
    !   and gv(i), gw(i) are givens rotations in the (i,n) plane which
    !   eliminate elements in the i-th and n-th planes, respectively.
    !   q itself is not given, rather the information to recover the
    !   gv, gw rotations is supplied.
    !   the subroutine statement is
    !     subroutine r1mpyq(m, n, a, lda, v, w)
    !   where
    !     m is a positive integer input variable set to the number of rows of a.
    !     n is a positive integer input variable set to the number of columns of a.
    !     a is an m by n array.  on input a must contain the matrix to be
    !       postmultiplied by the orthogonal matrix q described above.
    !       on output a*q has replaced a.
    !     lda is a positive integer input variable not less than m
    !       which specifies the leading dimension of the array a.
    !     v is an input array of length n. v(i) must contain the information
    !       necessary to recover the givens rotation gv(i) described above.
    !     w is an input array of length n. w(i) must contain the information
    !       necessary to recover the givens rotation gw(i) described above.
    !   subroutines called
    !     fortran-supplied ... abs, sqrt
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more
    !   **********
    integer    :: i, j, nmj, nm1
    real (dp)  :: cos, sin, temp
    real (dp), parameter  :: one = 1.0_dp
    !     apply the first set of givens rotations to a.
    nm1 = n - 1
    if (nm1 >= 1) then
       do  nmj = 1, nm1
          j = n - nmj
          ! shutao changed if to if else then
          if (abs(v(j)) > one) then
             cos = one / v(j)
             sin = sqrt(one-cos**2)
          else
             sin = v(j)
             cos = sqrt(one-sin**2)
          end if
          do  i = 1, m
             temp = cos * a(i,j) - sin * a(i,n)
             a(i,n) = sin * a(i,j) + cos * a(i,n)
             a(i,j) = temp
          end do
       end do
       !     apply the second set of givens rotations to a.
       do  j = 1, nm1
          if (abs(w(j)) > one) then
             cos = one / w(j)
             sin = sqrt(one-cos**2)
          else
             sin = w(j)
             cos = sqrt(one-sin**2)
          end if
          do  i = 1, m
             temp = cos * a(i,j) + sin * a(i,n)
             a(i,n) = -sin * a(i,j) + cos * a(i,n)
             a(i,j) = temp
          end do
       end do
    end if
    return
    !     last card of subroutine r1mpyq.
  end subroutine r1mpyq
  subroutine r1updt(m, n, s, ls, u, v, w, sing)
    implicit none
    !integer, parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: m,n,ls
    real (dp), intent(in out)  :: s(ls)
    real (dp), intent(in)      :: u(m)
    real (dp), intent(in out)  :: v(n)
    real (dp), intent(out)     :: w(m)
    logical, intent(out)       :: sing
    !   **********
    !   subroutine r1updt
    !   given an m by n lower trapezoidal matrix s, an m-vector u,
    !   and an n-vector v, the problem is to determine an
    !   orthogonal matrix q such that
    !                 t
    !         (s + u*v )*q
    !   is again lower trapezoidal.
    !   this subroutine determines q as the product of 2*(n - 1) transformations
    !         gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
    !   where gv(i), gw(i) are givens rotations in the (i,n) plane
    !   which eliminate elements in the i-th and n-th planes, respectively.
    !   q itself is not accumulated, rather the information to recover the gv,
    !   gw rotations is returned.
    !   the subroutine statement is
    !     subroutine r1updt(m,n,s,ls,u,v,w,sing)
    !   where
    !     m is a positive integer input variable set to the number of rows of s.
    !     n is a positive integer input variable set to the number
    !       of columns of s.  n must not exceed m.
    !     s is an array of length ls. on input s must contain the lower
    !       trapezoidal matrix s stored by columns. on output s contains
    !       the lower trapezoidal matrix produced as described above.
    !     ls is a positive integer input variable not less than
    !       (n*(2*m-n+1))/2.
    !     u is an input array of length m which must contain the vector u.
    !     v is an array of length n. on input v must contain the vector v.
    !       on output v(i) contains the information necessary to
    !       recover the givens rotation gv(i) described above.
    !     w is an output array of length m. w(i) contains information
    !       necessary to recover the givens rotation gw(i) described above.
    !     sing is a logical output variable.  sing is set true if any of the
    !       diagonal elements of the output s are zero.  otherwise sing is
    !       set false.
    !   subprograms called
    !     minpack-supplied ... spmpar
    !     fortran-supplied ... abs,sqrt
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more, john l. nazareth
    !   **********
    integer    :: i, j, jj, l, nmj, nm1
    real (dp)  :: cos, cotan, giant, sin, tan, tau, temp
    real (dp), parameter  :: one = 1.0_dp, p5 = 0.5_dp, p25 = 0.25_dp, zero = 0.0_dp
    !     giant is the largest magnitude.
    giant = huge(1.0_dp)
    !     initialize the diagonal element pointer.
    jj = (n*(2*m-n+1)) / 2 - (m-n)
    !     move the nontrivial part of the last column of s into w.
    l = jj
    do  i = n, m
       w(i) = s(l)
       l = l + 1
    end do
    !     rotate the vector v into a multiple of the n-th unit vector
    !     in such a way that a spike is introduced into w.
    nm1 = n - 1
    if (nm1 >= 1) then
       do  nmj = 1, nm1
          j = n - nmj
          jj = jj - (m-j+1)
          w(j) = zero
          if (v(j) /= zero) then
             !        determine a givens rotation which eliminates the j-th element of v.
             if (abs(v(n)) < abs(v(j))) then
                cotan = v(n) / v(j)
                sin = p5 / sqrt(p25+p25*cotan**2)
                cos = sin * cotan
                tau = one
                if (abs(cos)*giant > one) tau = one / cos
             else
                tan = v(j) / v(n)
                cos = p5 / sqrt(p25+p25*tan**2)
                sin = cos * tan
                tau = sin
             end if
             !        apply the transformation to v and store the information
             !        necessary to recover the givens rotation.
             v(n) = sin * v(j) + cos * v(n)
             v(j) = tau
             !        apply the transformation to s and extend the spike in w.
             l = jj
             do  i = j, m
                temp = cos * s(l) - sin * w(i)
                w(i) = sin * s(l) + cos * w(i)
                s(l) = temp
                l = l + 1
             end do
          end if
       end do
    end if
    !     add the spike from the rank 1 update to w.
    do  i = 1, m
       w(i) = w(i) + v(n) * u(i)
    end do
    !     eliminate the spike.
    sing = .false.
    if (nm1 >= 1) then
       do  j = 1, nm1
          if (w(j) /= zero) then
             !        determine a givens rotation which eliminates the
             !        j-th element of the spike.
             if (abs(s(jj)) < abs(w(j))) then
                cotan = s(jj) / w(j)
                sin = p5 / sqrt(p25 + p25*cotan**2)
                cos = sin * cotan
                tau = one
                if (abs(cos)*giant > one) tau = one / cos
             else
                tan = w(j) / s(jj)
                cos = p5 / sqrt(p25+p25*tan**2)
                sin = cos * tan
                tau = sin
             end if
             !        apply the transformation to s and reduce the spike in w.
             l = jj
             do  i = j, m
                temp = cos * s(l) + sin * w(i)
                w(i) = -sin * s(l) + cos * w(i)
                s(l) = temp
                l = l + 1
             end do
             !        store the information necessary to recover the givens rotation.
             w(j) = tau
          end if
          !        test for zero diagonal elements in the output s.
          if (s(jj) == zero) sing = .true.
          jj = jj + (m-j+1)
       end do
    end if
    !     move w back into the last column of the output s.
    l = jj
    do  i = n, m
       s(l) = w(i)
       l = l + 1
    end do
    if (s(jj) == zero) sing = .true.
    return
    !     last card of subroutine r1updt.
  end subroutine r1updt

  function enormfun(n, x) result(fn_val)
    !implicit none
    !integer, parameter :: dp = kind(1.0d0)
    integer, intent(in)    :: n
    real (dp), intent(in)  :: x(n)
    real (dp)              :: fn_val
    !   **********
    !   function enorm
    !   given an n-vector x, this function calculates the euclidean norm of x.
    !   the euclidean norm is computed by accumulating the sum of squares in three
    !   different sums.  the sums of squares for the small and large components
    !   are scaled so that no overflows occur.  non-destructive underflows are
    !   permitted.  underflows and overflows do not occur in the computation of the unscaled
    !   sum of squares for the intermediate components.
    !   the definitions of small, intermediate and large components depend on
    !   two constants, rdwarf and rgiant.  the main restrictions on these constants
    !   are that rdwarf**2 not underflow and rgiant**2 not overflow.
    !   the constants given here are suitable for every known computer.
    !   the function statement is
    !  function enormfun(n, x)
    !   where
    !     n is a positive integer input variable.
    !     x is an input array of length n.
    !   subprograms called
    !     fortran-supplied ... abs,sqrt
    !   argonne national laboratory. minpack project. march 1980.
    !   burton s. garbow, kenneth e. hillstrom, jorge j. more
    !   **********
    integer    :: i
    real (dp)  :: agiant, floatn, s1, s2, s3, xabs, x1max, x3max,zero,one
    real (dp), parameter  :: rdwarf = 1.0d-100, rgiant = 1.0d+100
    ! shutao added this following, unclear whether it is correct, commented it out
    !fn_val=0.0_dp
    zero=0.0_dp
    one=1.0_dp
    s1 = zero
    s2 = zero
    s3 = zero
    x1max = zero
    x3max = zero
    floatn = n
    agiant = rgiant / floatn
    do  i = 1, n
       xabs = abs(x(i))
       if (xabs <= rdwarf .or. xabs >= agiant) then
          if (xabs > rdwarf) then
             !              sum for large components.
             if (xabs > x1max) then
                s1 = one + s1 * (x1max/xabs) ** 2
                x1max = xabs
             else
                s1 = s1 + (xabs/x1max) ** 2
             end if
          else
             !              sum for small components.
             if (xabs > x3max) then
                s3 = one + s3 * (x3max/xabs) ** 2
                x3max = xabs
             else
                if (xabs /= zero) s3 = s3 + (xabs/x3max) ** 2
             end if
          end if
       else
          !           sum for intermediate components.
          s2 = s2 + xabs ** 2
       end if
    end do
    !     calculation of norm.
    if (s1 /= zero) then
       fn_val = x1max * sqrt(s1 + (s2/x1max)/x1max)
    else
       if (s2 /= zero) then
          ! shutao changed if to if else then
          if (s2 >= x3max) then
             fn_val = sqrt(s2*(1.0_dp + (x3max/s2)*(x3max*s3)))
          else
             fn_val = sqrt(x3max*((s2/x3max) + (x3max*s3)))
          end if
          !if (s2 < x3max) fn_val = sqrt(x3max*((s2/x3max) + (x3max*s3)))
       else
          fn_val = x3max * sqrt(s3)
       end if
    end if
    return
  end function enormfun

  !----added a duplicate of hbrd to make it easier for two layer loops of f(x_1,x_2)=0.
  subroutine hbrd2(fcn, n, x, fvec, epsfcn, tol, info, diag)
    implicit none
    !integer,parameter :: dp = kind(1.0d0)
    integer, intent(in)        :: n
    real (dp), intent(in out)  :: x(n)
    real (dp), intent(in out)  :: fvec(n)
    real (dp), intent(in)      :: epsfcn
    real (dp), intent(in)      :: tol
    integer, intent(out)       :: info
    real (dp), intent(out)     :: diag(n)
    ! external fcn
    interface
       subroutine fcn(n, x, fvec, iflag)
         use modparaL
         implicit none
         integer, intent(in) :: n
         real(dp),intent(in) :: x(n)
         real(dp), intent(out)   :: fvec(n)
         integer, intent(in out)  :: iflag
       end subroutine fcn
    end interface
    integer    :: maxfev, ml, mode, mu, nfev, nprint
    real (dp)  :: xtol
    real (dp), parameter  :: factor = 100.0_dp, zero = 0.0_dp
    info = 0
    !     check the input parameters for errors.
    if (n <= 0 .or. epsfcn < zero .or. tol < zero) return
    !     call hybrd.
    !maxfev = 200*(n + 1)
    ! changed by shutao
    maxfev = 500*(n + 1)
    xtol = tol
    ml = n - 1
    mu = n - 1
    mode = 2 ! for nonlinear equations
    !mode = 1 ! for least square problems
    nprint = 0
    call hybrd(fcn, n, x, fvec, xtol, maxfev, ml, mu, epsfcn, diag, mode, factor, nprint, info, nfev)
    if (info == 5) info = 4
    return
    !     last card of subroutine hbrd.
  end subroutine hbrd2
    
end module modhbrd
