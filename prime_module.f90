module prime_module 
    use mpi_f08
    use, intrinsic :: iso_fortran_env
    implicit none

    contains

        logical function MillerRabin(n, k) result(is_prime_flag)
            integer(int64), intent(in) :: n, k
            is_prime_flag = is_prime(n, k)

        contains

            function mod_exp(base, exponent, modulus) result(res)
                integer(int64), intent(in) :: base, exponent, modulus
                integer(int64) :: res, base_temp, exp_temp
                res = 1
                base_temp = mod(base, modulus)
                exp_temp = exponent

                do while (exp_temp > 0)
                    if (mod(exp_temp, 2_int64) == 1) then
                        res = mod(res * base_temp, modulus)
                    end if
                    exp_temp = exp_temp / 2_int64
                    base_temp = mod(base_temp**2_int64, modulus)
                end do
            end function mod_exp

            logical function is_prime(n, k) result(is_prime_flag)
                integer(int64), intent(in) :: n, k
                !logical :: is_prime_flag
                integer(int64) :: d, a, x, i, r, s
                real :: randReal

                if (n == 2_int64) then
                    is_prime_flag = .true.
                    return
                end if
                if (mod(n, 2_int64) == 0 .or. n < 2) then
                    is_prime_flag = .false.
                    return
                end if

                ! Write n-1 as d*2^s
                d = n - 1
                s = 0
                do while (mod(d, 2_int64) == 0)
                    d = d / 2
                    s = s + 1
                end do

                ! Witness loop
                do i = 1, k
                    call random_number(randReal)
                    a = 2 + int(randReal * (n - 4))
                    x = mod_exp(a, d, n)

                    if (x == 1_int64 .or. x == n - 1_int64) cycle

                    do r = 1, s - 1
                        x = mod_exp(x, 2_int64, n)
                        if (x == 1) then
                            is_prime_flag = .false.
                            return
                        end if
                        if (x == n - 1_int64) exit
                    end do

                    if (x /= n - 1_int64) then
                        is_prime_flag = .false.
                        return
                    end if
                end do

                is_prime_flag = .true.
            end function is_prime
        end function MillerRabin


        subroutine find_primes(myid, local_numbers, local_primes, count)
            integer, intent(in) :: myid
            integer(int64), dimension(:), intent(in) :: local_numbers
            integer(int64), dimension(:), intent(out) :: local_primes
            integer, intent(out) :: count 
            integer :: i

            do i = 1, size(local_numbers) 
                if (MillerRabin(local_numbers(i), 10_int64)) then
                    local_primes(count) = local_numbers(i)
                    Print *, "Process: ", myid, "found prime ", local_numbers(i)
                    count = count + 1
                end if
            end do

        end subroutine find_primes 
end module prime_module