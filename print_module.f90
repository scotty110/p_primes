module print_module
    use, intrinsic :: iso_fortran_env
    implicit none

    contains

    subroutine printPrimes(primes)
        integer(int64), dimension(:), intent(in) :: primes
        integer(int64) :: i

        print *, "Primes:"
        do i = 1, size(primes)
            print *, primes(i)
        end do
    end subroutine printPrimes
end module print_module