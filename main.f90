subroutine generateRandomIntegers(size, maxVal, randomIntegers)
    use, intrinsic :: iso_fortran_env
    implicit none
    integer, intent(in) :: size, maxVal
    integer(int64), dimension(:), intent(out) :: randomIntegers(size)
    integer(int64) :: i
    real :: randReal

    ! Seed the random number generator
    call random_seed()

    ! Generate random integers
    do i = 1, size
        call random_number(randReal)
        randomIntegers(i) = int(randReal * maxVal, kind=int64) + 1_int64  ! Convert to integer in the range 1 to maxVal
    end do
end subroutine generateRandomIntegers


program main 
    use mpi_f08
    use prime_module ! Our code
    use print_module ! Our code
    use, intrinsic :: iso_fortran_env
    implicit none

    ! Variables
    integer :: i, local_size, size, maxVal, num_procs, myid, ierr
    integer, dimension(:), allocatable :: recvcounts, displs
    integer(int64), allocatable :: random_numbers(:)
    integer(int64), dimension(:), allocatable :: primes, local_numbers, all_primes, temp
    logical :: flag

    integer :: num_primes = 1

    ! Initialize MPI
    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

    allocate(recvcounts(num_procs), displs(num_procs))

    ! Initialize random numbers
    size = 100000
    if (myid == 0) then
        ! Variables w/ values
        maxVal = 100000000
        allocate(random_numbers(size))

        ! Generate random numbers
        call generateRandomIntegers(size, maxVal, random_numbers)
    end if

    ! Block until node 0 has finished generating random numbers
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! Determine local array size and allocate memory for all nodes
    local_size = size / num_procs
    allocate(local_numbers(local_size))
    allocate(primes(local_size))

    ! Scatter the numbers to all nodes
    call MPI_Scatter(random_numbers, local_size, MPI_INT64_T, &
                     local_numbers, local_size, MPI_INT64_T, 0, MPI_COMM_WORLD, ierr)

    ! Find primes on each node
    !num_primes = 1 ! Setting this here causes programm to crash
    call find_primes(myid, local_numbers, primes, num_primes)
   
    ! Resize the local_primes array to contain only the primes
    if (num_primes > 1) then
        ! Move the allocation to the temporary array
        num_primes = num_primes - 1

        allocate(temp(num_primes))
        temp = primes(1:num_primes) 

        deallocate(primes)
        allocate(primes(num_primes))
        primes = temp

        deallocate(temp)
    else
        ! If num_primes is less than 1, return an empty array
        deallocate(primes)
        allocate(primes(0))
    end if

    ! Gather the number of primes found by each process to the root process
    call MPI_Gather(num_primes, 1, MPI_INT, recvcounts, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)

    if (myid == 0) then
        ! Calculate the total number of primes and the displacements
        displs(1) = 0
        do i = 2, num_procs
            displs(i) = displs(i-1) + recvcounts(i-1)
        end do
        allocate(all_primes(sum(recvcounts)))
    end if

    ! Gather the primes found by each process to the root process
    call MPI_Gatherv(primes, num_primes, MPI_INT64_T, all_primes, recvcounts, displs, MPI_INT64_T, 0, MPI_COMM_WORLD, ierr)

    ! Barrier to synchronize before printing
    call MPI_Barrier(MPI_COMM_WORLD, ierr)

    ! Print results on node 0
    if (myid == 0) then
        call printPrimes(all_primes)
    end if

    ! Finalize MPI
    call MPI_Finalize(ierr)

    ! Deallocate arrays
    if (allocated(random_numbers)) deallocate(random_numbers)
    if (allocated(local_numbers)) deallocate(local_numbers)
    if (allocated(primes)) deallocate(primes)
end program main 