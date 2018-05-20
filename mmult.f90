program mmult
  implicit none
  integer, parameter :: size = 800
  integer, parameter :: max_exec = 3
  logical, parameter :: debug = .false.
  integer, dimension(1:size,1:size) :: A, B, C
  integer :: i, j, k, sum, exec, scale
  real :: start, finish

  ! creating matrices to do multiplication
  call matrix_init(A, size, 1)
  call matrix_init(B, size, 2)

  ! running matrix mult
  do exec = 1, max_exec
    call cpu_time(start)
    do i = 1, size
      do j = 1, size
        sum = 0
        do k = 1, size
          sum = sum + A(i, k) * B(k, j)
        enddo
        C(i,j) = sum
      enddo
    enddo
    call cpu_time(finish)
    print '("Time = ",f6.3," seconds.")',finish-start
  enddo

  ! printing results
  if (debug .eqv. .true.) then
    print *, "Matrix A"
    call print_matrix(A, size)
    print *, "Matrix B"
    call print_matrix(B, size)
    print *, "Matrix C"
    call print_matrix(C, size)
  end if

  ! clearing matrix C
  do j=1,size
    do i=1,size
      C(i,j) = 0
    enddo
  enddo

  ! running matrix mult with unit stride
  do exec = 1, max_exec
    call cpu_time(start)
    do k=1, size
      do j=1, size
        scale=B(k,j)
        do i=1,size
          C(i,j) = C(i,j) + scale * A(i,k)
        enddo
      enddo
    enddo
    call cpu_time(finish)
    print '("Time = ",f6.3," seconds.")',finish-start
  enddo

  ! printing results
  if (debug .eqv. .true.) then
    print *, "Matrix A"
    call print_matrix(A, size)
    print *, "Matrix B"
    call print_matrix(B, size)
    print *, "Matrix C"
    call print_matrix(C, size)
  end if

contains
  subroutine print_matrix( matrix, n )
    implicit none

    ! input and output variables
    integer, dimension(1:n, 1:n) :: matrix
    integer, intent(in) :: n

    ! local variables
    integer :: i, j

    do i=1,n
      write(*,"(10g5.5)") ( matrix(i,j), j=1,n )
    enddo

    return
  end subroutine print_matrix

  subroutine matrix_init( matrix, n, offset )
    implicit none

    ! input and output variables
    integer, dimension(1:n,1:n) :: matrix ! array to be initialized
    integer, intent(in) :: n              ! size of the array
    integer, intent(in) :: offset         ! used to shift srand

    ! local variables
    integer :: i, j, clock

    ! initializes seed for random number generation
    call system_clock(COUNT=clock)
    call srand(clock+offset)

    ! start initializing the matrix
    do i = 1,n
      do j = 1,n
        matrix(i, j) = rand() * 10
      enddo
    enddo

    return
  end subroutine matrix_init

end program mmult
