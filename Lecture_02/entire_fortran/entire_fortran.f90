! This is a comment.

! Code exists only inside program units: main program, procedures (functions and
! subroutines), modules.
program entire_fortran ! declare a main program named `entire_fortran`
    ! Using identation is not required but it is recommended.

    ! Modules are used before declarations.
    ! use :: my_module, only: my_function

    !+
    ! DECLARATIONS
    !-

    ! Declarations of types and variables must come before any other statements
    ! or expressions.

    ! Declare `implicit none` in the beginning of every program/module/procedure
    ! but after `use` statement.
    implicit none    ! switch off implicit typing.

    ! Fortran is not case sensitive, `rr` and `RR` are the same name.
    !real rr
    REAL RR

    ! In what follows, the `::` separator is often optional but indicates a good
    ! coding style.

    ! Initial values of variables contain memory garbage and depend on compiler.
    real :: v, x

    ! Default initialization with a literal constant follows the = sign.

    ! Integers, some are initialized.
    integer :: i, j, k = 1, m

    ! Real numbers.
    real :: a = 3, b = 2E12, c = 0.01

    ! Declare a complex variable and set to i = sqrt(-1).  It is a tuple of two
    ! reals.
    complex :: i_z = (0.0, 1.0) ! sqrt(-1)

    ! Declare boolean type and initialize to either true or false.
    logical :: yes = .true., no = .false.

    ! Declare a character and a string of 3 characters long.
    character :: one_char = 'i'
    character(len=3) :: month = 'Sep'

    ! Declare a pointer to real using `pointer` attribute.
    real, pointer :: p => null()

    !+
    ! Parameters (constants)
    !-

    ! Declare a real constant.  Note the upper case style
    real, parameter :: PI = 3.1415926535897931

    integer :: arr(7)

!goto 111
    ! Basic I/O with print and read
    print *, i, a, month
    read  *, i, a, month
    print '(i0, 1x, f6.3, 1x, a3)', i, a, month

    read  * ! press enter to continue
    print * ! new line

    ! I/O is record based.  The record lenght (the number of elements) is either
    ! taken from the io-list or from the format.  The latter has higher
    ! priority.
    read *, arr(1:7)
    print '(5i3)', arr

    ! More advanced forms of read and write: (unit=*, fmt=*) by default.
    read(*, *)  i, a, month
    write(*, *) i, a, month
!111 continue

    !+
    ! Integer
    !-
    block
        integer, parameter :: k  = selected_int_kind(1)
        integer, parameter :: k2 = kind(0)
        integer :: i, j
        integer(kind=k) :: i1 = 127_k
!goto 222
        i = 5; j = 2
        print *, i + j, i - j, i * j, i / j, i**j
        print *, i == j, i /= j, i < j, i > j, i <= j, i >= j
        print *, kind(i), huge(i), range(i)

        print '(i0)', i ! format descriptor is `i`
!222     continue
    end block

    !+
    ! Real
    !-
    block
        real :: x = 3.1415926535
        !real(kind=4) :: x = 3.1415926535e0
        real(kind=8) :: y = 2.71828182d0
        real(kind=16) :: w = 2.71828182q0
!goto 333
        print *, huge(x), tiny(x), epsilon(x), precision(x)
        print '(a, f20.15)', 'x = ', x ! real descriptors are f, e, es, en, g
!333     continue
    end block

    !+
    ! Complex
    !-
    block
        complex :: my_z = (0.0, 1.0) ! \sqrt(-1)
!goto 444
        print *, my_z
        print *, cmplx(1, 5)
        print *, my_z%re, my_z%im
        print *, real(my_z), aimag(my_z)
!444     continue
    end block

    !+
    ! Type coercion
    !-
    block
        integer :: i = 5
        real    :: x = 2.0
        complex :: w = (0.0, 1.0)

!goto 555
        print *, i + x
        print *, i * x / w
!555     continue
    end block

    !+
    ! Logical
    !-
    block
        logical :: flag = .true., mask = .false.
!goto 666
        print *, flag
        print *, .not.flag.and.mask, flag .eqv. mask, flag .or. mask
        print '(2l1)', mask, flag
!666     continue
    end block

    !+
    ! Character
    !-
    block
        character :: a_char = 'x'
        character(len=5) :: str = 'January'
        character(len=*), parameter :: PASSWORD = 'theonlywayoutisin'
        character(len=3) :: mon1 = 'Jan', mon2 = 'Sep'
        character(len=10) :: blank = ''
        character(len=:), allocatable :: dstr
!goto 777
        print *, a_char
        print *, str
        print *, "ain't"
        print *, "'", PASSWORD, "'"
        print *, str // PASSWORD
        print *, lle(mon1, mon2) ! mon1 < mon2
        print '(a)', repeat('.', 80)
        print '("|", a, "|")', blank

        !allocate(character(len=7) :: dstr)
        !dstr(:) = repeat('.', 7)
        !dstr(3:5) = 'For'
        dstr = 'Java'
        print '(3a)', '|', dstr, '|'
        print '(a)', dstr(3:3)
        deallocate(dstr)
!777     continue
    end block

    block
        integer          :: ii = -1
        real             :: xx = 2.718281
        complex          :: zz = (1, -1)
        character(len=7) :: name = 'January'
        logical          :: is_ok = .true.

        ! Generic format descriptor for all 5 types:
        print '(6(g0, 2x))', ii, xx, zz, name, is_ok
    end block

end program entire_fortran