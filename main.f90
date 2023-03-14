program tictactoe
    implicit none 

    character(len=1), dimension(3, 3) :: board = 'x'

    call fill_board(board)
    
    call print_board(board)


contains 
    
    subroutine print_board(b)
        character(len=1), dimension(3, 3) :: b
        integer :: i

        write(*,*) "----+---+----"
        do i=1, 3
            write(*,*) "| ", b(i, 1), " | ", b(i, 2), " | ", b(i, 3), " |"
            write(*,*) "----+---+----"
        end do

    end subroutine print_board

    subroutine fill_board(b)
        character(len=1), dimension(3, 3) :: b
        integer :: i, j, n=1

        do i=1, 3
            do j=1, 3
                b(i, j) = char(n + ichar('0'))
                n = n + 1
            end do
        end do

    end subroutine fill_board


end program tictactoe