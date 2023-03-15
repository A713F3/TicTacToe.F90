program tictactoe
    implicit none 

    character(len=1), dimension(3, 3) :: game_board = " "
    logical :: turn_player = .TRUE., game_over = .FALSE.
    integer :: move, x, y

    call fill_board(game_board)

    game_loop: do while (game_over .neqv. .TRUE.)
        call print_board(game_board)

        move = get_move()

        if (move .eq. 0) then
            exit game_loop
        end if

        move = move-1

        x = mod(move, 3) + 1
        y = (move / 3) + 1

        game_board(y, x) = "X"

        game_over = check_game_over(game_board)
    end do game_loop

    write(*,*) "Win!"
    call print_board(game_board)


contains 

    function get_move()
        integer :: get_move

        get_move = -1
        do while ((get_move .gt. 9) .or. (get_move .lt. 0))
            write(*,*) "Please pick a place from the board (1-9) (0 to exit):"
            read*, get_move
        end do

    end function get_move
    
    subroutine print_board(board)
        character(len=1), dimension(3, 3) :: board
        integer :: i

        write(*,*) "----+---+----"
        do i=1, 3
            write(*,*) "| ", board(i, 1), " | ", board(i, 2), " | ", board(i, 3), " |"
            write(*,*) "----+---+----"
        end do

    end subroutine print_board

    subroutine fill_board(board)
        character(len=1), dimension(3, 3) :: board
        integer :: i, j, n=1

        do i=1, 3
            do j=1, 3
                board(i, j) = char(n + ichar('0'))
                n = n + 1
            end do
        end do

    end subroutine fill_board

    function check_line(a1, a2, a3)
        logical check_line
        character(len=1) :: a1, a2, a3
        check_line = (a1 .eq. a2) .and. (a1 .eq. a3) .and. (a2 .eq. a3)
    end function

    function check_game_over(board) 
        character(len=1), dimension(3, 3) :: board
        ! horizontal, vertical and diagonal lines
        logical :: check_game_over, ht, hm, hb, vl, vm, vr, d1, d2

        ht = check_line(board(1,1), board(1,2), board(1,3))
        hm = check_line(board(2,1), board(2,2), board(2,3))
        hb = check_line(board(3,1), board(3,2), board(3,3))

        vl = check_line(board(1,1), board(2,1), board(3,1))
        vm = check_line(board(1,2), board(2,2), board(3,2))
        vr = check_line(board(1,3), board(2,3), board(3,3))

        d1 = check_line(board(1,1), board(2,2), board(3,3))
        d2 = check_line(board(1,3), board(2,2), board(3,1))

        check_game_over = ht .or. hm .or. hb .or. vl .or. vm .or. vr .or. d1 .or. d2            

    end function check_game_over

end program tictactoe