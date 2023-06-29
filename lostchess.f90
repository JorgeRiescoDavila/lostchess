
program lostchess
  implicit none
  integer,parameter::max_moves_per_game = 1024
  integer,parameter::max_moves_listed = 8192
  !side to move codes
  integer,parameter::team_white = 0
  integer,parameter::team_black = 1
  integer,parameter::team_none = 2
  !piece codes
  integer,parameter::empty = 0
  integer,parameter::wp = 1
  integer,parameter::wn = 2
  integer,parameter::wb = 3
  integer,parameter::wr = 4
  integer,parameter::wq = 5
  integer,parameter::wk = 6
  integer,parameter::bp = 7
  integer,parameter::bn = 8
  integer,parameter::bb = 9
  integer,parameter::br = 10
  integer,parameter::bq = 11
  integer,parameter::bk = 12
  !piece lookups
  integer,parameter,dimension(0:12)::get_color = (/ team_none, &
  & team_white,team_white,team_white,team_white,team_white,team_white, &
  & team_black,team_black,team_black,team_black,team_black,team_black /)
  logical,parameter,dimension(0:12)::is_queen_rook = (/ .false., &
  & .false.,.false.,.false.,.true.,.true.,.false., &
  & .false.,.false.,.false.,.true.,.true.,.false. /)
  logical,parameter,dimension(0:12)::is_queen_bishop = (/ .false., &
  & .false.,.false.,.true.,.false.,.true.,.false., &
  & .false.,.false.,.true.,.false.,.true.,.false. /)
  logical,parameter,dimension(0:12)::is_knight = (/ .false., &
  & .false.,.true.,.false.,.false.,.false.,.false., &
  & .false.,.true.,.false.,.false.,.false.,.false. /)
  logical,parameter,dimension(0:12)::is_king = (/ .false., &
  & .false.,.false.,.false.,.false.,.false.,.true., &
  & .false.,.false.,.false.,.false.,.false.,.true. /)
  !important tiles
  integer,parameter::ob = -1
  integer,parameter::a1 = 0
  integer,parameter::b1 = 1
  integer,parameter::c1 = 2
  integer,parameter::d1 = 3
  integer,parameter::e1 = 4
  integer,parameter::f1 = 5
  integer,parameter::g1 = 6
  integer,parameter::h1 = 7
  integer,parameter::a2 = 16
  integer,parameter::h2 = 23
  integer,parameter::a7 = 96
  integer,parameter::h7 = 103
  integer,parameter::a8 = 112
  integer,parameter::b8 = 113
  integer,parameter::c8 = 114
  integer,parameter::d8 = 115
  integer,parameter::e8 = 116
  integer,parameter::f8 = 117
  integer,parameter::g8 = 118
  integer,parameter::h8 = 119
  !castling permissions
  integer,parameter::cp_wq = 2
  integer,parameter::cp_wk = 1
  integer,parameter::cp_bq = 8
  integer,parameter::cp_bk = 4
  integer,parameter::cp_all = cp_wq+cp_wk+cp_bq+cp_bk
  integer,parameter,dimension(0:127)::cp_table = (/ &
  & cp_all-cp_wq,cp_all,cp_all,cp_all,cp_all-cp_wq-cp_wk,cp_all,cp_all,cp_all-cp_wk,0,0,0,0,0,0,0,0, &
  & cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,0,0,0,0,0,0,0,0, &
  & cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,0,0,0,0,0,0,0,0, &
  & cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,0,0,0,0,0,0,0,0, &
  & cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,0,0,0,0,0,0,0,0, &
  & cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,0,0,0,0,0,0,0,0, &
  & cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,cp_all,0,0,0,0,0,0,0,0, &
  & cp_all-cp_bq,cp_all,cp_all,cp_all,cp_all-cp_bq-cp_bk,cp_all,cp_all,cp_all-cp_bk,0,0,0,0,0,0,0,0 /)
  !piece directions
  integer,dimension(0:7),parameter::directions_rook        = (/  1, 16, -1,-16,  0,  0,  0,  0/)
  integer,dimension(0:7),parameter::directions_bishop      = (/ 15, 17,-15,-17,  0,  0,  0,  0/)
  integer,dimension(0:7),parameter::directions_rook_bishop = (/  1, 16, -1,-16, 15, 17,-15,-17/)
  integer,dimension(0:7),parameter::directions_knight      = (/ 14, 18, 31, 33,-14,-18,-31,-33/)
  
  character(len=2),parameter,dimension(-1:127)::raw2algebraic = (/ '- ', & 
  &'a1','b1','c1','d1','e1','f1','g1','h1','ob','ob','ob','ob','ob','ob','ob','ob', &
  &'a2','b2','c2','d2','e2','f2','g2','h2','ob','ob','ob','ob','ob','ob','ob','ob', &
  &'a3','b3','c3','d3','e3','f3','g3','h3','ob','ob','ob','ob','ob','ob','ob','ob', &
  &'a4','b4','c4','d4','e4','f4','g4','h4','ob','ob','ob','ob','ob','ob','ob','ob', &
  &'a5','b5','c5','d5','e5','f5','g5','h5','ob','ob','ob','ob','ob','ob','ob','ob', &
  &'a6','b6','c6','d6','e6','f6','g6','h6','ob','ob','ob','ob','ob','ob','ob','ob', &
  &'a7','b7','c7','d7','e7','f7','g7','h7','ob','ob','ob','ob','ob','ob','ob','ob', &
  &'a8','b8','c8','d8','e8','f8','g8','h8','ob','ob','ob','ob','ob','ob','ob','ob' /)
  
  type type_move
    integer::ini
    integer::fin
    logical::is_capture
    integer::promotion_pie
    logical::is_pawnstart
    logical::is_enpassant
    logical::is_castling
    integer::captured_pie
  end type
  type(type_move),parameter::move_null = type_move(ob,ob,.FALSE.,empty,.FALSE.,.FALSE.,.FALSE.,empty)
  
  type type_hash_table
    integer,dimension(0:12,0:127)::board
    integer::side
    integer::ep
    integer,dimension(0:15)::cp
  end type
  type(type_hash_table)::hash_table
  
  !board
  integer,dimension(0:127)::board
  
  !state
  type type_state
    integer::side
    integer::ep
    integer::cp
  end type
  type(type_state)::state

  !hash
  integer::position_hash
  
  !number of moves
  integer::ply
  
  !history
  type type_history
    integer::cp
    integer::ep
    integer::hash
    type(type_move)::move
  end type
  type(type_history),dimension(0:max_moves_per_game-1)::hist

  !moves list
  integer,dimension(0:max_moves_per_game-1)::moves_list_ind
  type(type_move),dimension(0:max_moves_listed-1)::moves_list
  
  !perft
  type type_perft
    integer::node
    integer::capture
    integer::enpassant
    integer::castle
    integer::promotion
    integer::check
  end type
  type(type_perft)::perft_results
  
  !search engine
  type type_search_const
    integer,dimension(0:12,0:127)::piece_sq
    integer,dimension(0:12)::piece_value
  end type
  type(type_search_const)::search_const1

  !testing variables
  integer::selected_option
  
  !init program
  call init()
  call restart_game()

  do while (.true.)
    write(*,*) 'select options -----------------------------------------------------------------'
    write(*,*) '1 write board numbers | 2 test_common_moves | 3 test_gen_moves_nopawns'
    write(*,*) '4 test_do_undo_nopawns | 5 test_do_undo_wp | 6 test_do_undo_bp '
    write(*,*) '7 perft | 8 test set_fen | 9 play'
    write(*,*) '--------------------------------------------------------------------------------'
    read*, selected_option
    select case (selected_option)
      case (1)
        call write_board_numbers
      case (2)
        call test_common_moves()
      case (3)
        call test_gen_moves_nopawns()
      case (4)
        call test_do_undo_nopawns
      case (5)
        call test_do_undo_wp
      case (6)
        call test_do_undo_bp
      case (7)
        call perft_handler()
      case (8)
        call test_set_fen()
      case (9)
        do while(.true.)
          call ask_player()
        end do
     case default
        exit
    end select
  end do
  
  contains
  
  subroutine mirror(board)
    integer,dimension(0:127)::board
    integer,dimension(0:15)::aux_row
    aux_row = board(0:15)
    board(0:15) = board(112:127)
    board(112:127) = aux_row
    
    aux_row = board(16:31)
    board(16:31) = board(96:111)
    board(96:111) = aux_row
    
    aux_row = board(32:47)
    board(32:47) = board(80:95)
    board(80:95) = aux_row
    
    aux_row = board(48:63)
    board(48:63) = board(64:79)
    board(64:79) = aux_row
  end subroutine
  
  subroutine ask_player()
    integer::m_ind = -1
    call write_board_raw()
    call gen_moves()
    call write_moves()
    write(*,*) 'write move index'
    do while(.not.(moves_list_ind(ply) <= m_ind .and. m_ind <= moves_list_ind(ply+1)-1))
      read*, m_ind
    end do
    call make_move(moves_list(m_ind))
  end subroutine
  
  subroutine init()
    call init_hash_table()
    call init_search_const()
  end subroutine

  subroutine init_hash_table()
    integer::ind,pie
    do pie=0,12
      do ind = 0,127
        hash_table%board(pie,ind) = floor(abs(sin(ind+pie*128+1.)*2**30))
      end do
    end do
    hash_table%side = floor(abs(sin(1664+1.)*2**30))
    hash_table%ep = floor(abs(sin(1665+1.)*2**30))
    do ind = 0,15
      hash_table%cp(ind) = floor(abs(sin(ind+1666+1.)*2**30))
    end do
  end subroutine

  subroutine init_search_const()
    integer::p_ind
    search_const1%piece_value = (/ 0,100,310,320,500,950,100000,100,310,320,500,950,100000 /)
  
    search_const1%piece_sq(bp,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      & 50, 50, 50, 50, 50, 50, 50, 50, 0,0,0,0,0,0,0,0, &
      & 10, 10, 20, 30, 30, 20, 10, 10, 0,0,0,0,0,0,0,0, &
      &  5,  5, 10, 25, 25, 10,  5,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0, 20, 20,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, -5,-10,  0,  0,-10, -5,  5, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10,-20,-20, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0 /)
      
    search_const1%piece_sq(bn,0:127) = (/ & 
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  0,  0,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -30,  0, 10, 15, 15, 10,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 15, 20, 20, 15,  5,-30, 0,0,0,0,0,0,0,0, &
      & -30,  0, 15, 20, 20, 15,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 10, 15, 15, 10,  5,-30, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  5,  5,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0 /)
      
    search_const1%piece_sq(bb,0:127) = (/ & 
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5, 10, 10,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5, 10, 10,  5,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0, 10, 10, 10, 10,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10, 10, 10, 10, 10, 10, 10,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  0,  0,  0,  0,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0 /)

    search_const1%piece_sq(br,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10, 10, 10, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  5,  5,  0,  0,  0,  0,0,0,0,0,0,0,0 /)
      
    search_const1%piece_sq(bq,0:127) = (/ & 
      &  -20,-10,-10, -5, -5,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &   -5,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &    0,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10, -5, -5,-10,-10,-20,  0,0,0,0,0,0,0,0 /)

    search_const1%piece_sq(bk,0:127) = (/ & 
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -20,-30,-30,-40,-40,-30,-30,-20, 0,0,0,0,0,0,0,0, &
      &  -10,-20,-20,-20,-20,-20,-20,-10, 0,0,0,0,0,0,0,0, &
      &   20, 20,  0,  0,  0,  0, 20, 20, 0,0,0,0,0,0,0,0, &
      &   20, 30, 10,  0,  0, 10, 30, 20,  0,0,0,0,0,0,0,0 /)
      
    search_const1%piece_sq(1:6,0:127) = search_const1%piece_sq(7:12,0:127)
    do p_ind=0,6
      call mirror(search_const1%piece_sq(p_ind,0:127))
    end do
  end subroutine

  subroutine restart_game()
    call restart_board()
    call restart_state()
    position_hash = hash_position()
    ply = 0
    call restart_history()
    call restart_move_list()
  end subroutine
  
  subroutine restart_board()
    board = empty
    board(a1:h1) = (/wr,wn,wb,wq,wk,wb,wn,wr/)
    board(a2:h2) = wp
    board(a7:h7) = bp
    board(a8:h8) = (/br,bn,bb,bq,bk,bb,bn,br /)
  end subroutine
  
  subroutine restart_state()
    state%side = team_white
    state%cp = cp_all
    state%ep = ob
  end subroutine
  
  subroutine restart_history()
    hist%cp = cp_all
    hist%ep = ob
    hist%move = move_null
  end subroutine
  
  subroutine restart_move_list()
    moves_list_ind = 0
    moves_list = move_null 
  end subroutine
  
  function hash_position()
    integer::hash_position,ind,pie
    hash_position = 0
    do ind=0,127
      if(iand(ind,136) == 0)then
        pie = board(ind)
        hash_position = ieor(hash_position,hash_table%board(pie,ind))
      end if
    end do
    hash_position = ieor(hash_position,hash_table%side)
    hash_position = ieor(hash_position,hash_table%ep)
    hash_position = ieor(hash_position,hash_table%cp(state%cp))
  end function

  function there_is_enemy_on(fin)
    logical::there_is_enemy_on
    integer::fin
    there_is_enemy_on = get_color(board(fin)) .eq. ieor(1,state%side)
  end function

  subroutine add_move(ini,fin,is_capture,promotion_pie,is_pawnstart,is_enpassant,is_castling,captured_pie)
    type(type_move)::m
    integer::ini,fin,promotion_pie,captured_pie,king_ind
    logical::is_capture,is_pawnstart,is_enpassant,is_castling,is_legal
    is_legal = .false.
    m%ini = ini
    m%fin = fin
    m%is_capture = is_capture
    m%promotion_pie = promotion_pie
    m%is_pawnstart = is_pawnstart
    m%is_enpassant = is_enpassant
    m%is_castling = is_castling
    m%captured_pie = captured_pie
    
    call make_move(m)
    do king_ind = 0,127
      if(is_king(board(king_ind)) .and. get_color(board(king_ind)) == ieor(1,state%side))then
        exit
      end if
    end do
    if(.not. is_attacked(king_ind,state%side))then
      is_legal =.true.
    end if
    call undo_last_move()
    
    if(is_legal)then
      moves_list(moves_list_ind(ply+1)) = m
      moves_list_ind(ply+1) = moves_list_ind(ply+1) + 1
    end if
  end subroutine
  
  subroutine gen_pawn_moves(ini)
    integer::ini,move_direction,fin,last_row_a,last_row_h,starting_row_a,starting_row_h,promotion_pie,d_ind
    integer::promo_q,promo_r,promo_b,promo_n
    integer,dimension(0:1)::capture_directions
    
    if(board(ini) == empty)return
    
    if(state%side == team_white)then
      move_direction = 16
      capture_directions = (/15,17/)
      last_row_a = a7
      last_row_h = h7
      starting_row_a = a2
      starting_row_h = h2
      promo_q = wq
      promo_r = wr
      promo_b = wb
      promo_n = wn
    end if
    
    if(state%side == team_black)then
      move_direction = -16
      capture_directions = (/-15,-17/)
      last_row_a = a2
      last_row_h = h2
      starting_row_a = a7
      starting_row_h = h7
      promo_q = bq
      promo_r = br
      promo_b = bb
      promo_n = bn
    end if
    
    do d_ind=0,1
      fin = ini+capture_directions(d_ind)
      if(iand(fin,136) == 0)then
        if(there_is_enemy_on(fin))then
          if(ini >= last_row_a .and. ini <= last_row_h)then
            call add_move(ini,fin,.TRUE.,promo_q,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_r,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_b,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_n,.FALSE.,.FALSE.,.FALSE.,board(fin))
          else      
            call add_move(ini,fin,.TRUE.,empty,.FALSE.,.FALSE.,.FALSE.,board(fin))
          end if
        end if
        if(fin == state%ep)then !en passants doesnt have capture flag becuase they are handled separately
          call add_move(ini,fin,.FALSE.,empty,.FALSE.,.TRUE.,.FALSE.,empty)
        end if
      end if
    end do
    fin = ini+move_direction
    if(iand(fin,136) == 0)then
      if(board(fin) == empty)then
        if(ini >= last_row_a .and. ini <= last_row_h)then
          call add_move(ini,fin,.TRUE.,promo_q,.FALSE.,.FALSE.,.FALSE.,board(fin))
          call add_move(ini,fin,.TRUE.,promo_r,.FALSE.,.FALSE.,.FALSE.,board(fin))
          call add_move(ini,fin,.TRUE.,promo_b,.FALSE.,.FALSE.,.FALSE.,board(fin))
          call add_move(ini,fin,.TRUE.,promo_n,.FALSE.,.FALSE.,.FALSE.,board(fin))
        else
          call add_move(ini,fin,.FALSE.,empty,.FALSE.,.FALSE.,.FALSE.,empty)
          fin = ini+2*move_direction
          if(ini >= starting_row_a .and. ini <= starting_row_h .and. board(fin)==empty)then
            call add_move(ini,ini+2*move_direction,.FALSE.,empty,.TRUE.,.FALSE.,.FALSE.,empty)
          end if
        end if
      end if
    end if
  end subroutine

  subroutine gen_castling_moves()
    if(state%side == team_white .and. board(e1)==wk)then
      if(iand(state%cp,cp_wq) /= 0)then
        if(board(b1)==empty .and. board(c1)==empty .and. board(d1)==empty) then
          if( .not. is_attacked(e1,team_black) .and. .not. is_attacked(d1,team_black))then
            call add_move(e1,c1,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        endif
      end if
      if(iand(state%cp,cp_wk) /= 0)then
        if(board(g1)==empty .and. board(f1)==empty) then
          if( .not. is_attacked(e1,team_black) .and. .not. is_attacked(f1,team_black))then
            call add_move(e1,g1,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        endif
      end if
    end if
    if(state%side == team_black .and. board(e8)==bk)then
      if(iand(state%cp,cp_bq) /= 0)then
        if(board(b8)==empty .and. board(c8)==empty .and. board(d8)==empty) then 
          if( .not. is_attacked(e8,team_white) .and. .not. is_attacked(d8,team_white))then
            call add_move(e8,c8,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        endif
      end if
      if(iand(state%cp,cp_bk) /= 0)then
        if(board(g8)==empty .and. board(f8)==empty) then
          if( .not. is_attacked(e8,team_white) .and. .not. is_attacked(f8,team_white))then
            call add_move(e8,g8,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        endif
      end if
    end if
  end subroutine

  subroutine gen_common_moves(ini,directions,is_slide)
    integer,intent(in)::ini
    integer,dimension(0:7)::directions
    integer::dir,dir_ind,fin
    logical::is_slide
    do dir_ind=0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = ini+dir
      do while (iand(fin,136) == 0)
        if(board(fin) == empty)then
          call add_move(ini,fin,.FALSE.,empty,.FALSE.,.FALSE.,.FALSE.,empty)
        else if(there_is_enemy_on(fin)) then
          call add_move(ini,fin,.TRUE.,empty,.FALSE.,.FALSE.,.FALSE.,board(fin))
          exit
        else
          exit
        end if
        if(.not. is_slide) exit
        fin = fin+dir
      end do
    end do
  end subroutine
  
  subroutine gen_moves()
    integer::ini,piece,piece_color,piece_type
    do ini=0,127
      if(iand(ini,136) == 0 .and. board(ini) /= empty)then
        piece = board(ini)
        piece_color = get_color(piece)
        if(piece_color == team_white .and. state%side == team_white)then
        select case (piece)
          case (wr)
            call gen_common_moves(ini,directions_rook,.TRUE.)
          case (wb)
            call gen_common_moves(ini,directions_bishop,.TRUE.)
          case (wq)
            call gen_common_moves(ini,directions_rook_bishop,.TRUE.)
          case (wk)
            call gen_common_moves(ini,directions_rook_bishop,.FALSE.)
          case (wn)
            call gen_common_moves(ini,directions_knight,.FALSE.)
          case (wp)
            call gen_pawn_moves(ini)
          case default         
        end select
        end if
        if(piece_color == team_black .and. state%side == team_black)then
        select case (piece)
          case (br)
            call gen_common_moves(ini,directions_rook,.TRUE.)
          case (bb)
            call gen_common_moves(ini,directions_bishop,.TRUE.)
          case (bq)
            call gen_common_moves(ini,directions_rook_bishop,.TRUE.)
          case (bk)
            call gen_common_moves(ini,directions_rook_bishop,.FALSE.)
          case (bn)
            call gen_common_moves(ini,directions_knight,.FALSE.)
          case (bp)
            call gen_pawn_moves(ini)
          case default         
        end select
        end if
      end if
    end do
    call gen_castling_moves()
  end subroutine
  
  subroutine make_move(m)
    type(type_move)::m
    
    hist(ply)%cp = state%cp
    hist(ply)%ep = state%ep
    hist(ply)%hash = position_hash
    hist(ply)%move = m
    
    position_hash = ieor(position_hash,hash_table%cp(state%cp))
    state%cp = iand(state%cp,cp_table(m%ini))
    state%cp = iand(state%cp,cp_table(m%fin))
    position_hash = ieor(position_hash,hash_table%cp(state%cp))
    
    position_hash = ieor(position_hash,hash_table%board(board(m%ini),m%ini))
    position_hash = ieor(position_hash,hash_table%board(board(m%fin),m%fin))
    board(m%fin) = board(m%ini)
    board(m%ini) = empty
    position_hash = ieor(position_hash,hash_table%board(board(m%ini),m%ini))
    position_hash = ieor(position_hash,hash_table%board(board(m%fin),m%fin))
    
    position_hash = ieor(position_hash,hash_table%ep)
    if(m%is_pawnstart)then
      if(state%side == team_white)then
        state%ep = m%ini+16
      end if
      if(state%side == team_black)then
        state%ep = m%ini-16
      end if
    else
      state%ep = ob
    end if
    position_hash = ieor(position_hash,hash_table%ep)
    
    
    if(m%is_enpassant)then
      if(state%side == team_white)then
        position_hash = ieor(position_hash,hash_table%board(board(m%fin-16),m%fin-16))
        board(m%fin-16) = empty
        position_hash = ieor(position_hash,hash_table%board(board(m%fin-16),m%fin-16))
      end if
      if(state%side == team_black)then
        position_hash = ieor(position_hash,hash_table%board(board(m%fin+16),m%fin+16))
        board(m%fin+16) = empty
        position_hash = ieor(position_hash,hash_table%board(board(m%fin-16),m%fin-16))
      end if
    end if
    
    
    if(m%is_castling)then
      select case (m%fin)
        case (c1)
          position_hash = ieor(position_hash,hash_table%board(board(a1),a1))
          position_hash = ieor(position_hash,hash_table%board(board(d1),d1))
          board(d1) = board(a1)
          board(a1) = empty
          position_hash = ieor(position_hash,hash_table%board(board(a1),a1))
          position_hash = ieor(position_hash,hash_table%board(board(d1),d1))
        case (g1)
          position_hash = ieor(position_hash,hash_table%board(board(f1),f1))
          position_hash = ieor(position_hash,hash_table%board(board(h1),h1))
          board(f1) = board(h1)
          board(h1) = empty
          position_hash = ieor(position_hash,hash_table%board(board(f1),f1))
          position_hash = ieor(position_hash,hash_table%board(board(h1),h1))
        case (c8)
          position_hash = ieor(position_hash,hash_table%board(board(a8),a8))
          position_hash = ieor(position_hash,hash_table%board(board(d8),d8))
          board(d8) = board(a8)
          board(a8) = empty
          position_hash = ieor(position_hash,hash_table%board(board(a8),a8))
          position_hash = ieor(position_hash,hash_table%board(board(d8),d8))
        case (g8)
          position_hash = ieor(position_hash,hash_table%board(board(f8),f8))
          position_hash = ieor(position_hash,hash_table%board(board(h8),h8))
          board(f8) = board(h8)
          board(h8) = empty  
          position_hash = ieor(position_hash,hash_table%board(board(f8),f8))
          position_hash = ieor(position_hash,hash_table%board(board(h8),h8))
        case default
      end select
    end if
    
    if(m%promotion_pie /= empty)then
      position_hash = ieor(position_hash,hash_table%board(board(m%fin),m%fin))
      board(m%fin) = m%promotion_pie
      position_hash = ieor(position_hash,hash_table%board(board(m%fin),m%fin))
    end if
    
    position_hash = ieor(position_hash,hash_table%side)
    state%side = ieor(1,state%side)

    ply = ply + 1
    moves_list_ind(ply+1) = moves_list_ind(ply)
  end subroutine
  
  subroutine undo_last_move()
    type(type_move)::m
    ply = ply-1
    m = hist(ply)%move
    state%cp = hist(ply)%cp
    state%ep = hist(ply)%ep
    position_hash = hist(ply)%hash
    ! write(*,*)'undo',m
    state%side = ieor(1,state%side)
    
    board(m%ini) = board(m%fin)
    board(m%fin) = empty
    if(m%is_capture)then
      board(m%fin) = m%captured_pie
    end if
    
    if(m%promotion_pie /= empty)then
      if(state%side == team_white)then
        board(m%ini) = wp
      end if
      if(state%side == team_black)then
        board(m%ini) = bp
      end if
    end if
    
    if(m%is_castling)then
      select case (m%fin)
        case (c1)
          board(d1) = empty
          board(a1) = wr
        case (g1)
          board(f1) = empty
          board(h1) = wr
        case (c8)
          board(d8) = empty
          board(a8) = br
        case (g8)
          board(f8) = empty
          board(h8) = br  
        case default
      end select
    end if
  
  if(m%is_enpassant)then
    if(state%side == team_white)then
      board(m%fin-16) = bp
    end if
    if(state%side == team_black)then
      board(m%fin+16) = wp
    end if
  end if

  end subroutine
  
  function is_attacked(tile,attacking_side)
    logical::is_attacked
    integer::tile,attacking_side
    integer,dimension(0:7)::directions
    integer::dir,dir_ind,fin,piece
    logical::is_slide
   
    is_attacked = .FALSE.
    
    directions = directions_rook
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = tile+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_queen_rook(piece))then
          is_attacked = .TRUE.
          return
        end if
        if(piece /= empty)then
          exit
        end if
        fin = fin+dir
      end do
    end do
    
    directions = directions_bishop
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = tile+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_queen_bishop(piece))then
          is_attacked = .TRUE.
          return
        end if
        if(piece /= empty)then
          exit
        end if
        fin = fin+dir
      end do
    end do
    
    directions = directions_knight
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = tile+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_knight(piece))then
          is_attacked = .TRUE.
          return
        end if
        exit
      end do
    end do
    
    directions = directions_rook_bishop
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = tile+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_king(piece))then
          is_attacked = .TRUE.
          return
        end if
        exit
      end do
    end do
    
    if(attacking_side == team_white)then
      if(board(tile-15) == wp .and. iand(tile-15,136) == 0 .or. board(tile-17) == wp .and. iand(tile-17,136) == 0)then
        is_attacked = .TRUE.
          return
      end if
    end if
    
    if(attacking_side == team_black)then
      if(board(tile+15) == bp .and. iand(tile+15,136) == 0 .or. board(tile+17) == bp .and. iand(tile+17,136) == 0)then
        is_attacked = .TRUE.
          return
      end if
    end if
    
  end function

!INPUT/OUTPUT UTILITIES--------------------------------------------------------- 
  subroutine write_board_numbers()
    integer,dimension(0:127)::board_numbers
    integer::ind  
    do ind=0,127
      board_numbers(ind) = ind
    end do
    write(*,*) 'board_numbers'
    write(*,'(A5,16(I4))') ' 1  |',board_numbers(0:16-1)
    write(*,'(A5,16(I4))') ' 2  |',board_numbers(16:32-1)
    write(*,'(A5,16(I4))') ' 3  |',board_numbers(32:48-1)
    write(*,'(A5,16(I4))') ' 4  |',board_numbers(48:64-1)
    write(*,'(A5,16(I4))') ' 5  |',board_numbers(64:80-1)
    write(*,'(A5,16(I4))') ' 6  |',board_numbers(80:96-1)
    write(*,'(A5,16(I4))') ' 7  |',board_numbers(96:112-1)
    write(*,'(A5,16(I4))') ' 8  |',board_numbers(112:128-1)
    write(*,*) '    ---------------------------------'
    write(*,*) '       a   b   c   d   e   f   g   h'
  end subroutine
  
  subroutine write_board_raw()
    write(*,*) 'board'
    write(*,'(A5,16(I4))') ' 1  |',board(0:16-1)
    write(*,'(A5,16(I4))') ' 2  |',board(16:32-1)
    write(*,'(A5,16(I4))') ' 3  |',board(32:48-1)
    write(*,'(A5,16(I4))') ' 4  |',board(48:64-1)
    write(*,'(A5,16(I4))') ' 5  |',board(64:80-1)
    write(*,'(A5,16(I4))') ' 6  |',board(80:96-1)
    write(*,'(A5,16(I4))') ' 7  |',board(96:112-1)
    write(*,'(A5,16(I4))') ' 8  |',board(112:128-1)
    write(*,*) '    ---------------------------------'
    write(*,*) '       a   b   c   d   e   f   g   h'
    write(*,*) 'state%side', state%side
    write(*,*) 'state%cp',state%cp
    write(*,*) 'state%ep', state%ep
    write(*,*) 'position_hash' ,position_hash
  end subroutine
  
  subroutine write_moves()
    character(len=2),parameter,dimension(0:12)::raw2piece_type = &
    & (/ '[]','wp','wn','wb','wr','wq','wk','bp','bn','bb','br','bq','bk' /)
    integer::m_ind
    write(*,'(I4,A7,I8,A3,I8)') moves_list_ind(ply+1)-moves_list_ind(ply), &
    & ' moves ',moves_list_ind(ply),' to',moves_list_ind(ply+1)-1
    write(*,*) '  ind | piece,ini,fin | ini,fin,capture,promo,start,ep,castle,captured:'
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      write(*,'(I8,A4,A3,A3,I12,I4,L2,I4,L2,L2,L2,I4)') m_ind, &
      & raw2piece_type(board(moves_list(m_ind)%ini)), &
      & raw2algebraic(moves_list(m_ind)%ini), &
      & raw2algebraic(moves_list(m_ind)%fin), &
      & moves_list(m_ind)
    end do
  end subroutine
  
  function algebraic2raw(tile)
    character(len=2)::tile
    integer::algebraic2raw
    do algebraic2raw=0,127
      if(raw2algebraic(algebraic2raw) == tile)exit
    end do
  end function
  
  function get_fen()
    character(len=9*8+2+5+3+3+4)::get_fen
    integer::emptys,tile
    character(len=1),dimension(1:12):: fen_piece = (/'P','N','B','R','Q','K','p','n','b','r','q','k'/)
    character(len=1),dimension(0:8):: fen_sq = (/ ' ','1','2','3','4','5','6','7','8' /)
    character(len=3),dimension(0:1):: fen_side = (/ ' w ',' b '/)
    character(len=4),dimension(0:15):: int2fen_cp = (/ &
    &'    ','K   ',' Q  ','KQ  ','  k ','K k ',' Qk ','KQk ','   q','K  q',' Q q','KQ q','  kq','K kq',' Qkq','KQkq' /)
    get_fen = ''
    !fen board
    emptys = 0
    do tile=0,119
      if(iand(tile,136) == 0)then
        if(board(tile) == empty)then
          emptys = emptys + 1
        else
          get_fen = trim(get_fen)//trim(fen_sq(emptys))//fen_piece(board(tile))
        end if
      end if
      if(iand(tile,8) /= 0 .and. iand(tile,7) == 0)then
          get_fen = trim(get_fen)//trim(fen_sq(emptys))//'/'
          emptys = 0
      end if
    end do
    get_fen = trim(get_fen)//trim(fen_sq(emptys))
    !fen side,castling,enpassant,50move,ply
    get_fen = trim(get_fen)//fen_side(state%side)//trim(int2fen_cp(state%cp))//raw2algebraic(state%ep)
    ! get_fen = trim(get_fen)//ply
  end function
  
  subroutine set_fen(fen_in) 
    character(*)::fen_in
    character(len=90)::fen
    character(len=72)::fen_board,fen_board_expanded
    character(len=8),dimension(0:7)::fen_row
    character(len=8)::fen_row_r
    character(len=6)::fen_side,fen_cp,fen_ep
    character(len=8),dimension(1:8)::fen_empty = &
    & (/'-       ','--      ','---     ','----    ','-----   ','------  ','------- ','--------'/)
    character(len=1),dimension(0:8):: int2char = (/ ' ','1','2','3','4','5','6','7','8' /)
    character(len=1),dimension(0:12)::pie = (/'-','P','N','B','R','Q','K','p','n','b','r','q','k'/)
    character(len=4),dimension(0:15):: int2fen_cp = (/ &
    &'    ','K   ','Q   ','KQ  ','k   ','Kk  ','Qk  ','KQk ','q   ','Kq  ','Qq  ','KQq ','kq  ','Kkq ','Qkq ','KQkq' /)
    integer::row_ind,col_ind,pie_ind,fen_board_ind,tile,emptys,fen_cp_ind,ep_ind,cp_ind
    logical::is_empty
    
    fen = trim(fen_in)
    
    !parse fen
    fen_board = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:90)
    fen_side = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:90)
    fen_cp = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:90)
    fen_ep = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:90)
      
    !expand fen_board
    fen_board_expanded = ''
    do fen_board_ind = 1,90
      is_empty = .false.
      do emptys = 1,8
        if(fen_board(fen_board_ind:fen_board_ind) == int2char(emptys))then
          is_empty = .true.
          fen_board_expanded = trim(fen_board_expanded)//fen_empty(emptys)
          exit
        end if
      end do
      if(.not. is_empty)then
        fen_board_expanded = trim(fen_board_expanded)//fen_board(fen_board_ind:fen_board_ind)
      end if
    end do
    ! write(*,*)fen_board_expanded
   
    !parse fen_board_expanded into fen_row
    do row_ind=0,6
      fen_row(row_ind) = fen_board_expanded(1:index(fen_board_expanded,'/')-1)
      fen_board_expanded = fen_board_expanded(index(fen_board_expanded,'/')+1:90)
      ! write(*,*) fen_row(row_ind)
    end do
    fen_row(7) = fen_board_expanded
    ! write(*,*) fen_row(7)
    
    !read fen_row to set board
    do row_ind = 0,7
      fen_row_r = fen_row(7-row_ind)
      do col_ind = 0,7
        tile = 16*row_ind+col_ind
        do pie_ind = 0,12
          if(pie(pie_ind) == fen_row_r(col_ind+1:col_ind+1))then
            ! write(*,*) tile,pie_ind
            board(tile) = pie_ind
          end if
        end do
      end do
    end do
    
    !side cp and ep
    if(fen_side == 'w') state%side=team_white
    if(fen_side == 'b') state%side=team_black
    
    do cp_ind = 0,15
      if(fen_cp == int2fen_cp(cp_ind))then
        state%cp = cp_ind
        exit
      end if
    end do
    
    do ep_ind = -1,127
      if(fen_ep == raw2algebraic(ep_ind))then
        state%ep = ep_ind
        exit
      end if
    end do
    
  end subroutine
  
!TESTS -------------------------------------------------------------------------  
  subroutine test_common_moves()
    integer::ini
    call write_board_numbers
    board = empty
    write(*,*) 'select starting tile'
    read*, ini
    write(*,'(A,I4)') 'moves for a rook in',ini
    call restart_game()
    board = empty
    call gen_common_moves(ini,directions_rook,.TRUE.)
    call write_moves()
    write(*,'(A,I4)') 'moves for a bishop in',ini
    call restart_game()
    board = empty
    call gen_common_moves(ini,directions_bishop,.TRUE.)
    call write_moves()
    write(*,'(A,I4)') 'moves for a queen in',ini
    call restart_game()
    board = empty
    call gen_common_moves(ini,directions_rook_bishop,.TRUE.)
    call write_moves()
    write(*,'(A,I4)') 'moves for a king in',ini
    call restart_game()
    board = empty
    call gen_common_moves(ini,directions_rook_bishop,.FALSE.)
    call write_moves()
    write(*,'(A,I4)') 'moves for a knight in',ini
    call restart_game()
    board = empty
    call gen_common_moves(ini,directions_knight,.FALSE.)
    call write_moves()
  end subroutine
  
  subroutine test_gen_moves_nopawns()
    call write_board_numbers 
    call restart_game()
    board = empty
    board(a1:h1) = (/wr,wn,wb,wq,wk,wb,wn,wr/)
    board(a8:h8) = (/br,bn,bb,bq,bk,bb,bn,br/)
    call write_board_raw 
    call gen_moves()
    call write_moves()
  end subroutine
  
  subroutine test_do_undo_nopawns()
    integer::m
    call write_board_numbers
    call restart_game()
    board = empty
    board(a1:h1) = (/wr,wn,wb,wq,wk,wb,wn,wr/)
    board(a8:h8) = (/br,bn,bb,bq,bk,bb,bn,br/)
    call write_board_raw
    call gen_moves()
    do m = moves_list_ind(ply),moves_list_ind(ply+1)-1
      call make_move(moves_list(m))
      call undo_last_move()
    end do
    call write_board_raw
  end subroutine
  
  subroutine test_do_undo_wp()
    integer::m
    call write_board_numbers
    call restart_game()
    board = empty
    board(a2:h2) = wp
    call write_board_raw
    call gen_moves()
    do m = moves_list_ind(ply),moves_list_ind(ply+1)-1
      call make_move(moves_list(m))
      call undo_last_move()
    end do
    call write_board_raw
  end subroutine
  
  subroutine test_do_undo_bp()
    integer::m
    call write_board_numbers
    call restart_game()
    state%side = team_black
    board = empty
    board(a7:h7) = bp
    call write_board_raw
    call gen_moves()
    do m = moves_list_ind(ply),moves_list_ind(ply+1)-1
      call make_move(moves_list(m))
      call undo_last_move()
    end do
    call write_board_raw
  end subroutine
  
  subroutine test_set_fen()
  integer,dimension(0:127)::cp_board
  integer::cp_castling_permissions,cp_enpassant
  type(type_state)::cp_state
  !set manually
  state%side = team_white
  state%cp = cp_all
  state%ep = ob
  board = empty
  board(a1:h1) = (/wr,wn,wb,wq,wk,wb,wn,wr/)
  board(a2:h2) = wp
  board(a7:h7) = bp
  board(a8:h8) = (/br,bn,bb,bq,bk,bb,bn,br /)
  !copy
  cp_state%side = state%side
  cp_state%cp = state%cp
  cp_enpassant = state%ep
  cp_board = board
  !set fen and compare
  call set_fen('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 ')
  if(cp_state%side == state%side .and. &
  & cp_state%cp == state%cp .and. &
  & cp_enpassant == state%ep .and. &
  & ALL( cp_board == board ))then
    write(*,*) 'PASS ','fen starting position'
  else
    write(*,*) 'NO PASS ','fen starting position'
    call write_board_raw()
  end if
  
  !set manually
  state%side = team_black
  state%cp = cp_all-cp_bk-cp_bq
  state%ep = 32
  board = empty
  board(a1:h1) = (/wr,wn,wb,wq,wk,wb,wn,wr/)
  board(a2:h2) = wp
  board(a7:h7) = bp
  board(a8:h8) = (/br,bn,bb,bq,bk,bb,bn,br /)
  !copy
  cp_state%side = state%side
  cp_state%cp = state%cp
  cp_enpassant = state%ep
  cp_board = board
  !set fen and compare
  call set_fen('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQ a3 0')
  if(cp_state%side == state%side .and. &
  & cp_state%cp == state%cp .and. &
  & cp_enpassant == state%ep .and. &
  & ALL( cp_board == board ))then
    write(*,*) 'PASS ','fen starting position b KQ c1'
  else
    write(*,*) 'NO PASS ','fen starting position b KQ c1'
    call write_board_raw()
  end if
  
  end subroutine
  
  recursive function perft(depth) result(res)
    integer::res
    integer::depth,cnt,m_ind
    type(type_move)::m

    cnt = 0
    if(depth == 0) then
      res = 1
      ! m=hist(ply-1)%move
      ! if(m%is_capture) perft_results%capture = perft_results%capture + 1
      ! if(m%is_castling) perft_results%castle = perft_results%castle + 1
      ! if(m%is_enpassant) perft_results%enpassant = perft_results%enpassant + 1
      ! if(m%promotion_pie /= empty) perft_results%promotion = perft_results%promotion + 1
      return
    end if

    call gen_moves()
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      call make_move(moves_list(m_ind))
      cnt = cnt + perft(depth-1)
      call undo_last_move()
    end do
    res = cnt
  end function
  
  subroutine perft_restart()
    perft_results%capture = 0
    perft_results%enpassant = 0
    perft_results%castle = 0
    perft_results%promotion = 0
  end subroutine
  
  subroutine perft_handler
    integer::depth
    integer,dimension(1:5)::perft_starting_nodes = (/ 20,400,8902,197281,4865609 /)
    integer,dimension(1:5)::perft_kiwipete_nodes = (/ 48,2039,97862,4085603,193690690 /)
    integer,dimension(1:6)::perft_position3_nodes = (/ 14,191,2812,43238,674624,11030083 /)
    integer,dimension(1:5)::perft_position4_nodes = (/ 6,264,9467,422333,15833292 /)
    integer,dimension(1:5)::perft_position5_nodes = (/ 44,1486,62379,2103487,89941194 /)
    real::time_ini,time_fin
    
    call cpu_time(time_ini)

    write(*,*) 'perft starting position'
    call set_fen('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0')     
    do depth = 1,5
      call restart_move_list()
      call perft_restart()
      write(*,*) perft(depth),perft_starting_nodes(depth)
    end do        
        
    write(*,*) 'perft kiwipete'
    call set_fen('r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0')
    do depth = 1,4
      call restart_move_list()
      call perft_restart()
      write(*,*) perft(depth),perft_kiwipete_nodes(depth)
    end do 
    
    write(*,*) 'perft position3'
    call set_fen('8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - 0')      
    do depth = 1,5
      call restart_move_list()
      write(*,*) perft(depth),perft_position3_nodes(depth)
    end do 
    
    write(*,*) 'perft position4'
    call set_fen('r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0')      
    do depth = 1,5
      call restart_move_list()
      write(*,*) perft(depth),perft_position4_nodes(depth)
    end do 
      
    write(*,*) 'perft position5'
    call set_fen('rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 0')      
    do depth = 1,4
      call restart_move_list()
      write(*,*) perft(depth),perft_position5_nodes(depth)
    end do 
    
    call cpu_time(time_fin)
    write(*,*)'time: ',time_fin-time_ini !9.65
        
  end subroutine
  
  end program lostchess
