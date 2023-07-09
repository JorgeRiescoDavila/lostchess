
program lostchess
  implicit none
  integer,parameter::max_moves_per_game = 1024
  integer,parameter::max_moves_listed = 8192
  !side to move codes
  integer,parameter::team_white = 0
  integer,parameter::team_black = 1
  integer,parameter::team_none = 2
  integer,parameter::team_both = 3
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
  logical,parameter,dimension(0:12)::is_pawn = (/ .false., &
  & .true.,.false.,.false.,.false.,.false.,.false., &
  & .true.,.false.,.false.,.false.,.false.,.false. /)
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
  integer,parameter::cp_wk = 1
  integer,parameter::cp_wq = 2
  integer,parameter::cp_bk = 4
  integer,parameter::cp_bq = 8
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
    integer,dimension(0:127)::ep
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
    integer::rule50
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
    integer::rule50
    integer::hash
    type(type_move)::move
  end type
  type(type_history),dimension(0:max_moves_per_game-1)::hist

  !moves list
  integer,dimension(0:max_moves_per_game-1)::moves_list_ind
  type(type_move),dimension(0:max_moves_listed-1)::moves_list
  integer,dimension(0:max_moves_listed-1)::moves_score
  
  !piece list
  type type_piece_list
    integer,dimension(0:1)::kings
  end type
  type(type_piece_list)::piece_list 
  
  !search engine
  integer,parameter::inf = 2**30
  integer,parameter::mate_score = 1000000
  integer,parameter::ulti_depth = 32
  !search engine
  type type_search
    !constants
    integer,dimension(0:12,0:127)::piece_sq
    integer,dimension(0:12)::piece_value
    !variabless
    integer::ss_material(0:1) 
    integer::ss_position(0:1)
    integer::calling_depth
    integer::best_score
    type(type_move)::best_move
    type(type_move),dimension(0:1,0:ulti_depth)::killers
    ! statistics
    integer::nodes
    real::t_ini
    real::t_cur
  end type
  type(type_search)::srch
  integer::ply_depth

  integer,parameter::TT_size = 2**10
  integer,dimension(0:TT_size-1,0:2)::TT

  !testing variables
  integer::selected_option,winner  
  
  !init program
  call init()

  do while (.true.)
    write(*,*) 'select options -----------------------------------------------------------------'
    write(*,*) '1 PVP | 2 PVE | 3 PerfTest'
    write(*,*) '--------------------------------------------------------------------------------'
    read*, selected_option
    select case (selected_option)
      case (1)
        call restart_game()
        do while(.true.)
          call player_handler()
          winner = check_end()
          if(winner /= team_none)then
            write(*,*)'WINNER ',winner
            exit
          end if
        end do
      case (2)
        call restart_game()      
        do while(.true.)
          call search_handler1(6)
          call make_move(srch%best_move)
          winner = check_end()
          if(winner /= team_none)then
            write(*,*)'WINNER ',winner
            exit
          end if
          call player_handler()
          winner = check_end()
          if(winner /= team_none)then
            write(*,*)'WINNER ',winner
            exit
          end if
        end do
      case (3)
        call restart_game()
        call perft_handler()
      case default
        exit
    end select
  end do
  
  contains

!RESTART

  subroutine init()
    call init_hash_table()
    call init_search_const()
  end subroutine

  subroutine init_hash_table()
    integer::sq,pie,cp_ind
    hash_table%board(0,0:127) = 0
    do pie = 1,12
      do sq = 0,127
        hash_table%board(pie,sq) = floor(abs(sin(sq+pie*128+1.1)*2**30))
      end do
    end do
    hash_table%side = floor(abs(sin(1664+1.1)*2**30))
    do sq = 0,127
      hash_table%ep(sq) = floor(abs(sin(1665+sq+1.1)*2**30))
    end do
    do cp_ind = 0,15
      hash_table%cp(cp_ind) = floor(abs(sin(cp_ind+1792+1.1)*2**30))
    end do
  end subroutine

  subroutine restart_game()
    !restart board
    board = empty
    board(a1:h1) = (/wr,wn,wb,wq,wk,wb,wn,wr/)
    board(a2:h2) = wp
    board(a7:h7) = bp
    board(a8:h8) = (/br,bn,bb,bq,bk,bb,bn,br /)
    !restart state
    state%side = team_white
    state%cp = cp_all
    state%ep = ob
    state%rule50 = 0
    !hash board and state
    position_hash = hash_position()
    !restart moves done
    ply = 0
    !restart history
    hist%cp = cp_all
    hist%ep = ob
    hist%rule50 = 0
    hist%move = move_null
    !reastart list of moves
    moves_list_ind = 0
    moves_list = move_null
    !restart piece list
    piece_list%kings = (/ e1,e8 /)
  end subroutine
  
  function hash_position() result(hash)
    integer::hash,sq,pie
    hash = 0
    do sq = 0,127
      if(iand(sq,136) == 0)then
        pie = board(sq)
        hash = ieor(hash,hash_table%board(pie,sq))
      end if
    end do
    hash = ieor(hash,hash_table%side)
    hash = ieor(hash,hash_table%ep(state%ep))
    hash = ieor(hash,hash_table%cp(state%cp))
  end function

!END

  function check_end() result(winner)
    integer::winner,rule50_ind,reps,king_sq
    winner = team_none
    reps = 1
    !50 move rule
    if(state%rule50 == 100)then
      winner = team_both
      return
    end if
    !threefold repetition
    do rule50_ind = 1,state%rule50
      if(hist(ply-rule50_ind)%hash == position_hash)then
        reps = reps+1
        if(reps == 3)then
          winner = team_both
          return
        end if
      end if
    end do
    !checkmate/stalemate
    call gen_moves()
    ! call write_moves()
    if(moves_list_ind(ply) == moves_list_ind(ply+1))then
      king_sq = piece_list%kings(state%side)
      if(is_attacked(king_sq,ieor(1,state%side)))then
        winner = ieor(1,state%side)
        return
      else
        winner = team_both
        return
      end if
    end if
  end function

!MOVE GENERATOR

  function get_row(sq) result(row)
    integer::sq,row
    row = ishft(sq,-4)
  end function

  function there_is_enemy_on(sq)
    logical::there_is_enemy_on
    integer::sq
    there_is_enemy_on = get_color(board(sq)) .eq. ieor(1,state%side)
  end function

  subroutine add_move(ini,fin,is_capture,promotion_pie,is_pawnstart,is_enpassant,is_castling,captured_pie)
    type(type_move)::m
    integer::ini,fin,promotion_pie,captured_pie,king_sq
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
    
    ! call make_move(m)
    ! king_sq = piece_list%kings(ieor(1,state%side))
    ! if(.not. is_attacked(king_sq,state%side))then
      ! is_legal =.true.
    ! end if
    ! call undo_last_move()
    
    ! if(is_legal)then
      moves_list(moves_list_ind(ply+1)) = m
      moves_list_ind(ply+1) = moves_list_ind(ply+1) + 1
    ! end if
  end subroutine
  
  subroutine gen_pawn_moves(ini)
    integer::ini,move_direction,fin,promotion_pie,d_ind
    integer::last_row,start_row
    integer::promo_q,promo_r,promo_b,promo_n
    integer,dimension(0:1)::capture_directions
    
    if(state%side == team_white)then
      move_direction = 16
      capture_directions = (/15,17/)
      last_row = 7
      start_row = 1
      promo_q = wq
      promo_r = wr
      promo_b = wb
      promo_n = wn
    end if
    
    if(state%side == team_black)then
      move_direction = -16
      capture_directions = (/-15,-17/)
      last_row = 0
      start_row = 6
      promo_q = bq
      promo_r = br
      promo_b = bb
      promo_n = bn
    end if
    
    do d_ind=0,1
      fin = ini+capture_directions(d_ind)
      if(iand(fin,136) == 0)then
        if(there_is_enemy_on(fin))then
          if(get_row(fin) == last_row)then
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
        if(get_row(fin) == last_row)then
          call add_move(ini,fin,.TRUE.,promo_q,.FALSE.,.FALSE.,.FALSE.,board(fin))
          call add_move(ini,fin,.TRUE.,promo_r,.FALSE.,.FALSE.,.FALSE.,board(fin))
          call add_move(ini,fin,.TRUE.,promo_b,.FALSE.,.FALSE.,.FALSE.,board(fin))
          call add_move(ini,fin,.TRUE.,promo_n,.FALSE.,.FALSE.,.FALSE.,board(fin))
        else
          call add_move(ini,fin,.FALSE.,empty,.FALSE.,.FALSE.,.FALSE.,empty)
          fin = ini+2*move_direction
          if(get_row(ini) == start_row .and. board(fin)==empty)then
            call add_move(ini,fin,.FALSE.,empty,.TRUE.,.FALSE.,.FALSE.,empty)
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
        end if
      end if
      if(iand(state%cp,cp_wk) /= 0)then
        if(board(g1)==empty .and. board(f1)==empty) then
          if( .not. is_attacked(e1,team_black) .and. .not. is_attacked(f1,team_black))then
            call add_move(e1,g1,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        end if
      end if
    end if
    if(state%side == team_black .and. board(e8)==bk)then
      if(iand(state%cp,cp_bq) /= 0)then
        if(board(b8)==empty .and. board(c8)==empty .and. board(d8)==empty) then 
          if( .not. is_attacked(e8,team_white) .and. .not. is_attacked(d8,team_white))then
            call add_move(e8,c8,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        end if
      end if
      if(iand(state%cp,cp_bk) /= 0)then
        if(board(g8)==empty .and. board(f8)==empty) then
          if( .not. is_attacked(e8,team_white) .and. .not. is_attacked(f8,team_white))then
            call add_move(e8,g8,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        end if
      end if
    end if
  end subroutine

  subroutine gen_common_moves(ini,directions,is_slide)
    integer,intent(in)::ini
    integer,dimension(0:7)::directions
    integer::dir,dir_ind,fin
    logical::is_slide
    do dir_ind = 0,7
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
    integer::ini,pie,col
    moves_list_ind(ply+1) = moves_list_ind(ply)
    do ini = 0,127
      if(iand(ini,136) == 0 .and. board(ini) /= empty)then
        pie = board(ini)
        col = get_color(pie)
        if(col == state%side)then
          if(is_queen_bishop(pie))then
            call gen_common_moves(ini,directions_bishop,.TRUE.)
          end if
          if(is_queen_rook(pie))then
            call gen_common_moves(ini,directions_rook,.TRUE.)
          end if
          if(is_knight(pie))then
            call gen_common_moves(ini,directions_knight,.FALSE.)
          end if
          if(is_king(pie))then
            call gen_common_moves(ini,directions_rook_bishop,.FALSE.)
          end if
          if(is_pawn(pie))then
            call gen_pawn_moves(ini)
          end if
        end if
      end if
    end do
    call gen_castling_moves()
  end subroutine
  
  subroutine gen_pawn_good(ini)
    integer::ini,move_direction,fin,promotion_pie,d_ind
    integer::last_row,start_row,promo_q
    integer,dimension(0:1)::capture_directions
    
    if(state%side == team_white)then
      move_direction = 16
      capture_directions = (/15,17/)
      last_row = 7
      start_row = 1
      promo_q = wq
    end if
    
    if(state%side == team_black)then
      move_direction = -16
      capture_directions = (/-15,-17/)
      last_row = 0
      start_row = 6
      promo_q = bq
    end if
    
    do d_ind=0,1
      fin = ini+capture_directions(d_ind)
      if(iand(fin,136) == 0)then
        if(there_is_enemy_on(fin))then
          if(get_row(fin) == last_row)then
            call add_move(ini,fin,.TRUE.,promo_q,.FALSE.,.FALSE.,.FALSE.,board(fin))
          else      
            call add_move(ini,fin,.TRUE.,empty,.FALSE.,.FALSE.,.FALSE.,board(fin))
          end if
        end if
      end if
    end do
    fin = ini+move_direction
    if(iand(fin,136) == 0)then
      if(board(fin) == empty)then
        if(get_row(fin) == last_row)then
          call add_move(ini,fin,.TRUE.,promo_q,.FALSE.,.FALSE.,.FALSE.,board(fin))
        end if
      end if
    end if
  end subroutine
  
  subroutine gen_common_good(ini,directions,is_slide)
    integer,intent(in)::ini
    integer,dimension(0:7)::directions
    integer::dir,dir_ind,fin
    logical::is_slide
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = ini+dir
      do while (iand(fin,136) == 0)
        if(board(fin) == empty)then
        else if(there_is_enemy_on(fin)) then
          if(.not. is_pawn(board(fin)))then
            call add_move(ini,fin,.TRUE.,empty,.FALSE.,.FALSE.,.FALSE.,board(fin))
          end if  
          exit
        else
          exit
        end if
        if(.not. is_slide) exit
        fin = fin+dir
      end do
    end do
  end subroutine
  
  subroutine gen_good_moves()
    integer::ini,pie,col
    moves_list_ind(ply+1) = moves_list_ind(ply)
    do ini = 0,127
      if(iand(ini,136) == 0 .and. board(ini) /= empty)then
        pie = board(ini)
        col = get_color(pie)
        if(col == state%side)then
          if(is_queen_bishop(pie))then
            call gen_common_good(ini,directions_bishop,.TRUE.)
          end if
          if(is_queen_rook(pie))then
            call gen_common_good(ini,directions_rook,.TRUE.)
          end if
          if(is_knight(pie))then
            call gen_common_good(ini,directions_knight,.FALSE.)
          end if
          if(is_king(pie))then
            call gen_common_good(ini,directions_rook_bishop,.FALSE.)
          end if
          if(is_pawn(pie))then
            call gen_pawn_good(ini)
          end if
        end if
      end if
    end do
  end subroutine
  
  subroutine make_move(m)
    type(type_move)::m
    
    hist(ply)%cp = state%cp
    hist(ply)%ep = state%ep
    hist(ply)%rule50 = state%rule50
    hist(ply)%hash = position_hash
    hist(ply)%move = m
    
    if(m%is_capture) state%rule50 = 0
    if(is_pawn(board(m%ini))) state%rule50 = 0
    
    if(is_king(board(m%ini)))then
      if(get_color(board(m%ini)) == team_white)then
        piece_list%kings(team_white) = m%fin
      end if
      if(get_color(board(m%ini)) == team_black)then
        piece_list%kings(team_black) = m%fin
      end if
    end if
    
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
    
    position_hash = ieor(position_hash,hash_table%ep(state%ep))
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
    position_hash = ieor(position_hash,hash_table%ep(state%ep))
    
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
    state%rule50 = hist(ply)%rule50
    position_hash = hist(ply)%hash
    
    if(is_king(board(m%fin)))then
      if(get_color(board(m%fin)) == team_white)then
        piece_list%kings(team_white) = m%ini
      end if
      if(get_color(board(m%fin)) == team_black)then
        piece_list%kings(team_black) = m%ini
      end if
    end if
    
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
  
  function is_attacked(sq,attacking_side)
    logical::is_attacked
    integer::sq,attacking_side
    integer,dimension(0:7)::directions
    integer::dir,dir_ind,fin,piece
    logical::is_slide
   
    is_attacked = .FALSE.
    
    directions = directions_rook
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = sq+dir
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
      fin = sq+dir
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
      fin = sq+dir
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
      fin = sq+dir
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
      if(board(sq-15) == wp .and. iand(sq-15,136) == 0 .or. board(sq-17) == wp .and. iand(sq-17,136) == 0)then
        is_attacked = .TRUE.
          return
      end if
    end if
    
    if(attacking_side == team_black)then
      if(board(sq+15) == bp .and. iand(sq+15,136) == 0 .or. board(sq+17) == bp .and. iand(sq+17,136) == 0)then
        is_attacked = .TRUE.
          return
      end if
    end if
    
  end function

!INPUT/OUTPUT UTILITIES

  subroutine mirror(board)
    integer,dimension(0:127)::board
    integer,dimension(0:7,0:15)::rows
    integer::row 
    do row=0,7
      rows(row,0:15) = board(16*row:16*row+15)
    end do
    do row=0,7
      board(16*row:16*row+15) = rows(7-row,0:15)
    end do
  end subroutine

  subroutine write_board_numbers()
    integer,dimension(0:127)::board
    integer::ind,row 
    do ind=0,127
      board(ind) = ind
    end do
    write(*,*) 'board_numbers'
    write(*,*) '    ---------------------------------'
    do row=0,7
      write(*,'(A1,I1,A3,16(I4))') ' ',row+1,'  |',board(16*row:16*row+15)
    end do
    write(*,*) '    ---------------------------------'
    write(*,*) '       a   b   c   d   e   f   g   h'
  end subroutine
  
  subroutine write_board_raw()
    integer::row
    write(*,*) 'board'
    do row=0,7
      write(*,'(A1,I1,A3,16(I4))') ' ',row+1,'  |',board(16*row:16*row+15)
    end do
    write(*,*) '    ---------------------------------'
    write(*,*) '       a   b   c   d   e   f   g   h'
    write(*,*) 'state%side', state%side
    write(*,*) 'state%cp',state%cp
    write(*,*) 'state%ep', state%ep
    write(*,*) 'position_hash' ,position_hash
  end subroutine
  
  subroutine write_board_pretty()
    integer::row,sq
    character(len=1),dimension(0:127)::board_pie
    character(len=1),dimension(0:12):: fen_piece = (/'-','P','N','B','R','Q','K','p','n','b','r','q','k'/)
    do sq=0,127
      board_pie(sq) = fen_piece(board(sq))
    end do
    write(*,*) 'board'
    do row=7,0,-1
      write(*,'(A1,I1,A3,8(A4))') ' ',row+1,'  |',board_pie(16*row:16*row+7)
    end do
    write(*,*) '    ---------------------------------'
    write(*,*) '       a   b   c   d   e   f   g   h'
  end subroutine
  
  subroutine write_board(board)
    integer,dimension(0:127)::board
    integer::row
    write(*,*) '    ---------------------------------'
    do row=0,7
      write(*,'(A1,I1,A3,16(I4))') ' ',row+1,'  |',board(16*row:16*row+15)
    end do
    write(*,*) '    ---------------------------------'
    write(*,*) '       a   b   c   d   e   f   g   h'    
  end subroutine
  
  function raw2alg(sq) result(alg)
    character(len=2)::alg
    integer::sq
    character(len=2),parameter,dimension(-1:127)::raw2alg_table = (/ '- ', & 
      &'a1','b1','c1','d1','e1','f1','g1','h1','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a2','b2','c2','d2','e2','f2','g2','h2','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a3','b3','c3','d3','e3','f3','g3','h3','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a4','b4','c4','d4','e4','f4','g4','h4','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a5','b5','c5','d5','e5','f5','g5','h5','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a6','b6','c6','d6','e6','f6','g6','h6','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a7','b7','c7','d7','e7','f7','g7','h7','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a8','b8','c8','d8','e8','f8','g8','h8','ob','ob','ob','ob','ob','ob','ob','ob' /)
    alg = raw2alg_table(sq)
  end function
  
  function alg2raw(alg) result(sq)
    character(len=2)::alg
    integer::sq
    do sq=0,127
      if(raw2alg(sq) == alg)exit
    end do
  end function
  
  function move2alg(m) result(alg)
    type(type_move)::m
    character(len=5)::alg
    alg = raw2alg(m%ini)//raw2alg(m%fin)//' '
  end function
  
  function alg2move(alg) result(m)
    type(type_move)::m
    character(len=5)::alg
    integer::ini,fin,m_ind
    ini=alg2raw(alg(1:2))
    fin=alg2raw(alg(3:4))
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m = moves_list(m_ind)
      if(m%ini == ini .and. m%fin == fin) return
    end do
    m = move_null
  end function
  
  subroutine write_moves()
    integer::m_ind
    type(type_move)::m
    write(*,*)'List of moves:'
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m=moves_list(m_ind)
      write(*,'(A4,I8,A1,I4,I4,L2,I4,L2,L2,L2,I4)')move2alg(m),m_ind,'|',m
    end do
  end subroutine
  
  subroutine write_piece_value()
    integer::pie
    do pie=1,12
      write(*,*)pie
      call write_board(srch%piece_sq(pie,0:127))
    end do
  end subroutine
  
  function get_fen()
    character(len=9*8+2+5+3+3+4)::get_fen
    integer::emptys,tile
    character(len=1),dimension(0:12):: fen_piece = (/'-','P','N','B','R','Q','K','p','n','b','r','q','k'/)
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
    get_fen = trim(get_fen)//fen_side(state%side)//trim(int2fen_cp(state%cp))//raw2alg(state%ep)
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
    integer::row_ind,col_ind,pie_ind,fen_board_ind,tile,emptys,fen_cp_ind,ep_ind,cp_ind,sq_king
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
      if(fen_ep == raw2alg(ep_ind))then
        state%ep = ep_ind
        exit
      end if
    end do
    
    do sq_king = 0,127
      if(is_king(board(sq_king)))then
        if(get_color(board(sq_king)) == team_white)then
          piece_list%kings(team_white) = sq_king
        end if
        if(get_color(board(sq_king)) == team_black)then
          piece_list%kings(team_black) = sq_king
        end if
      end if
    end do
    
    position_hash = hash_position()
    
  end subroutine
  
!TESTS
  
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
    
  recursive function perft(depth) result(nodes)
    integer::nodes,depth,m_ind,king_sq,delta_nodes
    nodes = 0
    if(depth == 0) then
      nodes = 1
      return
    end if
    ! if(TT(mod(position_hash,TT_size),0) ==  position_hash .and. TT(mod(position_hash,TT_size),1) == depth)then
      ! nodes = TT(mod(position_hash,TT_size),2)
      ! return
    ! end if
    call gen_moves()
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      call make_move(moves_list(m_ind))
        king_sq = piece_list%kings(ieor(1,state%side))
        if(.not. is_attacked(king_sq,state%side))then
          delta_nodes = perft(depth-1)
          nodes = nodes + delta_nodes
        end if
      call undo_last_move()
    end do
    ! TT(mod(position_hash,TT_size),0) = position_hash
    ! TT(mod(position_hash,TT_size),1) = depth
    ! TT(mod(position_hash,TT_size),2) = delta_nodes
  end function
  
  subroutine perft_handler
    type type_position
      character(len=20)::desc
      character(len=100)::fen
      integer,dimension(1:6)::nodes
    end type
    type(type_position),dimension(1:5)::positions
    integer::p_ind,depth,nodes,hash_ini
    real::time_ini,time_fin
    
    positions(1)%desc = 'starting position'
    positions(1)%fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0'
    positions(1)%nodes = (/ 20,400,8902,197281,4865609,119060324 /)
    
    positions(2)%desc = 'kiwipete'
    positions(2)%fen = 'r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0'
    positions(2)%nodes = (/ 48,2039,97862,4085603,193690690,inf /)
    
    positions(3)%desc = 'perft position 3'
    positions(3)%fen = '8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - 0'
    positions(3)%nodes = (/ 14,191,2812,43238,674624,11030083 /)
    
    positions(4)%desc = 'perft position 4'
    positions(4)%fen = 'r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0'
    positions(4)%nodes = (/ 6,264,9467,422333,15833292,inf /)
    
    positions(5)%desc = 'perft position 5'
    positions(5)%fen = 'rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 0'
    positions(5)%nodes = (/ 44,1486,62379,2103487,89941194,inf /)
    
    call cpu_time(time_ini)
    
    do p_ind = 1,5
      write(*,*) positions(p_ind)%desc
      call set_fen(positions(p_ind)%fen)
      hash_ini = hash_position()
      do depth = 1,6
        if(positions(p_ind)%nodes(depth) < 12000000)then
          nodes = perft(depth)
          write(*,*) nodes,positions(p_ind)%nodes(depth),nodes==positions(p_ind)%nodes(depth)
        end if
      end do
      write(*,*)'unchanged hash',hash_ini==position_hash
    end do
    
    call cpu_time(time_fin)
    write(*,*)'time: ',time_fin-time_ini
    
  end subroutine

!PLAYER

  subroutine player_handler()
    integer::m_ind = -1
    type(type_move)::m=move_null
    character(len=5)::alg
    call write_board_pretty()
    call gen_moves()
    write(*,*) 'write move in long algebraic'
    do while(.true.)
      read*, alg
      m = alg2move(alg)
      if(.not. equal_m(m,move_null))exit
    end do
    call make_move(m)
  end subroutine

!SEARCH ENGINE

  subroutine init_search_const()
    integer::p_ind
    srch%piece_value = (/ 0,100,310,320,500,950,100000,100,310,320,500,950,100000 /)
  
    srch%piece_sq(wp,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      & 50, 50, 50, 50, 50, 50, 50, 50, 0,0,0,0,0,0,0,0, &
      & 10, 10, 20, 30, 30, 20, 10, 10, 0,0,0,0,0,0,0,0, &
      &  5,  5, 10, 25, 25, 10,  5,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0, 20, 20,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, -5,-10,  0,  0,-10, -5,  5, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10,-20,-20, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0 /)
      
    srch%piece_sq(wn,0:127) = (/ & 
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  0,  0,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -30,  0, 10, 15, 15, 10,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 15, 20, 20, 15,  5,-30, 0,0,0,0,0,0,0,0, &
      & -30,  0, 15, 20, 20, 15,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 10, 15, 15, 10,  5,-30, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  5,  5,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0 /)
      
    srch%piece_sq(wb,0:127) = (/ & 
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5, 10, 10,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5, 10, 10,  5,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0, 10, 10, 10, 10,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10, 10, 10, 10, 10, 10, 10,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  0,  0,  0,  0,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0 /)

    srch%piece_sq(wr,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10, 10, 10, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  5,  5,  0,  0,  0,  0,0,0,0,0,0,0,0 /)
      
    srch%piece_sq(wq,0:127) = (/ & 
      &  -20,-10,-10, -5, -5,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &   -5,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &    0,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10, -5, -5,-10,-10,-20,  0,0,0,0,0,0,0,0 /)

    srch%piece_sq(wk,0:127) = (/ & 
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -20,-30,-30,-40,-40,-30,-30,-20, 0,0,0,0,0,0,0,0, &
      &  -10,-20,-20,-20,-20,-20,-20,-10, 0,0,0,0,0,0,0,0, &
      &   20, 20,  0,  0,  0,  0, 20, 20, 0,0,0,0,0,0,0,0, &
      &   20, 30, 10,  0,  0, 10, 30, 20,  0,0,0,0,0,0,0,0 /)
      
    srch%piece_sq(7:12,0:127) = srch%piece_sq(1:6,0:127)
    do p_ind=7,12
      call mirror(srch%piece_sq(p_ind,0:127))
    end do
  end subroutine
   
  function static_eval() result(white_score)
    integer::white_score,sq,pie,col
    srch%ss_material = 0
    srch%ss_position = 0
    do sq = 0,127
      if(iand(sq,136) == 0)then
        pie = board(sq)
        if(pie /= empty)then
          col = get_color(pie)
          srch%ss_material(col) = srch%ss_material(col) + srch%piece_value(pie)
          srch%ss_position(col) = srch%ss_position(col) + srch%piece_sq(pie,sq)
        end if
      end if
    end do
    white_score = srch%ss_material(team_white) - srch%ss_material(team_black) &
              & + srch%ss_position(team_white) - srch%ss_position(team_black)
    if(state%side == team_black) white_score = -white_score         
  end function
 
  function equal_m(m1,m2) result(eq)
    type(type_move)::m1,m2
    logical::eq 
    eq = .false.
    if(m1%ini == m2%ini)then
      if(m1%fin == m2%fin)then
        if(m1%is_capture .eqv. m2%is_capture)then
          if(m1%promotion_pie == m2%promotion_pie)then
            if(m1%is_pawnstart .eqv. m2%is_pawnstart)then
              if(m1%is_enpassant .eqv. m2%is_enpassant)then
                if(m1%is_castling .eqv. m2%is_castling)then
                  if(m1%captured_pie == m2%captured_pie)then
                    eq = .true. !i know, eqv operator sux
                  end if
                end if 
              end if
            end if
          end if
        end if
      end if
    end if
  end function
 
  subroutine score_moves()
    integer::m_ind,pie,captured
    type(type_move)::m
    integer,dimension(0:12)::reward,hunter
    
    reward = (/ 0,1000,3100,3200,5000,9500,100000,1000,3100,3200,5000,9500,1000000 /)
    hunter = (/ 0,-10,-31,-32,-50,-95,-100,-10,-31,-32,-50,-95,-100/)
    
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      moves_score(m_ind) = 0
      m = moves_list(m_ind)
      if(equal_m(m,srch%killers(1,ply_depth)))then
         moves_score(m_ind) = 100
      end if
      if(equal_m(m,srch%killers(0,ply_depth)))then
         moves_score(m_ind) = 200
      end if
      if(m%is_capture)then
        pie = board(m%ini)
        captured = m%captured_pie
        moves_score(m_ind) = moves_score(m_ind) + reward(captured)
        moves_score(m_ind) = moves_score(m_ind) + hunter(pie)
      end if
      if(m%promotion_pie /= empty)then
        moves_score(m_ind) = moves_score(m_ind) + reward(m%promotion_pie)
      end if
    end do
  end subroutine

  recursive function alpha_beta(depth,alpha,beta) result(best_score)
    integer::depth,v
    integer::best_score,beta,alpha
    integer::score
    integer::m_ind,king_sq,rule50_ind
    type(type_move)::m
    integer::best_ind,best_score_move
    !return due to game end
    if(state%rule50 == 100)then
      best_score = 0
      return
    end if
    do rule50_ind = 1,state%rule50
      if(hist(ply-rule50_ind)%hash == position_hash)then
        best_score = 0
        return
      end if
    end do
    call gen_moves()
    if(moves_list_ind(ply) == moves_list_ind(ply+1))then
      king_sq = piece_list%kings(state%side)
      if(is_attacked(king_sq,ieor(1,state%side)))then
        best_score = -mate_score+depth
      else
        best_score = 0
      end if
      return
    end if
    !static eval
    if(depth==srch%calling_depth)then
      srch%nodes = srch%nodes + 1
      best_score = static_eval()
      best_score = quies(best_score,beta)
      return
    end if
    !start alpha_beta
    best_score = alpha
    !score moves for sort
    call score_moves()
    do while(.true.)
      !select best move
      best_score_move = -inf
      best_ind = -1
      do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
        if(moves_score(m_ind) > best_score_move)then
          best_score_move = moves_score(m_ind)
          best_ind = m_ind
          if(m_ind >= 0)then
            moves_score(m_ind) = -inf
          else
            write(*,*)'trying to acces move_score with negative index'
          end if
        end if
      end do
      m_ind = best_ind
    
      if(m_ind == -1) exit
      m = moves_list(m_ind)
      call make_move(m)
      ply_depth = ply_depth+1
      v = -alpha_beta(depth+1,-beta,-best_score)
      call undo_last_move()
      ply_depth = ply_depth-1
      if(v >= beta)then
        best_score = beta
          srch%killers(1,ply_depth) = srch%killers(0,ply_depth)
          srch%killers(0,ply_depth) = m
        return
      end if
      if(v > best_score)then
        best_score = v
        if(depth == 0)then
          srch%best_move = m
        end if
      end if
    end do
  end function
  
  recursive function quies(alpha_in,beta) result(alpha)
    integer::alpha_in,beta,alpha,v,m_ind,best_ind,best_score_move
    type(type_move)::m
    alpha = alpha_in
    v = static_eval()
    if(v >= beta)then
      alpha = beta
      return
    end if
    if(v > alpha)then
      alpha = v
    end if
    call gen_good_moves()
    if(moves_list_ind(ply) /= moves_list_ind(ply+1))then
      ! alpha = v
      ! return
    ! end if
    call score_moves()
    do while(.true.)
      !select best move
      best_score_move = -inf
      best_ind = -1
      do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
        if(moves_score(m_ind) > best_score_move)then
          best_score_move = moves_score(m_ind)
          best_ind = m_ind
          if(m_ind >= 0)then
            moves_score(m_ind) = -inf
          else
            write(*,*)'trying to acces move_score with negative index'
          end if
        end if
      end do
      m_ind = best_ind
      
      if(m_ind == -1) exit
      m = moves_list(m_ind)
      call make_move(m)
      v = -quies(-beta,-alpha)
      call undo_last_move()
      if(v >= beta)then
        alpha = beta
        return
      end if
      if(v > alpha)then
        alpha = v
      end if
    end do
    else
    ! alpha = v
    end if
  end function
  
  subroutine search_handler1(depth)
    integer::depth,iter_depth
    !iterate depth
    do iter_depth = 1,depth
      !init
      srch%nodes = 0
      srch%best_score = -inf
      srch%best_move = move_null
      srch%calling_depth = iter_depth
      !alpha beta
      call cpu_time(srch%t_ini)
      ply_depth = 0
      srch%best_score = alpha_beta(0,-inf,inf)
      call cpu_time(srch%t_cur)
      !write results
      write(*,*)'depth ',iter_depth,'best_move ',move2alg(srch%best_move),'best_score ',srch%best_score, &
        & 'nodes ',srch%nodes,'time ',srch%t_cur-srch%t_ini
    end do
  end subroutine
  
  end program lostchess
