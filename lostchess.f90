
program lostchess
  implicit none
  integer,parameter::inf=2**20
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
  integer,dimension(0:max_moves_listed-1)::moves_score
  
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
    type(type_move),dimension(0:8)::killers
    ! statistics
    integer::nodes
    real::t_ini
    real::t_cur
  end type
  type(type_search)::srch1

  !testing variables
  integer::selected_option,alpha_beta_result,nodes
  
  !init program
  call init()
  call restart_game()

  do while (.true.)
    write(*,*) 'select options -----------------------------------------------------------------'
    write(*,*) '1 write board numbers | 2 perft | 3 PvP | 4PvE'
    write(*,*) '--------------------------------------------------------------------------------'
    read*, selected_option
    select case (selected_option)
      case (1)
        call write_board_numbers()
      case (2)
        call perft_handler()
      case (3)
        do while(.true.)
          call player_handler()
        end do
      case (4)
        ! do while(.true.)
        ! call player_handler()
        call search_handler1(6)
        ! nodes = 0
        ! alpha_beta_result = alpha_beta(5,-99999999,99999999)
        write(*,*)nodes
        ! end do
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
    do pie = 0,12
      do sq = 0,127
        hash_table%board(pie,sq) = floor(abs(sin(sq+pie*128+1.)*2**30))
      end do
    end do
    hash_table%side = floor(abs(sin(1664+1.)*2**30))
    hash_table%ep = floor(abs(sin(1665+1.)*2**30))
    do cp_ind = 0,15
      hash_table%cp(cp_ind) = floor(abs(sin(cp_ind+1666+1.)*2**30))
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
    !hash board and state
    position_hash = hash_position()
    !restart moves done
    ply = 0
    !restart history
    hist%cp = cp_all
    hist%ep = ob
    hist%move = move_null
    !reastart list of moves
    moves_list_ind = 0
    moves_list = move_null 
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
    hash = ieor(hash,hash_table%ep)
    hash = ieor(hash,hash_table%cp(state%cp))
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
      call write_board(srch1%piece_sq(pie,0:127))
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
      if(fen_ep == raw2alg(ep_ind))then
        state%ep = ep_ind
        exit
      end if
    end do
    
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
      call perft_restart()
      write(*,*) perft(depth),perft_starting_nodes(depth)
    end do        
        
    write(*,*) 'perft kiwipete'
    call set_fen('r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0')
    do depth = 1,4
      call perft_restart()
      write(*,*) perft(depth),perft_kiwipete_nodes(depth)
    end do 
    
    write(*,*) 'perft position3'
    call set_fen('8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - 0')      
    do depth = 1,5
      write(*,*) perft(depth),perft_position3_nodes(depth)
    end do 
    
    write(*,*) 'perft position4'
    call set_fen('r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0')      
    do depth = 1,5
      write(*,*) perft(depth),perft_position4_nodes(depth)
    end do 
      
    write(*,*) 'perft position5'
    call set_fen('rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 0')      
    do depth = 1,4
      write(*,*) perft(depth),perft_position5_nodes(depth)
    end do 
    
    call cpu_time(time_fin)
    write(*,*)'time: ',time_fin-time_ini !9.65
        
  end subroutine

  subroutine test_pline(depth)
    type(type_move)::best_move
    integer::depth,d_ind
    do d_ind = depth,1
      call search_handler1(depth)
      write(*,*)move2alg(srch1%best_move)
      call make_move(srch1%best_move)
    end do
    do d_ind = depth,1
      call undo_last_move()
    end do
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
    srch1%piece_value = (/ 0,100,310,320,500,950,100000,100,310,320,500,950,100000 /)
  
    srch1%piece_sq(wp,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      & 50, 50, 50, 50, 50, 50, 50, 50, 0,0,0,0,0,0,0,0, &
      & 10, 10, 20, 30, 30, 20, 10, 10, 0,0,0,0,0,0,0,0, &
      &  5,  5, 10, 25, 25, 10,  5,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0, 20, 20,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, -5,-10,  0,  0,-10, -5,  5, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10,-20,-20, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0 /)
      
    srch1%piece_sq(wn,0:127) = (/ & 
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  0,  0,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -30,  0, 10, 15, 15, 10,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 15, 20, 20, 15,  5,-30, 0,0,0,0,0,0,0,0, &
      & -30,  0, 15, 20, 20, 15,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 10, 15, 15, 10,  5,-30, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  5,  5,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0 /)
      
    srch1%piece_sq(wb,0:127) = (/ & 
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5, 10, 10,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5, 10, 10,  5,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0, 10, 10, 10, 10,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10, 10, 10, 10, 10, 10, 10,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  0,  0,  0,  0,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0 /)

    srch1%piece_sq(wr,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10, 10, 10, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  5,  5,  0,  0,  0,  0,0,0,0,0,0,0,0 /)
      
    srch1%piece_sq(wq,0:127) = (/ & 
      &  -20,-10,-10, -5, -5,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &   -5,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &    0,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10, -5, -5,-10,-10,-20,  0,0,0,0,0,0,0,0 /)

    srch1%piece_sq(wk,0:127) = (/ & 
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -20,-30,-30,-40,-40,-30,-30,-20, 0,0,0,0,0,0,0,0, &
      &  -10,-20,-20,-20,-20,-20,-20,-10, 0,0,0,0,0,0,0,0, &
      &   20, 20,  0,  0,  0,  0, 20, 20, 0,0,0,0,0,0,0,0, &
      &   20, 30, 10,  0,  0, 10, 30, 20,  0,0,0,0,0,0,0,0 /)
      
    srch1%piece_sq(7:12,0:127) = srch1%piece_sq(1:6,0:127)
    do p_ind=7,12
      call mirror(srch1%piece_sq(p_ind,0:127))
    end do
  end subroutine
   
  function static_eval() result(white_score)
    integer::white_score,sq,pie,col
    srch1%ss_material = 0
    srch1%ss_position = 0
    do sq = 0,127
      if(iand(sq,136) == 0)then
        pie = board(sq)
        if(pie /= empty)then
          col = get_color(pie)
          srch1%ss_material(col) = srch1%ss_material(col) + srch1%piece_value(pie)
          srch1%ss_position(col) = srch1%ss_position(col) + srch1%piece_sq(pie,sq)
        end if
      end if
    end do
    white_score = srch1%ss_material(team_white) - srch1%ss_material(team_black) &
              & + srch1%ss_position(team_white) - srch1%ss_position(team_black)
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
 
  subroutine score_moves(depth)
    integer::m_ind,pie,captured,depth
    type(type_move)::m
    integer,dimension(0:12)::reward,hunter
    
    reward = (/ 0,1000,3100,3200,5000,9500,100000,1000,3100,3200,5000,9500,1000000 /)
    hunter = (/ 0,-10,-31,-32,-50,-95,-100,-10,-31,-32,-50,-95,-100/)
    
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m = moves_list(m_ind)
      if(equal_m(m,srch1%best_move))then
         moves_score(m_ind) = 100000
      end if
      if(equal_m(m,srch1%killers(depth)))then
         moves_score(m_ind) = 200000
      end if
      if(m%ini == 1 .and. m%fin == 34)then
         moves_score(m_ind) = 9900000
      end if
      moves_score(m_ind) = 0
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

  function select_next_best() result(best_ind)
    integer::m_ind,best_score,best_ind
    best_score = -inf
    best_ind = -1
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      if(moves_score(m_ind) > best_score)then
        best_score = moves_score(m_ind)
        best_ind = m_ind
      end if
    end do
    moves_score(best_ind) = -inf
  end function

  recursive function alpha_beta(depth,alpha,beta) result(best_score)
    integer::depth,v
    integer::best_score,beta,alpha
    integer::score
    integer::m_ind
    type(type_move)::m
    !init
    best_score = alpha
    !leaf node
    if(depth==srch1%calling_depth)then
      srch1%nodes = srch1%nodes + 1
      ! best_score = static_eval()
      best_score = -quies(-beta,-best_score)
      return
    end if
    !expand tree
    call gen_moves()
    call score_moves(depth)
    do while(.true.)
      m_ind = select_next_best()
      if(m_ind == -1) exit
    ! do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m = moves_list(m_ind)
      ! write(*,*)m
      call make_move(m)
      v = -alpha_beta(depth+1,-beta,-best_score)
      call undo_last_move()
      if(v >= beta)then
        best_score = beta
        srch1%killers(depth) = m
        return
      end if
      if(v > best_score)then
        best_score = v
        if(depth == 0)then
          srch1%best_move = moves_list(m_ind)
        end if
      end if
    end do
  end function
  
  recursive function quies(alpha,beta) result(best_score)
    integer::alpha,beta,best_score,v,m_ind
    type(type_move)::m
    best_score = alpha
    v = static_eval()
    if(v >= beta)then
      best_score = beta
      return
    end if
    if(v > best_score)then
      best_score = v
    end if
    call gen_good_moves()
    call score_moves(0)!cant depend on depth
    do while(.true.)
      m_ind = select_next_best()
      if(m_ind == -1) exit
      m = moves_list(m_ind)
      call make_move(m)
      v = -quies(-beta,-best_score)
      call undo_last_move()
      if(v >= beta)then
        best_score = beta
        return
      end if
      if(v > best_score)then
        best_score = v
      end if
    end do
  end function
  
  subroutine search_handler1(depth)
    integer::depth,iter_depth
    !iterate depth
    do iter_depth = 1,depth
      !init
      srch1%nodes = 0
      srch1%best_score = -inf
      ! srch1%best_move = move_null
      srch1%calling_depth = iter_depth
      !alpha beta
      call cpu_time(srch1%t_ini)
      srch1%best_score = alpha_beta(0,-inf,inf)
      call cpu_time(srch1%t_cur)
      !write results
      write(*,*)'depth ',iter_depth,'best_move ',move2alg(srch1%best_move),'best_score ',srch1%best_score, &
        & 'nodes ',srch1%nodes,'time ',srch1%t_cur-srch1%t_ini
    end do
  end subroutine
  
  end program lostchess
