/* 
  This is a prolog impementation of red-black trees
  Author : Roman Pitak  
*/

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_EMPTY
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_empty( ?Tree ).
rb_empty( t( b, nil, nil, nil ) ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_SEARCH
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_search( +Tree, +Element ).
rb_search( t( b, nil, nil, nil ), _ ) :- !, fail.
rb_search( t( _, K, _, _ ), K ).
rb_search( t( _, K, Ls, _ ), Ks ) :-
  K > Ks,
  rb_search( Ls, Ks ).
rb_search( t( _, K, _, Rs ), Ks ) :-
  K < Ks,
  rb_search( Rs, Ks ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_INORDER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_inOrder( +Tree, -List ).
rb_inOrder( t( b, nil, nil, nil ), [] ).
rb_inOrder( t( _, K, Ls, Rs ), L ) :-
  rb_inOrder( Ls, Ll ), 
  rb_inOrder( Rs, Lr ),
  append( Ll, [ K | Lr ], L ).
  
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_PREORDER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_preOrder( +Tree, -List ).
rb_preOrder( t( b, nil, nil, nil ), [] ).
rb_preOrder( t( _, K, Ls, Rs ), L ) :-
  rb_preOrder( Ls, Ll ), 
  rb_preOrder( Rs, Lr ),
  append( [ K | Ll ], Lr, L ).
  
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_POSTORDER
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_postOrder( +Tree, -List ).
rb_postOrder( t( b, nil, nil, nil ), [] ).
rb_postOrder( t( _, K, Ls, Rs ), L ) :-
  rb_postOrder( Ls, Ll ), 
  rb_postOrder( Rs, Lr ),
  append( Ll, Lr, Lp ), 
  append( Lp, [ K ], L ).
  
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_BUILD
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_build( +List, -Tree )
rb_build( L, T ) :- rb_empty( T0 ), rb_b( L, T0, T ).
rb_b( [], T, T ).
rb_b( [ K ], T1, T2 ) :- 
  rb_insert( T1, K, T2 ).
rb_b( [ H | T ], T1, T3 ) :-
  rb_insert( T1, H, T2 ), 
  rb_b( T, T2, T3 ).
  
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_MAXIMUM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_maximum( +Tree, -Maximum ).
% fails on empty !
rb_maximum( t( _, K, _, t( b, nil, nil, nil ) ), K ).
rb_maximum( t( _, _, _, Rs ), Maximum ) :-
  rb_maximum( Rs, Maximum ).
  
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_MINIMUM
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_minimum( +Tree, -Minimum ).
% fails on empty !
rb_minimum( t( _, K, t( b, nil, nil, nil ), _ ), K ).
rb_minimum( t( _, _, Ls, _ ), Minimum ) :-
  rb_minimum( Ls, Minimum ).
  
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_PRINT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_print( +Tree ).
rb_print( T ) :-
  write( '[ ] Black node' ), nl, 
  write( '( ) Red node' ), nl, nl,
  rb_print( T, [] , nodebug ), nl, !.

rb_print( t( b, nil, nil, nil ), _, nodebug ).
rb_print( nil, N , debug ) :- !, 
  rb_spaces( N ), write( 'nil' ), nl.
rb_print( t( b, nil, Ls, Rs ), N , debug ) :- 
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , debug ), !, 
  rb_spaces( N ), put( '[' ), write( nil ), put( ']' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , debug ), !.
rb_print( t( b, K, Ls, Rs ), N , D ) :-
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , D ), !, 
  rb_spaces( N ), put( '[' ), write( K ), put( ']' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , D ), !.
rb_print( t( r, K, Ls, Rs ), N , D ) :-
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , D ), !, 
  rb_spaces( N ), put( '(' ), write( K ), put( ')' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , D ), !.
rb_print( t( bb, K, Ls, Rs ), N , D ) :-
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , D ), !, 
  rb_spaces( N ), put( '{' ), write( K ), put( '}' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , D ), !.

rb_spaces( [] ).
rb_spaces( [ _ ] ) :- 
  write( ' |---' ).
rb_spaces( [ H, H | T ] ) :-
  write( '     ' ), 
  rb_spaces( [ H | T ] ).
rb_spaces( [ _ | T ] ) :-
  write( ' |   ' ), 
  rb_spaces( T ).
  
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_CHECK
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_check( +Tree ).
rb_check( t( b, K, Ls, Rs ) ) :-
  rb_ch( t( b, K, Ls, Rs ), true, _ ),
  write( 'rb_check successfull' ), nl.

% rb_ch( +Tree, -Validity, -NumberOfBlackNodes ).
% Nbn - number of black nodes
rb_ch( t( b, nil, nil, nil ), true, 1 ).
% black node with no children
rb_ch( t( b, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), true, 2 ) :- !, 
  K \= nil.

% black node with a left child
rb_ch( t( b, K, t( r, Kl, Lsl, Rsl ), t( b, nil, nil, nil ) ), true, 2 ) :- !, 
  rb_ch( t( r, Kl, Lsl, Rsl ), Bl, Nl ), !, 
  Bl = true,
  Nl = 1,
  K \= nil,
  Kl \= nil,
  K > Kl.
% black node with a right child
rb_ch( t( b, K, t( b, nil, nil, nil ), t( r, Kr, Lsr, Rsr ) ), true, 2 ) :- !, 
  rb_ch( t( r, Kr, Lsr, Rsr ), Br, Nr ), !, 
  Br = true,
  Nr = 1,
  K \= nil,
  Kr \= nil,
  K < Kr.
% black node with both children
rb_ch( t( b, K, t( Cl, Kl, Lsl, Rsl ), t( Cr, Kr, Lsr, Rsr ) ), true, N ) :- !, 
  rb_ch( t( Cl, Kl, Lsl, Rsl ), Bl, Nl ), !, 
  rb_ch( t( Cr, Kr, Lsr, Rsr ), Br, Nr ), !, 
  Bl = true, 
  Br = true,
  Nl = Nr,
  N is Nl + 1,
  K \= nil,
  Kl \= nil,
  Kr \= nil,
  K > Kl, 
  K < Kr.  
% red node with no children
rb_ch( t( r, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), true, 1 ) :- !, 
  K \= nil.
% I don't have to implement red node with one child cuz it never happens. :)
% red node with both children
rb_ch( t( r, K, t( b, Kl, Lsl, Rsl ), t( b, Kr, Lsr, Rsr ) ), true, N ) :- !, 
  rb_ch( t( b, Kl, Lsl, Rsl ), Bl, Nl ), !, 
  rb_ch( t( b, Kr, Lsr, Rsr ), Br, Nr ), !, 
  Bl = true, 
  Br = true,
  Nl = Nr,
  N is Nl + 0,
  K \= nil,
  Kl \= nil,
  Kr \= nil,
  K > Kl, 
  K < Kr.
  
% default FALSE
rb_ch( T, false, 0 ) :- !, 
  rb_print( T ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_INSERT
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_insert( +Tree, +Key, -Tree ).
rb_insert( T1, K, T3 ) :-
  rb_ins( T1, K, T2 ), 
  rb_reblackRoot( T2, T3 ), !.
% rb_ins( +Tree, +Key, -Tree ).
rb_ins( t( b, nil, nil, nil ), K, t( r, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ) ).
rb_ins( t( C, K, Ls, Rs ), K, t( C, K, Ls, Rs ) ).
rb_ins( t( C, K, Ls, Rs ), Ki, Tfixed ) :-
  K < Ki, 
  rb_ins( Rs, Ki, Rsi ), 
  rb_insFixup( t( C, K, Ls, Rsi ), Tfixed ).
rb_ins( t( C, K, Ls, Rs ), Ki, Tfixed ) :-
  K > Ki, 
  rb_ins( Ls, Ki, Lsi ), 
  rb_insFixup( t( C, K, Lsi, Rs ), Tfixed ).
  
% rb_insFixup( +Tree, -FixedTree ).
% CASE #1
rb_insFixup( t( b, Kz, t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ), t( r, Ku, Lsu, Rsu ) ), t( r, Kz, t( b, Ky, t( r, Kx, Lsx, Rsx ), Rsy ), t( b, Ku, Lsu, Rsu ) ) ).
rb_insFixup( t( b, Kz, t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ), t( r, Ku, Lsu, Rsu ) ), t( r, Kz, t( b, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ), t( b, Ku, Lsu, Rsu ) ) ).
rb_insFixup( t( b, Kz, t( r, Ku, Lsu, Rsu ), t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ) ), t( r, Kz, t( b, Ku, Lsu, Rsu ), t( b, Ky, t( r, Kx, Lsx, Rsx ), Rsy ) ) ).
rb_insFixup( t( b, Kz, t( r, Ku, Lsu, Rsu ), t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ) ), t( r, Kz, t( b, Ku, Lsu, Rsu ), t( b, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ) ) ).
% CASE #2
rb_insFixup( t( b, Kz, t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ), Uncle ), Tfixed ) :-
  rb_insFixup( t( b, Kz, t( r, Kx, t( r, Ky, Lsy, Lsx ), Rsx ), Uncle ), Tfixed ).
rb_insFixup( t( b, Kz, Uncle, t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ) ), Tfixed ) :-
  rb_insFixup( t( b, Kz, Uncle, t( r, Kx, Lsx, t( r, Ky, Rsx, Rsy ) ) ), Tfixed ).
% CASE #3
rb_insFixup( t( b, Kz, t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ), Uncle ), t( b, Ky, t( r, Kx, Lsx, Rsx ), t( r, Kz, Rsy, Uncle ) ) ).
rb_insFixup( t( b, Kz, Uncle, t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ) ), t( b, Ky, t( r, Kz, Uncle, Lsy ), t( r, Kx, Lsx, Rsx ) ) ).

rb_insFixup( T, T ).

% rb_reblackRoot( +Tree, -Tree ).
rb_reblackRoot( t( _, K, Ls, Rs ), t( b, K, Ls, Rs ) ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%% RB_DELETE
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rb_delete( +Tree, +Key, -Tree ).
rb_delete( T, K, Trb ) :-
  rb_del( T, K, Td ), 
  rb_reblackRoot( Td, Trb ).
  
% The Key has not been found
rb_del( t( b, nil, nil, nil ),  _, t( b, nil, nil, nil ) ).
% deleting a red node with no children
rb_del( t( r, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), K, t( b, nil, nil, nil ) ).
% deleting a red node with a left child
rb_del( t( r, K, Ls, t( b, nil, nil, nil ) ), K, Ls ).
% deleting a red node with a right child
rb_del( t( r, K, t( b, nil, nil, nil ), Rs ), K, Rs ).
% deleting a black node with a red left child
rb_del( t( b, K, t( r, Kl, Lsl, Rsl ), t( b, nil, nil, nil ) ), K, t( b, Kl, Lsl, Rsl ) ).
% deleting a black node with a red right child
rb_del( t( b, K, t( b, nil, nil, nil ), t( r, Kr, Lsr, Rsr ) ), K, t( b, Kr, Lsr, Rsr ) ).
% since a black node cannot have a  black only-child, the only fucked-up case is deleting a black node without children. We color the new node 'bb' which means double black
rb_del( t( b, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), K, t( bb, nil, nil, nil ) ).
% deleting a node with both children
rb_del( t( C, K, Ls, Rs ), K, Tfixed ) :-
  rb_maximum( Ls, Max ), 
  rb_del( Ls, Max, Lsd ),
  rb_delFixup( t( C, Max, Lsd, Rs ), Tfixed ).
% branching the delete
rb_del( t( C, K, Ls, Rs ), Kd, Tfixed ) :-
  K > Kd, 
  rb_del( Ls, Kd, Lsd ),
  rb_delFixup( t( C, K, Lsd, Rs ), Tfixed ).
rb_del( t( C, K, Ls, Rs ), Kd, Tfixed ) :-
  K < Kd, 
  rb_del( Rs, Kd, Rsd ),
  rb_delFixup( t( C, K, Ls, Rsd ), Tfixed ).
  
% rb_delFixup( +Tree, -FixedTree ).

% CASE #2
rb_delFixup( t( b, Kp, N, t( r, Ks, Lss, Rss ) ), t( b, Ks, Tfixed, Rss ) ) :-
  N = t( bb, _, _, _ ),
  rb_delFixup( t( r, Kp, N, Lss ), Tfixed ).
rb_delFixup( t( b, Kp, t( r, Ks, Lss, Rss ), N ), t( b, Ks, Lss, Tfixed ) ) :-
  N = t( bb, _, _, _ ),
  rb_delFixup( t( r, Kp, Rss, N ), Tfixed ).
  
% CASE #3
rb_delFixup( t( b, Kp, t( bb, Kn, Lsn, Rsn ), t( b, Ks, Lss, Rss ) ),
             t( bb, Kp, t( b, Kn, Lsn, Rsn ), t( r, Ks, Lss, Rss ) ) ) :-
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).
rb_delFixup( t( b, Kp, t( b, Ks, Lss, Rss ), t( bb, Kn, Lsn, Rsn ) ), 
             t( bb, Kp, t( r, Ks, Lss, Rss ), t( b, Kn, Lsn, Rsn ) ) ) :-
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).

% CASE #4
rb_delFixup( t( r, Kp, N, t( b, Ks, Lss, Rss ) ),
             t( b, Kp, N2, t( r, Ks, Lss, Rss ) ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).
rb_delFixup( t( r, Kp, t( b, Ks, Lss, Rss ), N ), 
             t( b, Kp, t( r, Ks, Lss, Rss ), N2 ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).
  
% CASE #5
rb_delFixup( t( Cp, Kp, N, S ), Tfixed ) :-
  N = t( bb, _, _, _ ),
  S = t( b, Ks, Lss, Rss ),
  Lss = t( r, Ksl, Lsl, Rsl ),
  Rss = t( b, _, _, _ ),
  rb_delFixup( t( Cp, Kp, N, t( b, Ksl, Lsl, t( r, Ks, Rsl, Rss ) ) ), Tfixed ).
rb_delFixup( t( Cp, Kp, S, N ), Tfixed ) :-
  N = t( bb, _, _, _ ),
  S = t( b, Ks, Lss, Rss ),
  Lss = t( b, _, _, _ ),
  Rss = t( r, Ksr, Lsr, Rsr ),
  rb_delFixup( t( Cp, Kp, t( b, Ksr, t( r, Ks, Lss, Lsr ), Rsr ), N ), Tfixed ).
  
% CASE #6
rb_delFixup( t( Cp, Kp, N, S ), 
             t( Cp, Ks, t( b, Kp, N2, Lss ), Rss2 ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  S = t( b, Ks, Lss, Rss ), 
  Rss = t( r, Kr, Lsr, Rsr ),
  Rss2 = t( b, Kr, Lsr, Rsr ).
  
rb_delFixup( t( Cp, Kp, S, N ), 
             t( Cp, Ks, Lss2, t( b, Kp, Rss, N2 ) ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  S = t( b, Ks, Lss, Rss ), 
  Lss = t( r, Kl, Lsl, Rsl ),
  Lss2 = t( b, Kl, Lsl, Rsl ).
			 
% the default GOOD case ( nothing is wrong with the tree ( probably ) )
rb_delFixup( T, T ).
