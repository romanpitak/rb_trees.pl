/*
  Test procedures for the rb_trees.pl file
  Author : Roman Pitak
*/

:- include( 'rb_trees.pl' ).

% test1 - takes 10000 random numbers, builds a tree and deletes 10000 random elements from it.  
test1 :-
  rl( 10000, L ), 
  rb_build( L, T ), 
  rb_check( T ), 
  rd( 1000, T , _).
  
% builds a random tree of 60 elements
test2 :- 
  rl( 60 , L ), 
  rb_build( L, T ), 
  rb_check( T ), 
  rb_print( T ).
  
test3 :-
  rl( 200 , L ), 
  rb_build( L, T ), 
  rb_check( T ), 
  rd( 200, T, T1 ), 
  rd( 200, T1, T2 ), 
  rb_print( T2 ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% rl( +Number of elements, -List )
% outputs a List of N random elements
rl( N, L ) :- rl2( N, N, L ).
rl2( _, 0, [] ).
rl2( R, N, [ Rh | Rt ] ) :-
  N2 is N - 1,
  Rh is random( R ),   
  rl2( R, N2, Rt ).

% deletes N random numbers from T and checks the result
rd( N, T , TD) :-
  rd2( N, N, T, TD ).
rd2( 0, _, TD, TD ).
rd2( N, R, T , TD) :-
  N2 is N - 1, 
  Rd is random( R ), 
  write( 'Deleting : ' ), write( Rd ), nl, 
  rb_delete( T, Rd, Td ), 
  rb_check( Td ), 
  rd2( N2, R, Td, TD ).

% otputs a list of elements from zero to N
line( 0, [ 0 ] ).
line( N, [ N | L ] ) :-
  N2 is N-1, 
  line( N2, L ).
