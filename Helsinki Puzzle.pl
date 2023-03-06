grid_build(N,M):-
                  grid_build_Helper(N,N,M).
grid_build_Helper(_,0,[]).
grid_build_Helper(N,Counter,[H|T]):-
                                     Counter > 0,
                                     length(H,N),
					                 Counter1 is Counter - 1,
					                 grid_build_Helper(N,Counter1,T).

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

grid_gen(N,M):- grid_build(N,M),
                num_gen(1,N,L),
               	grid_gen_Helper(L,M).
grid_gen_Helper(_,[]).
grid_gen_Helper(L,[H|T]):-    
                       acceptedList(H,L),
                       grid_gen_Helper(L,T).					   
acceptedList([],_).
acceptedList([H|T],L):- member(H,L),
                        acceptedList(T,L).

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

grid_gen1(N,M):- grid_build(N,M),
                 trans(M,M1), 
                 num_gen(1,N,L),
				 acceptable_permutation(L,P),
				 grid_gen_Helper1(M,M1,L,P),
				 distinct_rows(M).
grid_gen_Helper1([],_,_,_).				 
grid_gen_Helper1([H|T],M1,L,P):- unification([H|T],M1,P),
                                 flatten([H|T],F),
                                 acceptedList1(F,L).					   

unification(_,[],[]).
unification(M,[H|T],[H1|T1]):- get_row(M,H1,Row),
                               H = Row,
							   unification(M,T,T1).

acceptedList1([],_).
acceptedList1([H|T],L):- member(H,L),
                         acceptedList1(T,L).
	
%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

num_gen(F,L,R):- numlist(F,L,R).

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

get_N([H|_],N):- length(H,N).
						
%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

get_Max(Grid,Max):- get_Max_Helper(Grid,0,Max).
get_Max_Helper([],Max,Max).
get_Max_Helper([H|T],N,Max):- max_list(H,N1), N1 >= N , get_Max_Helper(T,N1,Max).
get_Max_Helper([H|T],N,Max):- max_list(H,N1), N > N1 , get_Max_Helper(T,N,Max).						

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

check_num_grid(G):- get_N(G,N),
                    get_Max(G,Max),
					num_gen(1,Max,List),
					N >= Max,
					check_num_grid_Helper1(G,List).
check_num_grid_Helper1(_,[]).					
check_num_grid_Helper1([H|T],[H1|T1]):- check_num_grid_Helper2(H,[H1|T1],R),
                                        check_num_grid_Helper1(T,R).
check_num_grid_Helper2([],L,L).
check_num_grid_Helper2([H|T],List,R):- member(H,List),
                                       delete(List,H,List1),
									   check_num_grid_Helper2(T,List1,R).
check_num_grid_Helper2([H|T],List,R):- \+member(H,List),
									   check_num_grid_Helper2(T,List,R).									 

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

get_row([H|_],1,H).
get_row([_|T],Index,Row):- Index1 is Index - 1,
                           get_row(T,Index1,Row).

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

get_Column([],_,_).
get_Column(Grid,Index,Column):- get_Column_Helper(Grid,Index,[],Column).
get_Column_Helper([],_,Acc,Acc).
get_Column_Helper([H|T],Index,Acc,Column):- get_Element(H,Index,Element),
                                            append(Acc,[Element],Acc1),
											get_Column_Helper(T,Index,Acc1,Column).
get_Element([H|_],1,H).
get_Element([_|T],Index,Element):- Index1 is Index - 1,
                                   get_Element(T,Index1,Element).											

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

acceptable_distribution(G):- get_N(G,N),
                             acceptable_distribution_Helper(G,N).
acceptable_distribution_Helper(_,0).
acceptable_distribution_Helper(G,Index):- get_row(G,Index,Row),
                                          get_Column(G,Index,Column),
                                          Row \= Column,
                                          Index1 is Index - 1,
                                          acceptable_distribution_Helper(G,Index1).										  

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

acceptable_permutation(L,R):- permutation(L,R),
                              length(L,N),
                              acceptable(N,L,R).
acceptable(0,_,_).
acceptable(N,L,R):- nth1(N,L,Element1),
                    nth1(N,R,Element2),
                    Element1 \= Element2,
                    N1 is N - 1,
                    acceptable(N1,L,R).					
                              
%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

trans(M,M1):- get_N(M,N),
              trans_Helper(M,1,N,[],M1).
trans_Helper(_,N1,N,Acc,Acc):- N1 is N + 1.
trans_Helper(M,Index,N,Acc,M1):- get_Column(M,Index,Column),
                                 ColumnInList = [Column],
                                 append(Acc,ColumnInList,Acc1),
								 Index1  is Index + 1,
                                 trans_Helper(M,Index1,N,Acc1,M1).								 

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

distinct_rows([]).
distinct_rows([H|T]):- \+member(H,T),
                       distinct_rows(T).

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

distinct_columns(M):- trans(M,M1),
                      distinct_rows(M1).

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

row_col_match(M):- acceptable_distribution(M),
                   distinct_rows(M),
				   distinct_columns(M),
				   trans(M,M1),
				   row_col_match_Helper(M,M1).
row_col_match_Helper([],_).
row_col_match_Helper([H|T],M1):- member(H,M1),
                                 row_col_match_Helper(T,M1).				   

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

helsinki(N,G):- grid_gen1(N,G),
				check_num_grid(G).

%-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


















