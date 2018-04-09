:-use_module(library(clpfd)).
:-use_module(library(lists)).


visnum([_X],1).
visnum([A,B|L],K):-
    (B#>A)#<=>X,
    K0 #=K-X,
    C#=max(A,B),
    visnum([C|L],K0).


skysudoku(SP, Matrix):-
    
    %build the matrix, establish the domain
    SP=ss(N,Clues),
    Length is N*N,
    length(Matrix, Length), maplist(same_length(Matrix), Matrix),
    append(Matrix, Vars), domain(Vars, 1, Length),
    
    %extract clues
    extractClues(Clues, Matrix),
    
    %sudoku constraints
    maplist(all_distinct, Matrix),
    transpose(Matrix, Columns),
    maplist(all_distinct, Columns),
    extract_subgrid(Matrix, N, Extracted),
    maplist(all_distinct, Extracted),

    %call to labeling
    labeling([bisect],Vars).
    
extract_subgrid(L, Size, Extracted):-
    maplist(chop(Size), L, List),
    transpose(List,List_flip),
    maplist(append, List_flip, List_big),
    maplist(chop(Size*Size),List_big,List_big2),
	append(List_big2, Extracted).



extractClues([],Matrix).
extractClues([H|T], Matrix):-
	(   H=g(N,R,C),nth1(R,Matrix,Row),nth1(C,Row,N)
	;   H=v(V,Dir,RC), processVis(V,Dir,RC,Matrix)
	),    
	extractClues(T,Matrix).

processVis(V,Dir,RC,Matrix):-
	( Dir=n,
	  transpose(Matrix,Columns),
	  nth1(RC,Columns,Column),visnum(Column,V)

	    ; Dir=e,
	    nth1(RC,Matrix,Row),reverse(Row,RowRev),visnum(RowRev,V)

	
	; Dir=s,
	    transpose(Matrix,Columns),
	    nth1(RC,Columns,Column), reverse(Column,ColRev), visnum(ColRev,V)
	   
	    ; Dir=w,
	    nth1(RC,Matrix,Row),visnum(Row,V)
	).

%the following two predicates are used in my implementation of extract_subgrid

chop(N, List, ListOfLists):-
    ( List=[] ->   ListOfLists=[]
    ;   ListOfLists=[Head|_L2],\+split(N,List,Head,_Back) ->  ListOfLists=[List]
    ;   ListOfLists=[Head|L2],split(N,List,Head,Back), chop(N,Back,L2)
    ).

split(N, List, Front, Back):-
    (   N=0 ->  List=Back, Front=[]
    ;   N1 is N-1,List=[X|L3], Front=[X|L1], split(N1,L3,L1,Back)
    ).
