:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(between)).

delete([],[]).
delete([H|T],L):-
    (   H=0 ->  delete(T,L)
    ;   L=[H2|T2],H2=H, delete(T,T2)
    ).

is_set1(Lst) :-
    sort(Lst, Set),
    length(Lst, N),
    length(Set, N).

list_check(L):-
    delete(L,X),
    is_set1(X).

list_of_lists_check([]).
list_of_lists_check([H|T]):-
    list_check(H),
    list_of_lists_check(T).

consistent(L):-
    list_of_lists_check(L),
    transpose(L,X),
    list_of_lists_check(X),
    L=[H|T],
    length(H,Length),
    Size is round(sqrt(Length)),
    extract_subgrid(L, Size, Extracted),
    list_of_lists_check(Extracted).

chop(N, List, ListOfLists):-
    ( List=[] ->   ListOfLists=[]
    ;   ListOfLists=[Head|_L2],\+split(N,List,Head,_Back) ->  ListOfLists=[List]
    ;   ListOfLists=[Head|L2],split(N,List,Head,Back), chop(N,Back,L2)
    ).

split(N, List, Front, Back):-
    (   N=0 ->  List=Back, Front=[]
    ;   N1 is N-1,List=[X|L3], Front=[X|L1], split(N1,L3,L1,Back)
    ).

extract_subgrid(L, Size, Extracted):-
    maplist(chop(Size), L, List),
    transpose(List,List_flip),
    maplist(append, List_flip, List_big),
    maplist(chop(Size*Size),List_big,List_big2),
	append(List_big2, Extracted).




generate_row(Length, Row,List):- generate_row_recursive(Length, Row,[],List).

generate_row_recursive(Length,[],List,List).
generate_row_recursive(Length,[H|T],List,Soln):-
    (   H=0 ->  between(1,Length,H1), 
    			append(List,[H1],List0),
    			generate_row_recursive(Length,T,List0,Soln)
    ;   append(List,[H],List0),
        generate_row_recursive(Length,T,List0,Soln)
    ).

sudoku0(Grid0, Grid):-
    Grid0=[H|T],
    length(H,Length),
    maplist(generate_row(Length),Grid0, Grid),
    consistent(Grid).

