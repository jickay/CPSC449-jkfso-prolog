/**
 * first_line(List, FirstLine)
 * extracts the first line from the character array as delimited by newline characters
 */

first_line(List, X) :-	/*newline base case*/
	[Head|Tail] = List,
	char_code(NL, 10),
	Head = NL,
	X = [].
first_line(List, X) :-	/*single element (ie EOF) base case*/
	length(List, 1), 
	X = List.
first_line(List, X) :-	/*recursive case*/
	[Head|Tail] = List,
	first_line(Tail, Y),
	X = [Head|Y].

	
/**
 * no_first_line(List, AllButFirst)
 * obtains the inverse of the function first_line, save for the \n 
 */

no_first_line(List, X) :-	/*newline recursion interupt*/
	[Head|Tail] = List,
	char_code(NL, 10),
	Head = NL,
	X = Tail.
no_first_line(List, X) :-	/*recursive case*/
	[Head|Tail] = List,
	no_first_line(Tail, X).


/** 
 * parse_pair(Line, [M,T])
 * obtains the pair as a tuple from a line
 * Ex: parse_pair(['(', 1, ',', A, ')'], X).
*/

parse_pair(Line, X) :-
	nth(2, Line, Machine),
	nth(4, Line, Task),
	X = (Machine, Task).

	
/**
* 
* Ex: parse_triple(['(', 1, ',', A, ',', 3, 4,')'], X).
*/

parse_triple(Line, X) :-
	nth(2, Line, Machine),
	nth(4, Line, Task),
	['(', Machine, ',', Task, ','|PEN] = Line,
	parse_penalty_triple(PEN, PenaltyList),
	concatenate_num(PenaltyList, PenaltyAtom),
	number_atom(Penalty, PenaltyAtom),
	X = (Machine, Task, Penalty).
	
	
/**
 * takes a line of chars of form "<penalty>)    " and returns penalty as an atom
 * Ex: parse_penalty_triple([2, 4, 8, ')', '\t', ' '], X).
 * >> returns 248 as an atom
 */
parse_penalty_triple(PEN, X) :-	/*base rightbracket case*/
	[H|Tail] = PEN,
	char_code(RB, 41),
	H = RB,
	X = [].
parse_penalty_triple(PEN, X) :-	/*recursive L->R gather*/
	[H|Tail] = PEN,
	parse_penalty_triple(Tail, Y),
	X = [H|Y].


/**
 * 
 * Ex: concatenate_num([1,2,3,4,5,6],X).
 */	
concatenate_num(List, X) :-
	length(List, 1),
	nth(1, List, A),
	number_atom(A, X), 
	!.
concatenate_num(List, X) :-	/*for when List ain't a list, it's a number*/
	integer(List),
	number_atom(List, X),
	!.
concatenate_num(List, X) :-
	length(List, 2),
	nth(1, List, A), 
	nth(2, List, B),
	number_atom(A, A_atom), 
	number_atom(B, B_atom),
	atom_concat(A_atom, B_atom, X), 
	!.
concatenate_num(List, X) :-
	[Head|Tail] = List,
	number_atom(Head, H_atom),
	concatenate_num(Tail, Y),
	atom_concat(H_atom, Y, X).
 	
	
	
 
 
 
 
 
 
 
 
 
 