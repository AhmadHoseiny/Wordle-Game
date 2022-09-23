:-dynamic word/2.
build_kb :-
		write('Please enter a word and its category on separate lines:'), 
		nl,
		read(X),
		(
		X=done
		;
		read(Y),
		assert(word(X,Y)),
		build_kb
		).
		
is_Category(C):- word(_,C).
																									
categories(L):- bagof(C,is_Category(C), Li),
				unique(Li,[],L) .
unique([],Acc,Acc).
unique([H|T] , Acc , R):- \+ member(H,Acc),
							append(Acc,[H], Acc2),
							unique(T,Acc2,R).
unique([H|T] , Acc , R):-   member(H,Acc),
							unique(T,Acc,R).
							
available_length(L):- word(X,_) ,
					  string_length(X,L).
					  
pick_word(W,L,C) :- word(W,C),
					string_length(W,L).
					
getRandomWord(W,Len,C):- setof(Wo, pick_word(Wo,Len,C) , L),
						length(L,LenLi),
						LenLiN is LenLi +1 ,
						random(1,LenLiN, N),
						getRandomWordH(W,L,N).
getRandomWordH(H,[H|_], 1).
getRandomWordH(W,[_|T] , N):-   N>1,
								N2 is N-1,
								getRandomWordH(W,T,N2).

deleteOneInst([H|T],H,T):- !.
deleteOneInst([H|T],X,[H|R]):-
			deleteOneInst(T,X,R).
correct_letters([],_,[]).
correct_letters([H|T],L2,[H|R]):- 
		member(H,L2),
		deleteOneInst(L2,H,L2N),!,
		correct_letters(T,L2N,R).
correct_letters([H|T],L2,CL):- 
		\+ member(H,L2),!,	
		correct_letters(T,L2,CL).

		
% assuming that the two lists are of same size which is always the case 
% as the word guessed by the player has to be of the same length of the 
% computer's word otherwise, the player has to try again.
% correct_positions([],_,[]).
% correct_positions(_,[],[]).
correct_positions([],[],[]).
correct_positions([H1|T1],[H2|T2],[H1|R]):- 
		H1==H2,
		correct_positions(T1,T2,R).
correct_positions([H1|T1],[H2|T2],R):- 
		H1\==H2,
		correct_positions(T1,T2,R).					


available_length_inCateg(Len,Categ):- word(X,Categ) ,
									string_length(X,Len).
readLength(Len,Categ):- read(X),
				  (
				  available_length_inCateg(X,Categ),
				  Len = X 
				  ;
				  \+ available_length_inCateg(X,Categ),
				  write('There are no words of this length.'),
				  nl,
				  write('Choose a length: '),
				  nl,
				  readLength(Len,Categ)
				  ).

readCategory(Categ):- read(X),
					  (
					  is_Category(X),
					  Categ = X 
					  ;
					  \+ is_Category(X),
					  write('This category does not exist.'),
					  nl,
					  write('Choose a category: '),
				      nl,
					  readCategory(Categ)
					  ).
					  
% wordM --> the main word selected randomly by the computer which 
% the player should guess a word to match with.					  
play:- write('The available categories are: '),
       categories(L),
	   write(L),
	   nl,
	   write('Choose a category: '),
	   nl,
	   readCategory(Categ),
	   write('Choose a length: '),
	   nl,
	   readLength(Len, Categ),
	   getRandomWord(WordM,Len,Categ),
	   CntGuess is Len +1 ,
	   write('Game started. You have '),
	   write(CntGuess),
	   write(' guesses.'),
	   nl,
	   nl,
	   playH(CntGuess,WordM).
playH(CntGuess,WordM):-
				  CntGuess > 0 ,
				  string_chars(WordM, L1),
				  string_length(WordM, LenM),
				  write('Enter a word composed of '),
				  write(LenM),
				  write(' letters:'),
				  nl,
				  read(WoCur),
				  string_length(WoCur , LenCur),
				  (
				    (
						LenM \== LenCur ,
						write('Word is not composed of '),
						write(LenM),
						write(' letters. Try Again.'),
						nl,
						write('Remaining Guesses are '),
						write(CntGuess),
						nl,
						playH(CntGuess , WordM)
					)
				  ;
				    (
						LenM == LenCur ,
						string_chars(WoCur,L2),
						correct_positions(L2,L1,CP),
						(
							(
							L1 == CP,
							write('You Won!')
							)	
						;
							(
								(
								L1\==CP,
								CntGuess>1,
								correct_letters(L2,L1,CL),
								write('Correct letters are: '),
								write(CL),
								nl,
								write('Correct letters in correct positions are: '),
								write(CP),
								nl,
								CntGuessN is CntGuess -1 ,
								write('Remaining Guesses are '),
								write(CntGuessN),
								nl,
								nl,
								playH(CntGuessN,WordM)
								)
								;
								(
								L1\==CP,
								CntGuess==1,
								write('You lost!')
								)
							)
						)
					)
				  ).
				  
				  
main :- 
		write('Welcome to Pro-Wordle!'),
	    nl,
	    write('-----------------------------'),
		nl,
		nl,
		build_kb,
		play.