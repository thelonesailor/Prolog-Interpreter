male(king_shantanu).
male(vyas).
male(sage_parashar).
male(chitrangad).
male(vichitraveerya).
male(pandu).
male(yudhisthir).
male(bheem).
male(arjun).
male(vidur).
male(karn).
male(nakul).
male(sahdev).
male(dhrutarashtra).
male(dussan).
male(yuyutsu).
male(duryon).
male(surya).
male(abhimanyu).
male(arjun).

female(ambalika).
female(kunti).
female(madri).
female(ganga).
female(satyavati).
female(ambika).
female(gandhari).
female(dushala).
female(third_maid).
female(sauvali).
female(draupadi).
female(hidimba).
female(subhadra).

%child(X,Y) X has child Y
child(sage_parashar,vyas).
child(satyavati,vyas).
child(satyavati,vichitraveerya).
child(satyavati,chitrangad).

child(ambalika,pandu).
child(vyas,pandu).
child(third_maid,vidur).
child(vyas,vidur).
child(kashya,ambika).
child(kashya,ambalika).

child(ambika,dhrutarashtra).
child(vyas,dhrutarashtra).

child(kunti,yudhisthir).
child(kunti,bheem).
child(kunti,arjun).
child(arjun,abhimanyu).
child(subhadra,abhimanyu).
child(pandu,yudhisthir).
child(pandu,bheem).
child(pandu,arjun).
child(gandhari,duryon).
child(gandhari,dushala).
child(gandhari,dussan).
child(dhrutarashtra,duryon).
child(dhrutarashtra,dushala).
child(dhrutarashtra,dussan).
child(madri,nakul).
child(madri,sahdev).
child(pandu,nakul).
child(pandu,sahdev).
child(kunti,karn).
child(surya,karn).
child(dhrutarashtra,yuyutsu).
child(sauvali,yuyutsu).
child(bheem,ghatotkach).
child(hidimba,ghatotkach).
child(ganga,bheeshma).
child(shantanu,bheeshma).


married(king_shantanu,ganga).
married(king_shantanu,satyavati).
married(ambika,vichitraveerya).
married(ambalika,vichitraveerya).
married(third_maid,vichitraveerya).
married(pandu,kunti).
married(pandu,madri).
married(gandhari,dhrutarashtra).
married(arjun,subhadra).
married(yudhistir,draupadi).
married(bheem,draupadi).
married(bheem,hidimba).
married(nakul,draupadi).
married(sahdev,draupadi).
married(arjun,draupadi).

married(ganga,king_shantanu).
married(satyavati,king_shantanu).
married(vichitraveerya,ambika).
married(vichitraveerya,ambalika).
married(vichitraveerya,third_maid).
married(kunti,pandu).
married(madri,pandu).
married(dhrutarashtra,gandhari).
married(subhadra,arjun).
married(draupadi,yudhistir).
married(draupadi,bheem).
married(hidimba,bheem).
married(draupadi,nakul).
married(draupadi,sahdev).
married(draupadi,arjun).

%married(X,Y) :- married(Y,X),!.

%X is father of Y
father(X,Y) :- male(X),child(X,Y).
%X is mother of Y
mother(X,Y) :- female(X),child(X,Y).

%X is son of Y
son(X,Y) :- male(X),child(Y,X).
%X is daughter of Y
daughter(X,Y) :- female(X),child(Y,X).

%X is grandfather of Y
grandfather(X,Y) :- male(X),child(X,Somebody),child(Somebody,Y).
%X is grandmother of Y
grandmother(X,Y) :- female(X),child(X,Somebody),child(Somebody,Y).

%X is aunt of Y
aunt(X,Y) :- female(X),sister(X,Parent),child(Parent,Y).
uncle(X,Y) :- male(X),brother(X,Parent),child(Parent,Y).

%X is ancestor of Y
ancestor(X,Y) :- child(X,Y).
ancestor(X,Y) :- child(X,Somebody),ancestor(Somebody,Y).

%X is descendant of Y
descendant(X,Y) :- ancestor(Y,X).

%X is wife of Y
wife(X,Y) :- female(X),married(X,Y).

%X is husband of Y
husband(X,Y) :- male(X),married(X,Y).

%X is sister of Y
sister(X,Y) :- female(X),sibling(X,Y).
%X is brother of Y
brother(X,Y) :- male(X),sibling(X,Y).

%X is sister_in_law of Y
sister_in_law(X,Y) :- female(X),married(X,C),brother(C,Y).
%X is brother of Y
brother_in_law(X,Y) :- male(X),married(X,C),brother(C,Y).

%X and Y are siblings
sibling(X,Y) :- male(C),female(D),child(C,X),child(C,Y),child(D,X),child(D,Y).

%X and Y are cousins
cousin(X,Y) :- child(C,X),sibling(C,D),child(D,Y).

%first_cousin
first_cousin(A,B) :- cousin(A,B).

%A and B are half_sibling
half_sibling(A,B) :- child(P1,A),child(P1,B),child(P2,A),child(P3,B).

%X is nephew of Y
nephew(X,Y) :- male(X),child(B,X),sibling(B,Y).
%X is neice of Y
neice(X,Y) :- female(X),child(B,X),sibling(B,Y).

%related if common ancestor
related(X,Y) :- ancestor(Somebody,X),ancestor(Somebody,Y).


relation(X,Y,father)    		:- father(X,Y).
relation(X,Y,mother)     		:- mother(X,Y).
relation(X,Y,son)		  		:- son(X,Y).
relation(X,Y,daughter)  		:- daughter(X,Y).
relation(X,Y,grandfather)    	:- grandfather(X,Y).
relation(X,Y,grandmother)    	:- grandmother(X,Y).
relation(X,Y,aunt)    		:- aunt(X,Y).
relation(X,Y,uncle)    		:- uncle(X,Y).
relation(X,Y,wife)    		:- wife(X,Y).
relation(X,Y,husband)    		:- husband(X,Y).
relation(X,Y,sister)    		:- sister(X,Y).
relation(X,Y,brother)    		:- brother(X,Y). 
relation(X,Y,sister_in_law)   :- sister_in_law(X,Y).
relation(X,Y,brother_in_law)  :- brother_in_law(X,Y). 
relation(X,Y,sibling)    		:- sibling(X,Y).
relation(X,Y,half_sibling)    :- half_sibling(X,Y).
relation(X,Y,first_cousin)    :- first_cousin(X,Y).
relation(X,Y,cousin)		    :- cousin(X,Y).
relation(X,Y,nephew)    		:- nephew(X,Y). 
relation(X,Y,neice)    		:- neice(X,Y). 
relation(X,Y,related) 	   	:- related(X,Y). 
relation(X,Y,ancestor)    	:- ancestor(X,Y).
relation(X,Y,descendant)    	:- descendant(X,Y).

