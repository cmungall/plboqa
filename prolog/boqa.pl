/* -*- Mode: Prolog -*- */

/** <module> Bayesian Ontology Query Algorithm

  Perform probabilistic queries over ontologies

*/

:- module(boqa,
          [
           bsearch/3,
           bsearch_sorted/3,
           bsearch_results/2
           ]).

:- multifile class_ancestor/2.
:- multifile inferred_association/2.
:- multifile cls/1.
:- multifile item/1.
:- multifile lookup/2.
:- multifile p_false_negative/2. % beta
:- multifile p_false_positive/2. % alpha

alpha(T,P) :- p_false_positive(T,P),!.
alpha(_,0.0001).                % default false positive

beta(T,P) :- p_false_negative(T,P),!.
beta(_,0.05).                   % default false negative


%% mxy1QH(+Item,+Qs:list,+X,+Y,?Num) is det
%
% {X,Y} = {0,1}
mxy1QH(Item,Qs,X,Y,Num) :-
        debug(xbn,' m~w~w1( ~w) = ...',[X,Y,Item]),
        aggregate(count,
                  Term,
                  (   cls(Term),
                      qstate(Term,Qs,X), 
                      hstate(Item,Term,Y)), 
                  Num),
        debug(xbn,'    m~w~w1QH( ~w) = ~w',[X,Y,Item,Num]),
        !.
mxy1QH(_,_,_,_,0).


a_j(Item,Qs,V) :-
        item(Item), % ensure ground
        debug(xbn,'Calc a_j where j=~w',[Item]),
        beta(_,Beta),
        alpha(_,Alpha),
        mxy1QH(Item,Qs,0,1,M011),
        mxy1QH(Item,Qs,1,1,M111),
        mxy1QH(Item,Qs,0,0,M001),
        mxy1QH(Item,Qs,1,0,M101),
        % TODO: iteratively multiply per-term constants
        V is ((Beta ** M011) * ((1-Beta) ** M111) * ((1-Alpha) ** M001) * (Alpha ** M101)),
        debug(xbn,' a(~w) = ~w',[Item,V]).

%% bsearch(+Qs:list, ?Item,?PrItem) is nondet
%
% given a set of query terms (resolved using lookup/2), unify PrItem
% with the probability of Item being true, given the BOQA dn structure
bsearch(Qs,Item,PrItem) :-
        expand_query(Qs,Qs_1),
        debug(bn,'Expanded: ~w',[Qs_1]),
        bsearch_1(Qs_1,Item,PrItem).

bsearch_1(Qs,Item,PrItem) :-
        setof(Item-V,
              a_j(Item,Qs,V),
              IVPairs),
        setof(V,Item^member(Item-V,IVPairs),Vs),
        sum_list(Vs,NormConst),
        member(Item-V,IVPairs),
        debug(bn,'Pr = ~w / ~w',[V,NormConst]),
        PrItem is V / NormConst.

bsearch_nd(Qs,Pairs) :-
        findall(PrItem-Item,
                bsearch(Qs,Item,PrItem),
                Pairs).

bsearch_results(Qs,RevPairsSorted) :-
        findall(PrItem-Item,
                bsearch(Qs,Item,PrItem),
                Pairs),
        sort(Pairs,PairsSorted),
        reverse(PairsSorted,RevPairsSorted).



%% bsearch_sorted(Qs,Pairs)
bsearch_sorted(Qs,Item,Pr) :-
        bsearch_results(Qs,Pairs),
        member(Pr-Item,Pairs).



sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.        

zero_or_one(0).
zero_or_one(1).


hstate(Item,Term,1) :- inferred_association(Item,Term),!.
hstate(Item,Term,0) :- \+ inferred_association(Item,Term),!.

qstate(Term,Qs,1) :- memberchk(Term,Qs),!.
qstate(Term,Qs,0) :- \+ memberchk(Term,Qs),!.

expand_query(L1,L3) :-
        findall(Y,
                (   member(X,L1),
                    debug(bn,'Lookup: ~w',[X]),
                    lookup(X,X2),
                    debug(bn,'IRI: ~w',[X2]),
                    class_ancestor(X2,Y)),
                L2),
        debug(bn,'Expanded: ~w',[L2]),
        sort(L2,L3).






                  
