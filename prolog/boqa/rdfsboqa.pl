/* -*- Mode: Prolog -*- */

:- module(rdfsboqa,
          []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdfs)).

:- multifile boqa:class_ancestor/2.
:- multifile boqa:inferred_association/2.
:- multifile boqa:cls/1.
:- multifile boqa:item/1.

boqa:class_ancestor(X,Y) :- rdfs_subclass_of(X,Y).
boqa:inferred_association(X,Y) :- rdf(X,rdf:type,Z), rdfs_subclass_of(Z,Y).

boqa:cls(X) :- rdf(X,rdf:type,owl:'Class').
boqa:item(X) :- setof(X,item1(X),Xs),member(X,Xs).
item1(X) :- rdf(X,rdf:type,C),boqa:cls(C).

boqa:lookup(L,U) :-
        rdfs_label(U,L),
        !.
boqa:lookup(L,U) :-
        rdf(U,_,literal(type(_,L))),
        !.
boqa:lookup(L,_) :-
        rdf(L,_,_),
        format(user_error,'NO: ~w',[L]),
        fail.
boqa:lookup(X,X).


