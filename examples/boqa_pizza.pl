
:- ensure_loaded(prolog/boqa).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdfs)).

:- rdf_register_ns(pizza,'http://www.co-ode.org/ontologies/pizza/pizza.owl#').

:- multifile boqa:class_ancestor/2.
:- multifile boqa:inferred_association/2.
:- multifile boqa:cls/1.
:- multifile boqa:item/1.

% MAPPINGS

pizza(X) :- rdfs_subclass_of(X,pizza:'Pizza').

boqa:cls(X) :- rdf(X,rdf:type,owl:'Class').

boqa:item(X) :- pizza(X).

boqa:p_false_negative(_,0.01).
boqa:p_false_positive(_,0.05).


boqa:class_ancestor(X,Y) :- rdfs_subclass_of(X,Y), \+ rdf_is_bnode(Y).
boqa:class_ancestor(X,Y) :-
        rdfs_subclass_of(X,Z),
        % ignore edge labels
        rdf(Z,owl:someValuesFrom,W),
        rdfs_subclass_of(W,Y),
        \+ rdf_is_bnode(Y).

boqa:inferred_association(X,Y) :- pizza(X),boqa:class_ancestor(X,Y).


boqa:lookup(L,U) :-
        rdfs_label(U,L),
        !.
boqa:lookup(L,U) :-
        rdf(U,_,literal(type(_,L))),
        !.
boqa:lookup(L,U) :-
        rdf_global_id(pizza:L,U),
        !.
boqa:lookup(L,_) :-
        rdf(L,_,_),
        format(user_error,'NO: ~w',[L]),
        fail.
boqa:lookup(X,X).

% TEST

init :-
        set_prolog_flag(verbose,true),
        debug(bn),
        debug(bn,'Loading:',[]),
        rdf_load('examples/data/pizza.ttl').

t(Qs) :-
        t(Qs,_).
t(Qs, Expected) :-
        init,
        debug(bn,'Searching: ~w',[Qs]),
        bsearch_results(Qs,Pairs),
        Pairs=[_-First|_],
        (   boqa:lookup(Expected,First)
        ->  format('GOT_EXPECTED: ~w~n',Expected)
        ;   format('FAILED: ~w~n',Expected)),
        member(P-Item,Pairs),
        format('P(~w | ~w) = ~w~n',[Item,Qs,P]),
        fail.
t(_, _).

t :-
        t(['OliveTopping', 'TomatoTopping'], _).

t1 :-
        t(['RocketTopping'], 'Soho').

t2 :-
        t(['GreenPepperTopping', 'HotSpicedBeefTopping', 'OnionTopping'], 'SloppyGuiseppe').


t3 :-
        t(['SpicyTopping', 'FishTopping'], _).

t4 :-
        t(['FruitTopping', 'NutTopping', 'VegetableTopping'], 'Veneziana').


