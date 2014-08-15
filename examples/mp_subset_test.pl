
:- ensure_loaded(prolog/boqa).
:- ensure_loaded(prolog/boqa/rdfsboqa).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdfs)).

t(Qs) :-
        debug(bn,'Searching: ~w',[Qs]),
        bsearch(Qs,Item,P),
        format('P(~w | ~w) = ~w~n',[Item,Qs,P]),
        fail.
t(_).

t :-
        set_prolog_flag(verbose,true),
        debug(bn),
        debug(bn,'Loading:',[]),
        rdf_load('examples/data/mp-subset.ttl'),
        %rdf_load('examples/data/mp.ttl'),
        t(['reproductive system phenotype', 'abnormal cerebellum development', 'abnormal synaptic transmission']).


