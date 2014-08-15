## Synopsis

    :- ensure_loaded(prolog/boqa).
    :- ensure_loaded(prolog/boqa/rdfsboqa).
    
    :- use_module(library(semweb/rdf_db)).
    :- use_module(library(semweb/rdf_turtle)).
    
    query(Qs) :-
            bsearch(Qs,Item,P),
            format('P(~w | ~w) = ~w~n',[Item,Qs,P]),
            fail.
    query(_).
    
    t :-
            rdf_load('examples/data/mp-subset.ttl'),
            query(['reproductive system phenotype', 'abnormal cerebellum development', 'abnormal synaptic transmission']).
    

## About

This is an implementation of Bauer et al's Ontology Bayesian Search algorithm for ontologies: 
        
    @article{Bauer2012,
        title = {{Bayesian ontology querying for accurate and noise-tolerant semantic searches.}},
        abstract = {Ontologies provide a structured representation of the concepts of a domain of knowledge as well as the relations between them. Attribute ontologies are used to describe the characteristics of the items of a domain, such as the functions of proteins or the signs and symptoms of disease, which opens the possibility of searching a database of items for the best match to a list of observed or desired attributes. However, naive search methods do not perform well on realistic data because of noise in the data, imprecision in typical queries and because individual items may not display all attributes of the category they belong to.},
        author = {Bauer, Sebastian and K\"{o}hler, Sebastian and Schulz, Marcel H and Robinson, Peter N},
        doi = {10.1093/bioinformatics/bts471},
        issn = {1367-4811},
        journal = {Bioinformatics (Oxford, England)},
        month = Oct,
        number = {19},
        pages = {2502--8},
        pmid = {22843981},
        url = {http://www.ncbi.nlm.nih.gov/pubmed/22843981},
        volume = {28},
        year = {2012}
    }

The implementation is designed to be pluggable with respect to how
underlying prolog data maps to the BN. However, at the moment there is
only one plugin, rdfsboqa, which implements a simple RDF Schema map
using the SWI semweb library:

 * Terms (e.g. Phenotypes) are OWL classes
 * Items (Diseases) are instances (rdf:type) of OWL classes
 * The inference rule is the reflexive transitive closure of rdfs:subClassOf (i.e. rdfs_subclass_of/2)

## TODO

 * Frequency-augmented network
 


