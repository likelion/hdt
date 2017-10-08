:- multifile
    error:has_type/2.

error:has_type(hdt_graph, G) :-
  error:has_type(atom, G).
error:has_type(hdt_graph, G) :-
  error:has_type(blob, G).

error:has_type(hdt_id, id(Role,N)) :-
  error:has_type(hdt_role, Role),
  error:has_type(positive_integer, N).

error:has_type(hdt_role, Role) :-
  error:has_type(oneof([predicate,shared,sink,source]), Role).

error:has_type(rdf_bnode, BNode) :-
  error:has_type(atom, BNode).

error:has_type(rdf_iri, Iri) :-
  error:has_type(atom, Iri).

error:has_type(rdf_literal, _^^D) :-
  error:has_type(rdf_iri, D).
error:has_type(rdf_literal, Lex@D) :-
  error:has_type(string, Lex),
  error:has_type(rdf_iri, D).

error:has_type(rdf_role, Role) :-
  error:has_type(hdt_role, Role).
error:has_type(rdf_role, Role) :-
  error:has_type(oneof([bnode,iri,literal,name,node,object,subject,term]), Role).

error:has_type(rdf_term, Term) :-
  error:has_type(rdf_bnode, Term).
error:has_type(rdf_term, Term) :-
  error:has_type(rdf_iri, Term).
error:has_type(rdf_term, Term) :-
  error:has_type(rdf_literal, Term).
