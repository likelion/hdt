:- module(
  hdt_generic,
  [
    hdt_close/1,      % +Hdt
    hdt_create/1,     % +RdfFile
    hdt_create/2,     % +RdfFile, -HdtFile
    hdt_create/3,     % +RdfFile, -HdtFile, +Options
    hdt_deinit/1,     % +Hdt
    hdt_graph/1,      % ?G
    hdt_graph/2,      % ?Hdt, ?G
    hdt_init/2,       % +HdtFile, ?G
    hdt_open/2,       % +HdtFile, -Hdt
    hdt_term_count/3, % +Hdt, +Role, ?Count
    subrole_/2,       % +Role, -Subrole
    subroles_/2       % +Role, -Subroles
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> HDT generic

Code that is shared between the various implementations of the HDT
API.

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09-2017/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml)).

:- use_foreign_library(foreign(hdt4pl)).

:- at_halt(forall(hdt_graph(Hdt, _), hdt_close(Hdt))).

:- dynamic
    hdt_graph/2.

:- rdf_meta
   atom_literal_(+, o),
   hdt_graph(r),
   hdt_graph(?, r),
   hdt_header_(+, r, r, o),
   hdt_init(+, r).





%! hdt_close(+Hdt:blob) is det.
%
% Closes the HDT file by its opaque handle (`Hdt`).

hdt_close(Hdt) :-
  hdt_close_(Hdt).



%! hdt_create(+RdfFile) is det.
%! hdt_create(+RdfFile, ?HdtFile:atom) is det.
%! hdt_create(+RdfFile, ?HdtFile:atom, +Options:list(compound)) is det.
%
% Create an HDT file from an uncompressed N-Triples file.
%
% The following Options are supported:
%
%    * base_uri(+atom)
%
%    The base URI that is used for generating HDT header properties.

hdt_create(RdfFile) :-
  hdt_create(RdfFile, _).


hdt_create(RdfFile, HdtFile) :-
  hdt_create(RdfFile, HdtFile, []).


hdt_create(RdfFile, HdtFile, Options1) :-
  % Determine the serialization format of the RDF file.
  ignore(option(format(Format), Options1)),
  (   ground(Format)
  ->  Options2 = Options1
  ;   once((
        atomic_list_concat(Comps, ., RdfFile),
        member(Ext, Comps),
        extension_format(Ext, Format)
      )),
      merge_options(Options1, [format(Format)], Options2)
  ),

  % The HDT output file.
  (   var(HdtFile)
  ->  directory_file_path(Dir, RdfLocal, RdfFile),
      atomic_list_concat(Segments1, ., RdfLocal),
      % SWI does not auto-detect that this is deterministic :(
      once(append(Segments2, [_], Segments1)),
      atomic_list_concat(Segments2, ., Base),
      file_name_extension(Base, hdt, HdtLocal),
      directory_file_path(Dir, HdtLocal, HdtFile)
  ;   true
  ),
  
  hdt_create_(HdtFile, RdfFile, Options2).

extension_format(n3, n3).
extension_format(nq, nquads).
extension_format(nt, ntriples).
extension_format(ttl, turtle).



%! hdt_deinit(+G:atom) is det.
%
% Closes the HDT file denoted by the named graph `G`.

hdt_deinit(G) :-
  with_mutex(hdt, (
    hdt_graph(Hdt, G),
    retractall(hdt_graph(Hdt, G)),
    hdt_close(Hdt)
  )).



%! hdt_graph(+G:atom) is semidet.
%! hdt_graph(-G:atom) is nondet.

hdt_graph(G) :-
  hdt_graph(_, G).


%! hdt_graph(+Hdt:blob, +G:atom) is semidet.
%! hdt_graph(+Hdt:blob, -G:atom) is semidet.
%! hdt_graph(-Hdt:blob, +G:atom) is semidet.
%! hdt_graph(-Hdt:blob, -G:atom) is nondet.



%! hdt_header_(+Hdt:blob, ?S, ?P, -O) is nondet.

hdt_header_(Hdt, S, P, O) :-
  hdt_triple_(Hdt, header, S, P, Atom),
  atom_object(Atom, O).

atom_object(Atom1, O) :-
  atom_concat('"', Atom2, Atom1), !,
  atom_concat(Lex, '"', Atom2),
  atom_literal(Lex, O).
atom_object(O, O).

atom_literal(Lex, literal(type(D,Lex))) :-
  catch(
    xsd_number_string(N, Lex),
    error(syntax_error(xsd_number),_),
    fail
  ), !,
  (integer(N) -> rdf_equal(D, xsd:integer) ; rdf_equal(D, xsd:float)).
atom_literal(Lex, literal(type(D,Lex))) :-
  catch(
    xsd_time_string(_, D, Lex),
    error(_,_),
    fail
  ), !.
atom_literal(Lex, literal(type(D,Lex))) :-
  rdf_equal(D, xsd:string).



%! hdt_init(+HdtFile:atom, ?G:atom) is det.
%
% Opens the given HDT file (`HdtFile`) and allows it to be denoted by
% the named graph `G`.
%
% @arg HdtFile An atomic denoting a local HDT file.
%
% @arg G An alias by which one can refer to the opaque HDT handle.
%      This alias acts as a name for the graph, or set of triples,
%      that is contained in the HDT file.
%
%      If the graph G is unboud, the URI version of the HDT file name
%      is used.

hdt_init(HdtFile, G) :-
  (var(G) -> uri_file_name(G, HdtFile) ; true),
  hdt_open(HdtFile, Hdt),
  with_mutex(hdt, (
    (   hdt_graph(Hdt, _)
    ->  throw(error(already_exists(hdt_blob, Hdt), _))
    ;   hdt_graph(_, G)
    ->  throw(error(already_exists(hdt_graph, G)))
    ;   assert(hdt_graph(Hdt, G))
    )
  )).



%! hdt_open(+HdtFile:atom, -Hdt:blob) is det.
%! hdt_open(+HdtFile:atom, -Hdt:blob, +Options:list(compound)) is det.
%
% Opens an existing HDT file `HdtFile', and unifies `Hdt' with a
% direct handle to it.
%
% @arg HdtFile An atom denoting a local HDT file.
%
% @arg Hdt An opaque symbol that acts as a handle to an opened HDT
%      files.  This symbol is subject to (atom) garbage collection.
%
% @arg Options The following Options are supported:
%
%      * access(+oneof([load,map]))
%
%      How the file is accessed.  On of `map` (map the file into
%      memory, default) or `load` (load the content of the file).
%
%      * indexed(+boolean)
%
%      Whether or not an index file is created.  Default is `true`.
%
%      An index is needed for partially instantiated calls to hdt/4,
%      but not for retrieving all statements (Triple Pattern 〈?,?,?〉).
%
%      The index is maintained in a file with extension `.index` in
%      the same directory as the HDT file.

hdt_open(HdtFile, Hdt) :-
  hdt_open(HdtFile, Hdt, []).


hdt_open(HdtFile, Hdt, Options) :-
  hdt_open_(HdtFile, Hdt, Options).



%! hdt_term_count(+Hdt:blob, +Role:atom, +N:nonneg) is semidet.
%! hdt_term_count(+Hdt:blob, +Role:atom, -N:nonneg) is det.

% object, predicate, shared, subject
hdt_term_count(Hdt, Role, N) :-
  header_role_property(Role, P), !,
  once(hdt_header_(Hdt, _, P, N0)),
  N0 = N^^_.
% sink
hdt_term_count(Hdt, sink, N) :- !,
  maplist(hdt_term_count(Hdt), [shared,subject], [N1,N2]),
  N is N2 - N1.
% source
hdt_term_count(Hdt, source, N) :- !,
  maplist(hdt_term_count(Hdt), [object,shared], [N1,N2]),
  N is N1 - N2.
% others: node, object, subject, term
hdt_term_count(Hdt, Role, N) :- !,
  subroles_(Role, Subroles),
  maplist(hdt_term_count(Hdt), Subroles, Ns),
  sum_list(Ns, N).

header_role_property(object, '<http://rdfs.org/ns/void#distinctObjects>').
header_role_property(predicate, '<http://rdfs.org/ns/void#properties>').
header_role_property(shared, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>').
header_role_property(subject, '<http://rdfs.org/ns/void#distinctSubjects>').



%! subrole_(+Role1:atom, +Subrole2:atom) is semidet.
%! subrole_(+Role1:atom, -Subrole2:atom) is nondet.

subrole_(node, shared).
subrole_(node, source).
subrole_(node, sink).
subrole_(object, shared).
subrole_(object, sink).
subrole_(subject, shared).
subrole_(subject, source).
subrole_(term, predicate).
subrole_(term, node).



%! subroles_(+Role:atom, -Subroles:ordset(atom)) is det.
%
% @arg Subroles An ordered set of leaf nodes that are sub-roles of
%      Role.

subroles_(Role1, Roles) :-
  aggregate_all(
    set(Role2),
    (
      closure(subrole_, Role1, Role2),
      \+ subrole_(Role2, _)
    ),
    Roles
  ).





% MESSAGES %

:- multifile
    prolog:error_message//1.

prolog:error_message(already_exists(Type,Term)) -->
  ["The "],
  hdt_type(Type),
  [" ‘~w’ already exists."-[Term]].

hdt_type(hdt_blob) -->
  "HDT blob".
hdt_type(hdt_graph) -->
  "HDT graph".
