:- module(
  hdt_generic,
  [
    hdt/1,              % ?Hdt
    hdt_close/1,        % +Hdt
    hdt_create/1,       % +RdfFile
    hdt_create/2,       % +RdfFile, -HdtFile
    hdt_create/3,       % +RdfFile, -HdtFile, +Options
    hdt_deinit/0,
    hdt_deinit/1,       % +G
    hdt_graph/1,        % ?G
    hdt_graph/2,        % ?Hdt, ?G
    hdt_init/1,         % +HdtFile
    hdt_init/2,         % +HdtFile, ?G
    hdt_open/2,         % +HdtFile, -Hdt
    hdt_property/2,     % +Hdt, ?Property
    hdt_term_count/3,   % +Hdt, +Role, ?Count
    role_leafrole/2,    % +Role, -LeafRole
    role_subrole/2      % +Role, -SubRole
  ]
).

/** <module> HDT generic

Code that is shared between the two HDT APIs.

@author Wouter Beek
@author Jan Wielemaker
@version 2017/09-2017/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(closure)).
:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- use_foreign_library(foreign(hdt4pl)).

:- at_halt(forall(hdt_graph_(Hdt, _), hdt_close(Hdt))).

:- dynamic
    hdt_graph_/2.

:- rdf_meta
   atom_literal_(+, o),
   hdt_graph(r),
   hdt_graph(?, r),
   hdt_init(+, r).





%! hdt(?Hdt:blob) is nondet.

hdt(Hdt) :-
  hdt_graph(Hdt, _).



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



%! hdt_deinit is det.
%! hdt_deinit(+G:atom) is det.
%
% Closes the HDT file denoted by the named graph `G`.

hdt_deinit :-
  forall(hdt_graph(G), hdt_deinit(G)).


hdt_deinit(G) :-
  with_mutex(hdt, (
    (   hdt_graph_(Hdt, G)
    ->  retractall(hdt_graph_(Hdt, G)),
        hdt_close(Hdt)
    ;   existence_error(hdt_graph, G)
    )
  )).



%! hdt_graph(+G:atom) is semidet.
%! hdt_graph(-G:atom) is nondet.

hdt_graph(G) :-
  hdt_graph(_, G).


%! hdt_graph(+Hdt:blob, +G:atom) is semidet.
%! hdt_graph(+Hdt:blob, -G:atom) is semidet.
%! hdt_graph(-Hdt:blob, +G:atom) is semidet.
%! hdt_graph(-Hdt:blob, -G:atom) is nondet.

hdt_graph(Hdt, G) :-
  ground(Hdt), !,
  once(hdt_graph_(Hdt, G)).
hdt_graph(Hdt, G) :-
  ground(G), !,
  once(hdt_graph_(Hdt, G)).
hdt_graph(Hdt, G) :-
  hdt_graph_(Hdt, G).



%! hdt_init(+HdtFile:atom) is det.
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

hdt_init(HdtFile) :-
  rdf_default_graph(G),
  hdt_init(HdtFile, G).


hdt_init(HdtFile, G) :-
  (var(G) -> uri_file_name(G, HdtFile) ; true),
  hdt_open(HdtFile, Hdt),
  with_mutex(hdt, (
    (   hdt_graph_(Hdt, _)
    ->  throw(error(already_exists(hdt_blob, Hdt), _))
    ;   hdt_graph_(_, G)
    ->  throw(error(already_exists(hdt_graph, G)))
    ;   assert(hdt_graph_(Hdt, G))
    )
  )).



hdt_literal1(Literal0) -->
  "\"",
  string(Lex0),
  "\"",
  hdt_literal2(Lex0, Literal0).

hdt_literal2(Lex0, literal(type(D0,Lex0))) -->
  "^^<",
  string_without("\">", D0),
  ">".
hdt_literal2(Lex0, literal(lang(LTag0,Lex0))) -->
  "@",
  string_without("\"", LTag0).
hdt_literal2(Lex0, literal(Lex0)) --> "".



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



%! hdt_property(+Hdt:blob, +Property:compound) is semidet.
%! hdt_property(+Hdt:blob, -Property:compound) is nondet.
%
%  True of Property is a property of HTD.  Defined properties are
%
%    * elements(-Count))
%    * mapping(-Mapping)
%    * max_id(-Id))
%    * max_object_id(-Id))
%    * max_predicate_id(-Id))
%    * max_subject_id(-Id))
%    * objects(-Count))
%    * predicates(-Count))
%    * shared(-Count))
%    * subjects(-Count))

hdt_property(Hdt, Property) :-
  hdt_property(Property),
  hdt_property_(Hdt, Property).

hdt_property(elements(_)).
hdt_property(mapping(_)).
hdt_property(max_id(_)).
hdt_property(max_object_id(_)).
hdt_property(max_predicate_id(_)).
hdt_property(max_subject_id(_)).
hdt_property(objects(_)).
hdt_property(predicates(_)).
hdt_property(shared(_)).
hdt_property(subjects(_)).



%! hdt_term_count(+Hdt:blob, +Role:atom, +N:nonneg) is semidet.
%! hdt_term_count(+Hdt:blob, +Role:atom, -N:nonneg) is det.

% object, predicate, shared, subject
hdt_term_count(Hdt, Role, N) :-
  header_role_property(Role, P), !,
  once(hdt_triple_(Hdt, header, _, P, Atom1)),
  atom_concat('"', Atom2, Atom1),
  atom_concat(Lex, '"', Atom2),
  atom_number(Lex, N).
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
  aggregate_all(set(LeafRole), role_leafrole(Role, LeafRole), LeafRoles),
  maplist(hdt_term_count(Hdt), LeafRoles, Ns),
  sum_list(Ns, N).

header_role_property(object, '<http://rdfs.org/ns/void#distinctObjects>').
header_role_property(predicate, '<http://rdfs.org/ns/void#properties>').
header_role_property(shared, '<http://purl.org/HDT/hdt#dictionarynumSharedSubjectObject>').
header_role_property(subject, '<http://rdfs.org/ns/void#distinctSubjects>').



%! leafrole(+Role:atom) is semidet.
%! leafrole(-Role:atom) is multi.

leafrole(Role) :-
  role_subrole(_, Role),
  \+ role_subrole(Role, _).



%! role_leafrole(+Role:atom, -LeafRole:atom) is nondet.

role_leafrole(Role, SubRole) :-
  closure0(role_subrole, Role, SubRole),
  leafrole(SubRole).



%! role_subrole(+Role:atom, +SubRole:atom) is semidet.
%! role_subrole(+Role:atom, -SubRole:atom) is nondet.

role_subrole(node, shared).
role_subrole(node, source).
role_subrole(node, sink).
role_subrole(object, shared).
role_subrole(object, sink).
role_subrole(subject, shared).
role_subrole(subject, source).
role_subrole(term, predicate).
role_subrole(term, node).



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
