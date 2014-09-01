-define(MAX_BYTE, 255).
-define(MAX_CHAR, 16#10ffff).

%%-----------------------------------------------------------------------------
%% Type tags and qualifiers
%%

-define(atom_tag,       atom).
-define(binary_tag,     binary).
-define(function_tag,   function).
-define(identifier_tag, identifier).
-define(list_tag,       list).
-define(matchstate_tag, matchstate).
-define(nil_tag,        nil).
-define(number_tag,     number).
-define(opaque_tag,     opaque).
-define(product_tag,    product).
-define(remote_tag,     remote).
-define(tuple_set_tag,  tuple_set).
-define(tuple_tag,      tuple).
-define(union_tag,      union).
-define(var_tag,        var).

-type tag()  :: ?atom_tag | ?binary_tag | ?function_tag | ?identifier_tag
              | ?list_tag | ?matchstate_tag | ?nil_tag | ?number_tag
              | ?opaque_tag | ?product_tag | ?tuple_tag | ?tuple_set_tag
              | ?union_tag | ?var_tag.

-define(float_qual,     float).
-define(integer_qual,   integer).
-define(nonempty_qual,  nonempty).
-define(pid_qual,       pid).
-define(port_qual,      port).
-define(reference_qual, reference).
-define(unknown_qual,   unknown).

-type qual() :: ?float_qual | ?integer_qual | ?nonempty_qual | ?pid_qual
              | ?port_qual | ?reference_qual | ?unknown_qual | {_, _}.

%%-----------------------------------------------------------------------------
%% The type representation
%%

-define(any,  any).
-define(none, none).
-define(unit, unit).
%% Generic constructor - elements can be many things depending on the tag.
-record(c, {tag			      :: tag(),
	    elements  = []	      :: term(),
	    qualifier = ?unknown_qual :: qual()}).

-opaque erl_type() :: ?any | ?none | ?unit | #c{}.

%%-----------------------------------------------------------------------------
%% Auxiliary types and convenient macros
%%

-type parse_form() :: {atom(), _, _} | {atom(), _, _, _} | {'op', _, _, _, _}. %% XXX: Temporarily
-type rng_elem()   :: 'pos_inf' | 'neg_inf' | integer().

-record(int_set, {set :: [integer()]}).
-record(int_rng, {from :: rng_elem(), to :: rng_elem()}).
-record(opaque,  {mod :: module(), name :: atom(),
		  args = [] :: [erl_type()], struct :: erl_type()}).
-record(remote,  {mod:: module(), name :: atom(), args = [] :: [erl_type()]}).

-define(atom(Set),                 #c{tag=?atom_tag, elements=Set}).
-define(bitstr(Unit, Base),        #c{tag=?binary_tag, elements=[Unit,Base]}).
-define(float,                     ?number(?any, ?float_qual)).
-define(function(Domain, Range),   #c{tag=?function_tag, 
				      elements=[Domain, Range]}).
-define(identifier(Types),         #c{tag=?identifier_tag, elements=Types}).
-define(integer(Types),            ?number(Types, ?integer_qual)).
-define(int_range(From, To),       ?integer(#int_rng{from=From, to=To})).
-define(int_set(Set),              ?integer(#int_set{set=Set})).
-define(list(Types, Term, Size),   #c{tag=?list_tag, elements=[Types,Term],
				      qualifier=Size}).
-define(nil,                       #c{tag=?nil_tag}).
-define(nonempty_list(Types, Term),?list(Types, Term, ?nonempty_qual)).
-define(number(Set, Qualifier),    #c{tag=?number_tag, elements=Set, 
				      qualifier=Qualifier}).
-define(opaque(Optypes),           #c{tag=?opaque_tag, elements=Optypes}).
-define(product(Types),            #c{tag=?product_tag, elements=Types}).
-define(remote(RemTypes),          #c{tag=?remote_tag, elements=RemTypes}).
-define(tuple(Types, Arity, Qual), #c{tag=?tuple_tag, elements=Types, 
				      qualifier={Arity, Qual}}).
-define(tuple_set(Tuples),         #c{tag=?tuple_set_tag, elements=Tuples}).
-define(var(Id),                   #c{tag=?var_tag, elements=Id}).

-define(matchstate(P, Slots),	   #c{tag=?matchstate_tag, elements=[P,Slots]}).
-define(any_matchstate,            ?matchstate(t_bitstr(), ?any)).

-define(byte,                      ?int_range(0, ?MAX_BYTE)).
-define(char,                      ?int_range(0, ?MAX_CHAR)).
-define(integer_pos,               ?int_range(1, pos_inf)).
-define(integer_non_neg,           ?int_range(0, pos_inf)).
-define(integer_neg,               ?int_range(neg_inf, -1)).

%%-----------------------------------------------------------------------------
%% Unions
%%

-define(union(List), #c{tag=?union_tag, elements=[_,_,_,_,_,_,_,_,_,_]=List}).

-define(atom_union(T),       ?union([T,?none,?none,?none,?none,?none,?none,?none,?none,?none])).
-define(bitstr_union(T),     ?union([?none,T,?none,?none,?none,?none,?none,?none,?none,?none])).
-define(function_union(T),   ?union([?none,?none,T,?none,?none,?none,?none,?none,?none,?none])).
-define(identifier_union(T), ?union([?none,?none,?none,T,?none,?none,?none,?none,?none,?none])).
-define(list_union(T),       ?union([?none,?none,?none,?none,T,?none,?none,?none,?none,?none])).
-define(number_union(T),     ?union([?none,?none,?none,?none,?none,T,?none,?none,?none,?none])).
-define(tuple_union(T),      ?union([?none,?none,?none,?none,?none,?none,T,?none,?none,?none])).
-define(matchstate_union(T), ?union([?none,?none,?none,?none,?none,?none,?none,T,?none,?none])).
-define(opaque_union(T),     ?union([?none,?none,?none,?none,?none,?none,?none,?none,T,?none])).
-define(remote_union(T),     ?union([?none,?none,?none,?none,?none,?none,?none,?none,?none,T])).
-define(integer_union(T),    ?number_union(T)).
-define(float_union(T),      ?number_union(T)).
-define(nil_union(T),        ?list_union(T)).


