Nonterminals elements element query_list.
Terminals name query.
Rootsymbol elements.

elements   -> '$empty'         : [].
elements   -> element elements : [{query_scope, '$1'} | '$2'].
element    -> name query_list  : {'$1', '$2'}.
query_list -> '$empty'         : [].
query_list -> query query_list : ['$1' | '$2'].
