Formal BNF for CaCa:

<Program> ::= start <stmnts>  end
<stmnts> ::= <fromget>;  as <vars>| <fromget>; where { <equals> } as <vars> 
<fromget> ::= from relation_symbol get <to_get> and <fromget> 
                      | from relation_symbol get <to_get> 
<equals> ::= var = var; |var = var; <equals>
<some> ::= some String; 
<to_get> ::= <some> | <var> | <to_get>, <to_get>
<vars> ::= var | var,<vars>
relation_symbol ::= A..Z
var ::= String

