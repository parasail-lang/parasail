if exists("b:current_syntax")
   finish
endif

syn keyword ParaSailKeyword abs abstract all and block class
syn keyword ParaSailKeyword concurrent const continue each
syn keyword ParaSailKeyword end exit extends exports
syn keyword ParaSailKeyword forward func global implements
syn keyword ParaSailKeyword import in interface is lambda locked
syn keyword ParaSailKeyword mod new not null of op optional 
syn keyword ParaSailKeyword or private queued ref rem return reverse
syn keyword ParaSailKeyword separate some type until var
syn keyword ParaSailKeyword with xor
syn keyword ParaSailConditional if then else elsif case
syn keyword ParaSailRepeat  for while loop

syn match ParaSailDelimiter "(\|)\|{\|}\|\[\|\]\|,\|:\|;\|\.\||\|<\|>\|'\|?\|::\|||\|\[\[\|\]\]\|->\|\.\.\|<\.\.\|\.\.<\|<\.\.<"
syn match ParaSailOperator "and=\|xor=\|or=\|<==\|==>\|<=>\|<|=\|\*\*=\|<<=\|>>=\|==\|!=\|=?\|<=\|>=\|\*\*\|<<\|>>\|=>\|:=\|+=\|-=\|\*=\|/=\||=\|+\|-\|*\|/"

syn match ParaSailInteger "\<\d[_0-9]*"
syn match ParaSailInteger "\<0[bB][01][_01]*"
syn match ParaSailInteger "\<0[xX][0-9a-fA-F][_0-9a-fA-F]*"
syn match ParaSailInteger "\<\d[_0-9]*\s*#\s*[0-9a-zA-Z][_0-9a-zA-Z]*\s*#"
syn match ParaSailReal "\<\d[_0-9]*\.[0-9][_0-9]*\([eE][+-]\=\d\+\)\="
syn match ParaSailReal "\<\d[_0-9]*\s*#\s*[0-9a-zA-Z][_0-9a-zA-Z]*\.[_0-9a-zA-Z][_0-9a-zA-Z]*\([eE][+-]\=\d\+\)\=\s*#"

syn match ParaSailCharacter "'[^\\']'"
syn match ParaSailCharacter "'\\[\\'\"nrtf0]'"
syn match ParaSailCharacter "'\\\s*#\s*\x[_0-9a-fA-F]*\s*#\s*'"

syn match ParaSailEnumeration "#[a-zA-Z][_a-zA-Z]*"
syn match ParaSailComment "//.*$"
syn region ParaSailString start="\"" end="\"" contains=ParaSailEscapedCharacter
syn match ParaSailEscapedCharacter "\\[\\'\"nrtf0]" contained display
syn match ParaSailEscapedCharacter "\\\s*#\s*\x[_0-9a-fA-F]*\s*#" contained display

hi link ParaSailClass Function
hi link ParaSailFunc  Function
hi link ParaSailKeyword Keyword
hi link ParaSailDelimiter Delimiter
hi link ParaSailOperator Operator
hi link ParaSailInteger Number
hi link ParaSailReal    Number
hi link ParaSailCharacter Character
hi link ParaSailComment Comment
hi link ParaSailEnumeration Boolean
hi link ParaSailString String
hi link ParaSailEscapedCharacter SpecialChar
hi link ParaSailConditional Conditional
hi link ParaSailRepeat Repeat
