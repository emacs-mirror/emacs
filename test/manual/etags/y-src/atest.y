%%
exp	:	exp '*' exp
			{ $$.value = $1.value ? $3.value : $5.value;
			  $$.unsignedp = $3.unsignedp || $5.unsignedp; }
	;
