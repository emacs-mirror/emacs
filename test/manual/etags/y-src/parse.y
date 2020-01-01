%{
/*	Copyright (C) 1990, 1992-1993, 2016-2020 Free Software Foundation,
 *	Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
%}


%right '?' ':'
/* %left '|' */
%left '&'
%nonassoc '=' NE
%nonassoc '<' LE '>' GE
%left '+' '-'
%left '*' '/' '%'
%right '^'
%left NEG '!'

%token	L_CELL L_RANGE
%token	L_VAR

%token	L_CONST
%token	L_FN0	L_FN1	L_FN2	L_FN3	L_FN4	L_FNN
%token	L_FN1R	L_FN2R	L_FN3R	L_FN4R	L_FNNR

%token	L_LE	L_NE	L_GE

%{
#include "funcdef.h"

#include <ctype.h>

#define obstack_chunk_alloc ck_malloc
#define obstack_chunk_free free
#include "obstack.h"
#include "sysdef.h"

#include "global.h"
#include "errors.h"
#include "node.h"
#include "eval.h"
#include "ref.h"

int yylex ();
#ifdef __STDC__
void yyerror (char *);
#else
void yyerror ();
#endif
VOIDSTAR parse_hash;
extern VOIDSTAR hash_find();

/* This table contains a list of the infix single-char functions */
unsigned char fnin[] = {
	SUM, DIFF, DIV, PROD, MOD, /* AND, OR, */ POW, EQUAL, IF, CONCAT, 0
};

#define YYSTYPE _y_y_s_t_y_p_e
typedef struct node *YYSTYPE;
YYSTYPE parse_return;
#ifdef __STDC__
YYSTYPE make_list (YYSTYPE, YYSTYPE);
#else
YYSTYPE make_list ();
#endif

char *instr;
int parse_error = 0;
extern struct obstack tmp_mem;

%}
%%
line:	exp
		{ parse_return=$1; }
	| error {
		if(!parse_error)
			parse_error=PARSE_ERR;
		parse_return=0; }
	;

exp:	  L_CONST
	| cell
	| L_FN0 '(' ')' {
		$$=$1; }
	| L_FN1 '(' exp ')' {
		($1)->n_x.v_subs[0]=$3;
		($1)->n_x.v_subs[1]=(struct node *)0;
		$$=$1; }
	| L_FN2 '(' exp ',' exp ')' {
		($1)->n_x.v_subs[0]=$3;
		($1)->n_x.v_subs[1]=$5;
		$$=$1; }
	| L_FN3 '(' exp ',' exp ',' exp ')' {
		($1)->n_x.v_subs[0]=make_list($3,$5);
 		($1)->n_x.v_subs[1]=$7;
 		$$=$1;}
	| L_FN4 '(' exp ',' exp ',' exp ',' exp ')' {
		($1)->n_x.v_subs[0]=make_list($3,$5);
 		($1)->n_x.v_subs[1]=make_list($7,$9);
 		$$=$1;}
	| L_FNN '(' exp_list ')' {
		($1)->n_x.v_subs[0]=(struct node *)0;
		($1)->n_x.v_subs[1]=$3;
		$$=$1; }
	| L_FN1R '(' L_RANGE ')' {
		$1->n_x.v_subs[0]=$3;
		$$=$1; }
	| L_FN1R '(' L_VAR ')' {
		$1->n_x.v_subs[0]=$3;
		$$=$1; }

	| L_FN2R '(' L_RANGE ',' exp ')' {
		$1->n_x.v_subs[0]=$3;
		$1->n_x.v_subs[1]=$5;
		$$=$1; }
	| L_FN2R '(' L_VAR ',' exp ')' {
		$1->n_x.v_subs[0]=$3;
		$1->n_x.v_subs[1]=$5;
		$$=$1; }

	/* JF:  These should be FN2R, but I'm hacking this for SYLNK */
	| L_FN2R '(' L_RANGE ',' exp ',' exp ')' {
		if($1->comp_value!=F_INDEX)
			parse_error=PARSE_ERR;
		$1->comp_value=F_INDEX2;
		$1->n_x.v_subs[0]=make_list($3,$5);
		$1->n_x.v_subs[1]=$7;
		$$=$1; }
	| L_FN2R '(' L_VAR ',' exp ',' exp ')' {
		if($1->comp_value!=F_INDEX)
			parse_error=PARSE_ERR;
		$1->comp_value=F_INDEX2;
		$1->n_x.v_subs[0]=make_list($3,$5);
		$1->n_x.v_subs[1]=$7;
		$$=$1; }

	| L_FN3R '(' L_RANGE ',' exp ',' exp ')' {
		($1)->n_x.v_subs[0]=make_list($3,$5);
 		($1)->n_x.v_subs[1]=$7;
 		$$=$1;}
	| L_FN3R '(' L_VAR ',' exp ',' exp ')' {
		($1)->n_x.v_subs[0]=make_list($3,$5);
 		($1)->n_x.v_subs[1]=$7;
 		$$=$1;}

	| L_FNNR '(' range_exp_list ')' {
		($1)->n_x.v_subs[0]=(struct node *)0;
		($1)->n_x.v_subs[1]=$3;
		$$=$1; }
	| exp '?' exp ':' exp {
		$2->comp_value=IF;
		$2->n_x.v_subs[0]=$4;
		$2->n_x.v_subs[1]=$5;
		$4->n_x.v_subs[0]=$1;
		$4->n_x.v_subs[1]=$3;
		$$=$2; }
	/* | exp '|' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; } */
	| exp '&' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '<' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp LE exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '=' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp NE exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '>' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp GE exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '+' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '-' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '*' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '/' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '%' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| exp '^' exp {
		$2->n_x.v_subs[0]=$1;
		$2->n_x.v_subs[1]=$3;
		$$ = $2; }
	| '-' exp %prec NEG {
		if($2->comp_value==CONST_FLT) {
			$2->n_x.v_float= -($2->n_x.v_float);
			/* free($1); */
			$$=$2;
		} else if($2->comp_value==CONST_INT) {
			$2->n_x.v_int= -($2->n_x.v_int);
			/* free($1); */
			$$=$2;
		} else {
			$1->comp_value = NEGATE;
			$1->n_x.v_subs[0]=$2;
			$1->n_x.v_subs[1]=(struct node *)0;
			$$ = $1;
		} }
	| '!' exp {
		$1->n_x.v_subs[0]=$2;
		$1->n_x.v_subs[1]=(struct node *)0;
		$$ = $1; }
	| '(' exp ')'
		{ $$ = $2; }
	| '(' exp error {
		if(!parse_error)
			parse_error=NO_CLOSE;
		}
	/* | exp ')' error {
		if(!parse_error)
			parse_error=NO_OPEN;
		} */
	| '(' error {
		if(!parse_error)
			parse_error=NO_CLOSE;
		}
	;


exp_list: exp
 		{ $$ = make_list($1, 0); }
	| exp_list ',' exp
		{ $$ = make_list($3, $1); }
	;

range_exp: L_RANGE
	| exp
	;

range_exp_list: range_exp
		{ $$=make_list($1, 0); }
	|   range_exp_list ',' range_exp
		{ $$=make_list($3,$1); }
	;

cell:	L_CELL
		{ $$=$1; }
	| L_VAR
	;
%%

void
yyerror FUN1(char *, s)
{
	if(!parse_error)
		parse_error=PARSE_ERR;
}

YYSTYPE
make_list FUN2(YYSTYPE, car, YYSTYPE, cdr)
{
	YYSTYPE ret;

	ret=(YYSTYPE)obstack_alloc(&tmp_mem,sizeof(*ret));
	ret->comp_value = 0;
	ret->n_x.v_subs[0]=car;
	ret->n_x.v_subs[1]=cdr;
	return ret;
}

#define ERROR -1

extern struct node *yylval;

#ifdef __STDC__
unsigned char parse_cell_or_range (char **,struct rng *);
#else
unsigned char parse_cell_or_range ();
#endif

int
yylex FUN0()
{
	int ch;
	struct node *new;
	int isflt;
	char *begin;
	char *tmp_str;
	unsigned char byte_value;
	int n;

	/* unsigned char *ptr; */
	int nn;
	struct function *fp;
	int tmp_ch;

#ifdef TEST
	if(!instr)
		return ERROR;
#endif
	while(isspace(*instr))
		instr++;
	ch = *instr++;
	if(ch=='(' || ch==',' || ch==')')
		return ch;

	new=(struct node *)obstack_alloc(&tmp_mem,sizeof(struct node));
	new->add_byte=0;
	new->sub_value=0;
	switch(ch) {
	case 0:
		return 0;

	case '0': case '1': case '2': case '3': case '4': case '5': case '6':
	case '7': case '8': case '9': case '.':
		isflt = (ch=='.');

		begin=instr-1;
		tmp_str=instr;

		while(isdigit(*tmp_str) || (!isflt && *tmp_str=='.' && ++isflt))
			tmp_str++;
		if(*tmp_str=='e' || *tmp_str=='E') {
			isflt=1;
			tmp_str++;
			if(*tmp_str=='-' || *tmp_str=='+')
				tmp_str++;
			while(isdigit(*tmp_str))
				tmp_str++;
		}
		if(isflt) {
			new->n_x.v_float=astof((char **)(&begin));
			byte_value=CONST_FLT;
		} else {
			new->n_x.v_int=astol((char **)(&begin));
			if(begin!=tmp_str) {
				begin=instr-1;
				new->n_x.v_float=astof((char **)(&begin));
				byte_value=CONST_FLT;
			} else
				byte_value=CONST_INT;
		}
		ch=L_CONST;
		instr=begin;
		break;

	case '"':
		begin=instr;
		while(*instr && *instr!='"') {
			if(*instr=='\\' && instr[1])
				instr++;
			instr++;
		}
		if(!*instr) {
			parse_error=NO_QUOTE;
			return ERROR;
		}
		tmp_str=new->n_x.v_string=(char *)ck_malloc(1+instr-begin);
		while(begin!=instr) {
			unsigned char n;

			if(*begin=='\\') {
				begin++;
				if(begin[0]>='0' && begin[0]<='7') {
					if(begin[1]>='0' && begin[1]<='7') {
						if(begin[2]>='0' && begin[2]<='7') {
							n=(begin[2]-'0') + (010 * (begin[1]-'0')) + ( 0100 * (begin[0]-'0'));
							begin+=3;
						} else {
							n=(begin[1]-'0') + (010 * (begin[0]-'0'));
							begin+=2;
						}
					} else {
						n=begin[0]-'0';
						begin++;
					}
				} else
					n= *begin++;
				*tmp_str++= n;
			} else
				*tmp_str++= *begin++;
		}
		*tmp_str='\0';
		instr++;
		byte_value=CONST_STR;
		ch=L_CONST;
		break;

	case '+':	case '-':

	case '*':	case '/':	case '%':	case '&':
	/* case '|': */	case '^':	case '=':

	case '?':
	{
		unsigned char *ptr;

		for(ptr= fnin;*ptr;ptr++)
			if(the_funs[*ptr].fn_str[0]==ch)
				break;
#ifdef TEST
		if(!*ptr)
			panic("Can't find fnin[] entry for '%c'",ch);
#endif
		byte_value= *ptr;
	}
		break;

	case ':':
		byte_value=IF;
		break;

	case '!':
	case '<':
	case '>':
		if(*instr!='=') {
			byte_value = (ch=='<') ? LESS : (ch=='>') ? GREATER : NOT;
			break;
		}
		instr++;
		byte_value = (ch=='<') ? LESSEQ : (ch=='>') ? GREATEQ : NOTEQUAL;
		ch = (ch=='<') ? LE : (ch=='>') ? GE : NE;
		break;

	case '\'':
	case ';':
	case '[':
	case '\\':
	case ']':
	case '`':
	case '{':
	case '}':
	case '~':
	bad_chr:
		parse_error=BAD_CHAR;
		return ERROR;

	case '#':
		begin=instr-1;
		while(*instr && (isalnum(*instr) || *instr=='_'))
			instr++;
		ch= *instr;
		*instr=0;
		if(!stricmp(begin,tname))
			byte_value=F_TRUE;
		else if(!stricmp(begin,fname))
			byte_value=F_FALSE;
		else if(!stricmp(begin,iname) && (begin[4]==0 || !stricmp(begin+4,"inity")))
			byte_value=CONST_INF;
		else if(!stricmp(begin,mname) ||
			!stricmp(begin,"#ninf"))
			byte_value=CONST_NINF;
		else if(!stricmp(begin,nname) ||
			!stricmp(begin,"#nan"))
			byte_value=CONST_NAN;
		else {
			for(n=1;n<=ERR_MAX;n++)
				if(!stricmp(begin,ename[n]))
					break;
			if(n>ERR_MAX)
				n=BAD_CHAR;
			new->n_x.v_int=n;
			byte_value=CONST_ERR;
		}
		*instr=ch;
		ch=L_CONST;
		break;

	default:
		if(!a0 && (ch=='@' || ch=='$'))
		   goto bad_chr;

		if(a0 && ch=='@') {
			begin=instr;
			while(*instr && (isalpha(*instr) || isdigit(*instr) || *instr=='_'))
				instr++;
			n=instr-begin;
		} else {
			begin=instr-1;
			byte_value=parse_cell_or_range(&begin,&(new->n_x.v_rng));
			if(byte_value) {
				if((byte_value& ~0x3)==R_CELL)
					ch=L_CELL;
				else
					ch=L_RANGE;
				instr=begin;
				break;
			}

			while(*instr && (isalpha(*instr) || isdigit(*instr) || *instr=='_'))
				instr++;

			n=instr-begin;
			while(isspace(*instr))
				instr++;

			if(*instr!='(') {
				ch=L_VAR;
				byte_value=VAR;
				new->n_x.v_var=find_or_make_var(begin,n);
				break;
			}
		}
		tmp_ch=begin[n];
		begin[n]='\0';
		fp=hash_find(parse_hash,begin);
		begin[n]=tmp_ch;
		byte_value= ERROR;
		if(!fp) {
			parse_error=BAD_FUNC;
			return ERROR;
		}

		if(fp>=the_funs && fp<=&the_funs[USR1])
			byte_value=fp-the_funs;
		else {
			for(nn=0;nn<n_usr_funs;nn++) {
				if(fp>=&usr_funs[nn][0] && fp<=&usr_funs[nn][usr_n_funs[nn]]) {
					byte_value=USR1+nn;
					new->sub_value=fp-&usr_funs[nn][0];
					break;
				}
			}
#ifdef TEST
			if(nn==n_usr_funs) {
				io_error_msg("Couln't turn fp into a ##");
				parse_error=BAD_FUNC;
				return ERROR;
			}
#endif
		}

		if(fp->fn_argn&X_J)
			ch= byte_value==F_IF ? L_FN3 : L_FN2;
		else if(fp->fn_argt[0]=='R' || fp->fn_argt[0]=='E')
			ch=L_FN1R-1+fp->fn_argn-X_A0;
		else
			ch=L_FN0 + fp->fn_argn-X_A0;

		break;
	}
	/* new->node_type=ch; */
	new->comp_value=byte_value;
	yylval=new;
	return ch;
}

/* Return value is
	0 if it doesn't look like a cell or a range,
	R_CELL if it is a cell (ptr now points past the characters, lr and lc hold the row and col of the cell)
	RANGE if it is a range (ptr points past the chars)
 */
unsigned char
parse_cell_or_range FUN2(char **,ptr, struct rng *,retp)
{
	if(a0) {
		unsigned tmpc,tmpr;
		char *p;
		int abz = ROWREL|COLREL;

		p= *ptr;
		tmpc=0;
		if(*p=='$') {
			abz-=COLREL;
			p++;
		}
		if(!isalpha(*p))
			return 0;
		tmpc=str_to_col(&p);
		if(tmpc<MIN_COL || tmpc>MAX_COL)
			return 0;
		if(*p=='$') {
			abz-=ROWREL;
			p++;
		}
		if(!isdigit(*p))
			return 0;
		for(tmpr=0;isdigit(*p);p++)
			tmpr=tmpr*10 + *p - '0';

		if(tmpr<MIN_ROW || tmpr>MAX_ROW)
			return 0;

		if(*p==':' || *p=='.') {
			unsigned tmpc1,tmpr1;

			abz = ((abz&COLREL) ? LCREL : 0)|((abz&ROWREL) ? LRREL : 0)|HRREL|HCREL;
			p++;
			if(*p=='$') {
				abz-=HCREL;
				p++;
			}
			if(!isalpha(*p))
				return 0;
			tmpc1=str_to_col(&p);
			if(tmpc1<MIN_COL || tmpc1>MAX_COL)
				return 0;
			if(*p=='$') {
				abz-=HRREL;
				p++;
			}
			if(!isdigit(*p))
				return 0;
			for(tmpr1=0;isdigit(*p);p++)
				tmpr1=tmpr1*10 + *p - '0';
			if(tmpr1<MIN_ROW || tmpr1>MAX_ROW)
				return 0;

			if(tmpr<tmpr1) {
				retp->lr=tmpr;
				retp->hr=tmpr1;
			} else {
				retp->lr=tmpr1;
				retp->hr=tmpr;
			}
			if(tmpc<tmpc1) {
				retp->lc=tmpc;
				retp->hc=tmpc1;
			} else {
				retp->lc=tmpc1;
				retp->hc=tmpc;
			}
			*ptr= p;
			return RANGE | abz;
		}
		retp->lr = retp->hr = tmpr;
		retp->lc = retp->hc = tmpc;
		*ptr=p;
		return R_CELL | abz;
	} else {
		char *p;
		unsigned char retr;
		unsigned char retc;
		int ended;
		long num;
		CELLREF tmp;

#define CK_ABS_R(x)	if((x)<MIN_ROW || (x)>MAX_ROW)	\
				return 0;		\
			else

#define CK_REL_R(x)	if(   ((x)>0 && MAX_ROW-(x)<cur_row)	\
			   || ((x)<0 && MIN_ROW-(x)>cur_row))	\
				return 0;			\
			else

#define CK_ABS_C(x)	if((x)<MIN_COL || (x)>MAX_COL)	\
				return 0;		\
			else

#define CK_REL_C(x)	if(   ((x)>0 && MAX_COL-(x)<cur_col)	\
			   || ((x)<0 && MIN_COL-(x)>cur_col))	\
				return 0;			\
			else

#define MAYBEREL(p) (*(p)=='[' && (isdigit((p)[1]) || (((p)[1]=='+' || (p)[1]=='-') && isdigit((p)[2]))))

		p= *ptr;
		retr=0;
		retc=0;
		ended=0;
		while(ended==0) {
			switch(*p) {
			case 'r':
			case 'R':
				if(retr) {
					ended++;
					break;
				}
				p++;
				retr=R_CELL;
				if(isdigit(*p)) {
					num=astol(&p);
					CK_ABS_R(num);
					retp->lr= retp->hr=num;
				} else if(MAYBEREL(p)) {
					p++;
					num=astol(&p);
					CK_REL_R(num);
					retp->lr= retp->hr=num+cur_row;
					retr|=ROWREL;
					if(*p==':') {
						retr=RANGE|LRREL|HRREL;
						p++;
						num=astol(&p);
						CK_REL_R(num);
						retp->hr=num+cur_row;
					}
					if(*p++!=']')
						return 0;
				} else if(retc || *p=='c' || *p=='C') {
					retr|=ROWREL;
					retp->lr= retp->hr=cur_row;
				} else
					return 0;
				if(*p==':' && retr!=(RANGE|LRREL|HRREL)) {
					retr= (retr&ROWREL) ? RANGE|LRREL : RANGE;
					p++;
					if(isdigit(*p)) {
						num=astol(&p);
						CK_ABS_R(num);
			 			retp->hr=num;
					} else if(MAYBEREL(p)) {
						p++;
						num=astol(&p);
						CK_REL_R(num);
						retp->hr=num+cur_row;
						retr|=HRREL;
						if(*p++!=']')
							return 0;
					} else
						return 0;
				}

				if(retc)
					ended++;
				break;

			case 'c':
			case 'C':
				if(retc) {
					ended++;
					break;
				}
				p++;
				retc=R_CELL;
				if(isdigit(*p)) {
					num=astol(&p);
					CK_ABS_C(num);
					retp->lc= retp->hc=num;
				} else if(MAYBEREL(p)) {
					p++;
					num=astol(&p);
					CK_REL_C(num);
					retp->lc= retp->hc=num+cur_col;
					retc|=COLREL;
					if(*p==':') {
						retc=RANGE|LCREL|HCREL;
						p++;
						num=astol(&p);
						CK_REL_C(num);
						retp->hc=num+cur_col;
					}
					if(*p++!=']')
						return 0;
				} else if(retr || *p=='r' || *p=='R') {
					retc|=COLREL;
					retp->lc= retp->hc=cur_col;
				} else
					return 0;
				if(*p==':' && retc!=(RANGE|LCREL|HCREL)) {
					retc= (retc&COLREL) ? RANGE|LCREL : RANGE;
					p++;
					if(isdigit(*p)) {
						num=astol(&p);
						CK_ABS_C(num);
			 			retp->hc=num;
					} else if(MAYBEREL(p)) {
						p++;
						num=astol(&p);
						CK_REL_C(num);
						retp->hc=num+cur_col;
						retc|=HCREL;
						if(*p++!=']')
							return 0;
					} else
						return 0;
				}

				if(retr)
					ended++;
				break;
			default:
				if(retr) {
					*ptr=p;
					retp->lc=MIN_COL;
					retp->hc=MAX_COL;
					if((retr|ROWREL)==(R_CELL|ROWREL))
						return (retr&ROWREL) ? (RANGE|LRREL|HRREL) : RANGE;
					else
						return retr;
				} else if(retc) {
					*ptr=p;
					retp->lr=MIN_ROW;
					retp->hr=MAX_COL;
					if((retc|COLREL)==(R_CELL|COLREL))
						return (retc&COLREL) ? (RANGE|LCREL|HCREL) : RANGE;
					else
						return retc;
				}
				return 0;
			}
		}
		if(!retr || !retc)
			return 0;
		*ptr=p;
		if(retp->lr>retp->hr)
			tmp=retp->lr,retp->lr=retp->hr,retp->hr=tmp;
		if(retp->lc>retp->hc)
			tmp=retp->lc,retp->lc=retp->hc,retp->hc=tmp;

		if((retr|ROWREL)==(R_CELL|ROWREL)) {
			if((retc|COLREL)==(R_CELL|COLREL))
				return retr|retc;
			return (retr&ROWREL) ? (retc|LRREL|HRREL) : retc;
		}
		if((retc|COLREL)==(R_CELL|COLREL))
			return (retc&COLREL) ? (retr|LCREL|HCREL) : retr;
		return retr|retc;
	}
}

int
str_to_col FUN1(char **,str)
{
	int ret;
	char c,cc,ccc;
#if MAX_COL>702
	char cccc;
#endif

	ret=0;
	c=str[0][0];
	if(!isalpha((cc=str[0][1]))) {
		(*str)++;
		return MIN_COL + (isupper(c) ? c-'A' : c-'a');
	}
	if(!isalpha((ccc=str[0][2]))) {
		(*str)+=2;
		return MIN_COL+26 + (isupper(c) ? c-'A' : c-'a')*26 + (isupper(cc) ? cc-'A' : cc-'a');
	}
#if MAX_COL>702
	if(!isalpha((cccc=str[0][3]))) {
		(*str)+=3;
		return MIN_COL+702 + (isupper(c) ? c-'A' : c-'a')*26*26 + (isupper(cc) ? cc-'A' : cc-'a')*26 + (isupper(ccc) ? ccc-'A' : ccc-'a');
	}
	if(!isalpha(str[0][4])) {
		(*str)+=4;
		return MIN_COL+18278 + (isupper(c) ? c-'A' : c-'a')*26*26*26 + (isupper(cc) ? cc-'A' : cc-'a')*26*26 + (isupper(ccc) ? ccc-'A' : ccc-'a')*26 + (isupper(cccc) ? cccc-'A' : cccc-'a');
	}
#endif
	return 0;
}
