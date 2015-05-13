/*****************************************************************************

  Copyright (c) 1992 Consorzio Pisa Ricerche.
  Authors: Caneve Maurizio, Salvatori Elena

  This software was produced under the ESPRIT/LOTOSPHERE
  project. All rights reserved.

*****************************************************************************/

/*									*/
/*      @(#)abstract.h	4.31  2/1/93                       		*/
/*									*/

#ifndef abstract_hh
#define abstract_hh

#include <tree.h>
#include <adatat.h>

class SignatureSorts;

class ID_Place: public Tree_Node
{
	char *str;
	char RBubble;	

friend class Gl_Identifier;

public:
 ID_Place();
 void  SetIdent(char *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetVisible(char);
 void SetFather(Tree_Node *);
 void ClearID(void);
 virtual ~ID_Place();

/* inline functions		*/
 char *GetIdent() {return(str);};
 void SetRBubble(char r) { RBubble = r; };
 char GetRBubble(void) { return(RBubble); };
}; /* end of class ID_Place	*/


class ID_List: public Tree_Node
{
	ID_Place *elem;
	ID_List *next;

friend class Gl_Id_List;
friend class Oper;

public:

 ID_List(ID_Place *, ID_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 ID_Place *GetElem();
 void HideMe(void);
 void SetCollapsed(char);
 void SetRBubble(char);
 void BuildSigSorts(char, SignatureSorts **, char, Oper *);
 void SetVisible(char);
 void SetAlignement(char);
 void ClearIDs(void);
 int GetCardinality(int);
}; /* end of class ID_List	*/


class Id_Decl: public Tree_Node
{
	ID_List *id_list;
	ID_Place *sort_id;

friend class Gl_Id_Decl;

public:
 Id_Decl(ID_List *, ID_Place *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Id_Decl */


class Id_Decl_List: public Tree_Node
{
	Id_Decl *elem;
	Id_Decl_List *next;

friend class Gl_Id_Decl_List;

public:
 Id_Decl_List(Id_Decl *, Id_Decl_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Id_Decl_List	*/


class Comment: public Tree_Node
{
	char *comm;

friend class Gl_Comment;

public:
 Comment();
 Comment(Tree_Node*, Coord , Coord);
 void SetComment(char *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 virtual ~Comment();
}; /* end of class Comment	*/


class Comment_List: public Tree_Node
{
	Comment *elem;
	Comment_List *next;

public:
 Comment_List(Comment *, Comment_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /* end of class Comment_List	*/


class Value_Expr: public Tree_Node
{

friend class Gl_Value_Expr;

public:
 Value_Expr();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);

}; /* end of class Value_Expr  */


class Value_Expr_List: public Tree_Node
{
	Tree_Node *elem;
	Value_Expr_List *next;

friend class Gl_Value_Expr_List;

public:
 Value_Expr_List(Tree_Node *, Value_Expr_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Value_Expr_List	*/


class Exit_Entry: public Tree_Node
{

friend class Gl_Exit_Entry;

public:
 Exit_Entry();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);

}; /* end of class Exit_Entry	*/


class Exit_Entry_List: public Tree_Node
{
	Tree_Node *elem;
	Exit_Entry_List *next;

friend class Gl_Exit_Entry_List;

public:
 Exit_Entry_List(Tree_Node *, Exit_Entry_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Exit_Entry_List	*/


class Exper_Off: public Tree_Node
{

public:
 Exper_Off();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);

}; /* end of class Exper_Off */


class Exper_Off_List: public Tree_Node
{
        Exper_Off *elem;
        Exper_Off_List *next;

friend class Gl_Exper_Off_List;

public:
 Exper_Off_List(Exper_Off *, Exper_Off_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Exper_Off_List      */


class Gate_Decl: public Tree_Node
{
	ID_List *gate_id_list1;
	ID_List *gate_id_list2;

friend class Gl_Gate_Decl;

public:
 Gate_Decl(ID_List *, ID_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Gate_Decl */


class Gate_Decl_List: public Tree_Node
{
	Gate_Decl *elem;
	Gate_Decl_List *next;

friend class Gl_Gate_Decl_List;

public:
 Gate_Decl_List(Gate_Decl *, Gate_Decl_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Gate_Decl_List */


class Ident_Eq: public Tree_Node
{
        Id_Decl *iddecl;
        Value_Expr *expr;

friend class Gl_Ident_Eq;

public:
 Ident_Eq(Id_Decl *,  Value_Expr *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Ident_Eq      */


class Ident_Eq_List: public Tree_Node
{
        Ident_Eq *elem;
        Ident_Eq_List *next;

friend class Gl_Ident_Eq_List;

public:
 Ident_Eq_List(Ident_Eq *, Ident_Eq_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Ident_Eq_List      */


class Half_Container: public Tree_Node
{
	ID_List *gate_list;

friend class Gl_Half_Container;

public:
 Half_Container(ID_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Half_Container	*/



/*								*/
/*	Process Definitions					*/
/*								*/


class Specification: public Tree_Node
{
class Definition;

        Comment_List *com_list;
	ID_Place *ident;
	ID_List *gate_list;
	Id_Decl_List *id_decl_list;
        Comment_List *com_list1;
	Tree_Node *func;
	Data_List *dt_list;
	Definition *def;
	Coord yl1,yl2,yl3;
	Coord MaxX, MaxY;

friend class Gl_Specification;

public:
 Specification(Comment_List *, ID_Place *, ID_List *, Id_Decl_List *,
	       Comment_List *, Definition *, Tree_Node *, Data_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetPath(int&, int&);
 Coord GetMaxX();
 Coord GetMaxY();
}; /* end of class Specification	*/


class Process: public Tree_Node
{
        Comment_List *com_list;
	ID_Place *ident;
	ID_List *gate_list;
	Id_Decl_List *id_decl_list;
	Definition *def;
	Tree_Node *func;
	char nesting;
	Coord yl1,yl2,yl3,ypath;
	Coord MaxX, MaxY;

friend class Gl_Process;

public:
 Process(Comment_List *, ID_Place *, ID_List *,Id_Decl_List  *, Definition *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetPath(char *, char, int&, int&);
 void NewBex(Tree_Node *);

 Coord GetMaxY();
 Coord GetMaxX();

/* inline functions		*/
 char GetNesting() {return(nesting);};
}; /* end of class Process	*/


class Proc_List: public Tree_Node
{
        Process *elem;
        Proc_List *next;

public:
 Proc_List(Process *, Proc_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetPath(char *, char, int&, int&);
 char *GetPath();
}; /* end of class Proc_List      */


class Definition: public Tree_Node
{
        Tree_Node *bex;
        Proc_List *process_list;
        Data_List *data_list;
	char path[PATH_LENGHT];

public:
 Definition(Tree_Node *, Proc_List *, Data_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetPath(char *, char, int&, int&);

/* inline functions		*/
 char *GetPath() { return(path);};
}; /* end of class Definition      */



/*								*/
/*	Functionalities						*/
/*								*/


class Exit: public Tree_Node
{
	ID_List *sort_list;

friend class Gl_Exit;

public:
 Exit(ID_List *);
 void SetPosition(Coord, Coord, Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /*end of class Exit */


class NoExit: public Tree_Node
{

friend class Gl_NoExit;

public:
 NoExit();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /*end of class NoExit */



/*								*/
/*	Value Expressions					*/
/*								*/


class Value: public Tree_Node
{
	ID_Place *ident;
	ID_Place *sort_id;

friend class Gl_Value;

public:
 Value(ID_Place *, ID_Place *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Value */


class Term: public Tree_Node
{
	ID_Place *op_ident;
	Value_Expr_List *expr_list;
	ID_Place *sort_id;

friend class Gl_Term;

public:
 Term(ID_Place *, Value_Expr_List *, ID_Place *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Term */


class Equality: public Tree_Node
{
        Tree_Node *express1;
        Tree_Node *express2;

friend class Gl_Equality;

public:
 Equality();
 Equality(Tree_Node *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Equality        */


class Sort_Id_Exit: public Tree_Node
{
	ID_Place *sort_id;

friend class Gl_Sort_Id_Exit;

public:
 Sort_Id_Exit(ID_Place *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Sort_id_Exit */



/*								*/
/*	Guard Options						*/
/*								*/


class NoGuard: public Tree_Node
{

friend class Gl_NoGuard;

public:
 NoGuard();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /* end of class NoGuard      */


class Guard: public Tree_Node
{
        Equality *equality;

friend class Gl_Guard;

public:
 Guard(Equality *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Guard        */



/*								*/
/*	Experiment Offers					*/
/*								*/


class NoExperiment: public Tree_Node
{

friend class Gl_NoExperiment;

public:
 NoExperiment();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /* end of class NoExperiment */


class Experiment: public Tree_Node
{
        Exper_Off_List *exp_offer_list;
        Tree_Node *guard_option;

friend class Gl_Experiment;

public:
 Experiment(Exper_Off_List *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);

/* inline functions		*/
 Tree_Node *GetGuard(void) { return(guard_option); };
}; /* end of class Experiment   */


class Exclam: public Tree_Node
{
        Tree_Node *value_exp;

friend class Gl_Exclam;

public:
 Exclam(Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Exclam      */


class Query: public Tree_Node
{
        Id_Decl *id_decl;

friend class Gl_Query;

public:
 Query(Id_Decl *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);
}; /* end of class Query      */



/*								*/
/*	Action Denotations					*/
/*								*/


class Internal: public Tree_Node
{

friend class Gl_Internal;

public:
 Internal();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /* end of class Internal     */


class Communication: public Tree_Node
{
	ID_Place *gate_identifier;
        Tree_Node *experiment_option;
        Coord xl, yl;
        int hr;

friend class Gl_Communication;

public:
 Communication(ID_Place *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /* end of class Communication        */



/*								*/
/*	Parallel Operators					*/
/*								*/


class Gen_Paral: public Tree_Node
{
        ID_List *gate_id_list;
	Coord Xclose;
	char border;

friend class Gl_Gen_Paral;

public:
 Gen_Paral(ID_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetCollapsed(char);

/* inline functions		*/
 void HideGate() { ((ID_List *)gate_id_list)->HideMe(); };
}; /* end of class Gen_Paral    */


class Interl: public Tree_Node
{

friend class Gl_Interl;

public:
 Interl();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /* end of class Interl	*/


class Syncr: public Tree_Node
{

friend class Gl_Syncr;

public:
 Syncr();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
}; /* end of class Syncr	*/



/*								*/
/*	Bejhviour Expressions					*/
/*								*/


class Action_Pref: public Tree_Node
{
        Tree_Node *action_den;
        Tree_Node *bex;
	char havebox;
	char border;

friend class Gl_Action_Pref;

public:
 Action_Pref(Tree_Node *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /* end of class Action_Pref  */


class Enable: public Tree_Node
{
        Tree_Node *bex1, *bex2;
	ID_List *gate_id_list;
        Coord xl, yl1, xl2, yl2, xid, yid, Yclose;
	char havebox;
	char border;

friend class Gl_Enable;

public:
 Enable(Tree_Node *, ID_List *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 int Get_Textual_H(void);
 int Get_Textual_W(void);
 void SetTerminalPos(void);
 void SetCollapsed(char);
}; /* end of class Enable       */


class Disable: public Tree_Node
{
        Tree_Node *bex1, *bex2;
        Coord xl, yl, yl2;
	char havebox;
	char border;

friend class Gl_Disable;

public:
 Disable(Tree_Node *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 int Get_Textual_H(void);
 int Get_Textual_W(void);
 void SetTerminalPos(void);
 void SetCollapsed(char);
}; /* end of class Disable      */


class Choice: public Tree_Node
{
	Tree_Node *bex1, *bex2;
	Coord xl, yl, yl1;
	char havebox;
	int delta;
	char border;

friend class Gl_Choice;

public:
 Choice(Tree_Node *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
 int Get_Textual_H(void);
 int Get_Textual_W(void);
 void SetTerminalPos(void);
 void ChangeH(int);
 void ChangeW(int);
}; /* end of class Choice	*/


class Stop: public Tree_Node
{
	char havebox;
	char border;

friend class Gl_Stop;

public:
 Stop();
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /* end of class Stop	*/


class Exit_Bex: public Tree_Node
{
	Exit_Entry_List *entry_list; 
	Coord Xopen, Xclose;
	char havebox;
	char border;

friend class Gl_Exit_Bex;

public:
 Exit_Bex(Exit_Entry_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /*end of class Exit_Bex */


class Hide: public Tree_Node
{
	ID_List *gate_list;
	Tree_Node *bex;
	char border;
	Coord yl;

friend class Gl_Hide;

public:
 Hide(ID_List *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /* end of class Hide	*/


class Guarded: public Tree_Node
{
        Equality *equality;
        Tree_Node *bex;
	Coord Xclose;
	char border;

friend class Gl_Guarded;

public:
 Guarded(Equality *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /* end of class Guarded        */


class Proc_Inst: public Tree_Node
{
	ID_List *gate_list;
	ID_Place *ident;
	Value_Expr_List *value_expr_list;
	Coord yp, yl1, Xopen, Xclose;
	int hp;
	char border;

friend class Gl_Proc_Inst;

public:
  Proc_Inst(ID_Place *, ID_List *, Value_Expr_List *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
};  /* end of class Proc_inst	*/


class Parallel: public Tree_Node
{
	Tree_Node *bex1, *bex2, *oper;
	Coord xl, yl, yl1;
	char havebox;
	int delta;
	char border;

friend class Gl_Parallel;

public:
 Parallel(Tree_Node *, Tree_Node *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 int Get_Textual_W(void);
 int Get_Textual_H(void);
 void SetTerminalPos(void);
 void SetCollapsed(char);

/* inline functions		*/
 char GetOperType(void) { return(oper->GetType());};
}; /* end of class Parallel	*/


class Local_Def: public Tree_Node
{
	Ident_Eq_List *equa_list;
	Tree_Node *bex;
	Coord yl;
	char border;

friend class Gl_Local_Def;

public:
 Local_Def(Ident_Eq_List *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /* end of class Local_Def	*/


class Par: public Tree_Node
{
	Gate_Decl_List *gate_decl_list;
	Tree_Node *oper, *bex;
	Coord xl, yl;
	char border;

friend class Gl_Par;

public:
 Par(Gate_Decl_List *, Tree_Node *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetCollapsed(char);
}; /* end of class Par	*/


class Sum_Gate: public Tree_Node
{
	Gate_Decl_List *gate_decl_list;
	Tree_Node *bex;
	Coord xl, yl, Xch, Ych, Xclose, Yclose;
	char border;

friend class Gl_Sum_Gate;

public:
 Sum_Gate(Gate_Decl_List *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetTerminalPos(void);
 void SetCollapsed(char);
}; /* end of class Sum_Gate	*/


class Sum_Ident: public Tree_Node
{
	Id_Decl_List *ident_decl_list;
	Tree_Node *bex;
	Coord xl, yl, Xch, Ych, Xclose, Yclose;
	char border;

friend class Gl_Sum_Ident;

public:
 Sum_Ident(Id_Decl_List *, Tree_Node *);
 void SetPosition(Coord, Coord);
 void SetDimensions(void);
 void SetFather(Tree_Node *);
 void SetTextual(char, char = FALSE);
 void SetTerminalPos(void);
 void SetCollapsed(char);
}; /* end of class Sum_Ident	*/



#endif
