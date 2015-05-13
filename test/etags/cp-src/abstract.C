/*****************************************************************************

  Copyright (c) 1992 Consorzio Pisa Ricerche.
  Authors: Caneve Maurizio, Salvatori Elena

  This software was produced under the ESPRIT/LOTOSPHERE
  project. All rights reserved.

*****************************************************************************/

/*								*/
/*	@(#)abstract.c	4.46  2/1/93				*/
/*								*/

#include <defs.h>
#include <adatat.h>
#include <abstract.h>
#include <editor.h>
#include <graphics.h>
#include <sheet.h>
#include <folder.h>

#include <stdio.h>
#include <string.h>

#ifdef DEBUG
 #include <stream.h>
#endif

/*								*/
/*	implementation of class Half_Container			*/
/*								*/

Half_Container::Half_Container(ID_List *g_l)
{
gate_list = g_l;
type = HALFCONTAINER;
selectable = FALSE;
visible = g_l->GetVisible();

Set_Unparser(new Gl_Half_Container(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Half_Container::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "HalfContainer" << x << " " << y << "\n";
 cout << "HalfContainer" << h << " " << w << "\n";
#endif
if(father->GetTextual())
   gate_list->SetPosition(x,y);
 else gate_list->SetPosition(x + ROUND_CORNER,y - ROUND_CORNER);
}

void Half_Container::SetDimensions(void)
{
gate_list->SetDimensions();

switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
     if(father->GetTextual())
	{
	h = Tree_Node::inUseFont()->Height();
	w = gate_list->Get_W(); 
	}
      else {
	    h = gate_list->Get_H() + ROUND_CORNER + VBORDER;
	    w = gate_list->Get_W() + 2 * ROUND_CORNER;
	    }
     }
aligline = round(h/2.0);
}

void Half_Container::SetFather(Tree_Node *f)
{
father = f;
gate_list->SetFather(this);
}

void Half_Container::SetCollapsed(char t)
{
collapsed = t;
gate_list->SetCollapsed(t);
}


/*								*/
/*	implementation of class Specification methods		*/
/*								*/

Specification::Specification(Comment_List *c_l, ID_Place *id, ID_List *g_i_l, 
			     Id_Decl_List *i_d_l, Comment_List *c_l1, Definition *d,
			      Tree_Node *f, Data_List *data_l)
{
Gl_Sheet *sh;
type = SPECIFICATION;
selectable = FALSE;
com_list = c_l;
com_list1 = c_l1;
ident = id;
gate_list = g_i_l;
id_decl_list = i_d_l;
def = d;
func = f;
dt_list = data_l; 
Set_Unparser(new Gl_Specification(this));
sh = (glow->Get_Folder())->Current();
sh->Add(Get_Unparser());
sh->SetRoot(this);
}

void Specification::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Specification" << x << " " << y << "\n";
 cout << "Specification" << h << " " << w << "\n";
#endif
com_list->SetPosition(x,y);

y1 = (com_list->Get_H() == 0)?y :y - com_list->Get_H() - SEPARATOR;
com_list1->SetPosition(x,y1);

x1 = x + round((w - gate_list->Get_W()) / 2.0);
y1 = (com_list1->Get_H() == 0)?y1 :y1 - com_list1->Get_H() - SEPARATOR;
gate_list->SetPosition(x1,y1);

y = y1 - gate_list->Get_H();

x1 = x + round((w - ident->Get_W()) / 2.0);
y1 = y - VBORDER;
ident->SetPosition(x1,y1);
yl1 = y1 - ident->Get_H() - VBORDER;

x1 = x + round((w - id_decl_list->Get_W()) / 2.0);
y1 = yl1 - VBORDER;
id_decl_list->SetPosition(x1,y1);

yl2 = y1 - id_decl_list->Get_H() - VBORDER;

y1 = (id_decl_list->Get_H() == 0)? yl2 + 2 * VBORDER - BORDER
                                  : yl2 - BORDER;
x1 = x + round((w - def->Get_W()) / 2.0);
def->SetPosition(x1,y1);

if(func->GetType() == EXIT && func->Get_W() != 0)
   yl3 = y1 - def->Get_H() - BORDER;
 else yl3 = 0;
x1 = x + round((w - func->Get_W()) / 2.0);
if(func->GetType() == EXIT)
  ((Exit *)func)->SetPosition(x1,y - h + func->Get_H(),x + w,y - h + func->Get_H());
 else func->SetPosition(x + w, y - h + func->Get_H());
}

void Specification::SetDimensions(void)
{
com_list->SetDimensions();
com_list1->SetDimensions();
ident->SetDimensions();
gate_list->SetDimensions();
id_decl_list->SetDimensions();
func->SetDimensions();
def->SetDimensions();
w = Max4(gate_list->Get_W(),ident->Get_W(),id_decl_list->Get_W(),def->Get_W());
w = Max(w, func->Get_W());
w += 2 * BORDER;

h = ident->Get_H()+id_decl_list->Get_H()+def->Get_H();
h = (func->GetType() == EXIT && func->Get_W() != 0)? h + func->Get_H(): h;
h = (id_decl_list->Get_H() == 0)? h + 2 * VBORDER + 2 * BORDER
                          : h + 4 * VBORDER + 2 * BORDER;
MaxX = com_list->Get_W();
MaxX = Max(MaxX, w + EXIT_S);
MaxY = h + gate_list->Get_H(); /* we have also to take in account the gate list      */
if(com_list->Get_H() != 0)
  MaxY = MaxY + com_list->Get_H() + SEPARATOR;          /* there are also some comments         */
}

void Specification::SetFather(Tree_Node *f)
{
father = f;
com_list->SetFather(this);
com_list1->SetFather(this);
ident->SetFather(this);
gate_list->SetFather(this);
id_decl_list->SetFather(this);
def->SetFather(this);
func->SetFather(this);
if(dt_list != NULL)
  dt_list->SetFather(this);
}

void Specification::SetPath(int& np, int& nd)
{
char buff[PATH_LENGHT];
np = np + 1;
sprintf(buff,"of %s",ident->GetIdent());
def->SetPath(buff, 1, np, nd);
if(dt_list != NULL)
  dt_list->SetPath(buff, 0, np, nd);
}

Coord Specification::GetMaxX()
{ return(MaxX);}

Coord Specification::GetMaxY()
{ return(MaxY);}

/*								*/
/*	implementation of class Process methods			*/
/*								*/

Process::Process(Comment_List *c_l, ID_Place *id, ID_List *g_i_l, Id_Decl_List *i_d_l,
			     Definition *d, Tree_Node *f)
{
Gl_Sheet *sh;
type = PROCESS;
selectable = FALSE;
nesting = 0;
com_list = c_l;
ident = id;
gate_list = g_i_l;
id_decl_list = i_d_l;
def = d;
func = f;
Set_Unparser(new Gl_Process(this));

sh = (glow->Get_Folder())->Current();
sh->Add(Get_Unparser());
sh->SetRoot(this);
}

void Process::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
ypath = yy;
y = yy - SEPARATOR - ident->Get_H(); 			/* it is equal to the height of the path */
#ifdef DEBUG
 cout << "Process" << x << " " << y << "\n";
 cout << "Process" << h << " " << w << "\n";
#endif

com_list->SetPosition(x,y);

x1 = x + round((w - gate_list->Get_W()) / 2.0);
y1 = (com_list->Get_H() == 0)?y :y - com_list->Get_H() - SEPARATOR;
gate_list->SetPosition(x1,y1);

y = y1 - gate_list->Get_H();

x1 = x + round((w - ident->Get_W()) / 2.0);
y1 = y - VBORDER;
ident->SetPosition(x1,y1);
yl1 = y1 - ident->Get_H() - VBORDER;

x1 = x + round((w - id_decl_list->Get_W()) / 2.0);
y1 = yl1 - VBORDER;
id_decl_list->SetPosition(x1,y1);

if(id_decl_list->Get_H() != 0)
  {
  yl2 = y1 - id_decl_list->Get_H() - VBORDER;
  y1 = yl2 - BORDER;
  }
 else {
      yl2 = 0;
      y1 = yl1 - BORDER;
      }
x1 = x + round((w - def->Get_W()) / 2.0);
def->SetPosition(x1,y1);

if(func->GetType() == EXIT && func->Get_W() != 0)
   yl3 = y1 - def->Get_H() - BORDER;
 else yl3 = 0;
x1 = x + round((w - func->Get_W()) / 2.0);
if(func->GetType() == EXIT)
  ((Exit *)func)->SetPosition(x1,y - h + func->Get_H(),x + w,y - h + func->Get_H());
 else func->SetPosition(x + w, y - h + func->Get_H());
}

void Process::SetDimensions(void)
{
com_list->SetDimensions();
ident->SetDimensions();
gate_list->SetDimensions();
id_decl_list->SetDimensions();
func->SetDimensions();
def->SetDimensions();
w = Max4(gate_list->Get_W(),ident->Get_W(),id_decl_list->Get_W(),def->Get_W());
w = Max(w, func->Get_W());
w += 2 * BORDER;

h = ident->Get_H()+id_decl_list->Get_H()+def->Get_H();
h = (func->GetType() == EXIT && func->Get_W() != 0)? h + func->Get_H(): h;
h = (id_decl_list->Get_H() == 0)? h + 2 * VBORDER + 2 * BORDER
                          : h + 4 * VBORDER + 2 * BORDER;
MaxX = Max(com_list->Get_W(),Tree_Node::inUseFont()->Width(((Proc_List *)GetFather())->GetPath()));
MaxX = Max(MaxX,w + EXIT_S);
MaxY = h + SEPARATOR + ident->Get_H() + gate_list->Get_H(); /* we have also to take in account 	*/
							     /* the path and the gate list	*/
if(com_list->Get_H() != 0)
  MaxY = MaxY + com_list->Get_H() + SEPARATOR;		/* there are also some comments		*/
}

void Process::SetFather(Tree_Node *f)
{
father = f;
com_list->SetFather(this);
ident->SetFather(this);
gate_list->SetFather(this);
id_decl_list->SetFather(this);
func->SetFather(this);
def->SetFather(this);
}

void Process::SetPath(char *p, char n, int& np, int& nd)
{
char buff[PATH_LENGHT];
nesting = n;
np = np + 1;
sprintf(buff,"of %s %s",ident->GetIdent(), p);
def->SetPath(buff, nesting + 1, np, nd);
}

Coord Process::GetMaxX()
{return(MaxX);}

Coord Process::GetMaxY()
{return(MaxY);}

/*								*/
/*	implementation of class Choice methods			*/
/*								*/


Choice::Choice(Tree_Node *b1, Tree_Node *b2)
{
type = CHOICE;
alignement = glow->GetAligs(CHOICE);
bex1 = b1;
bex2 = b2;

Set_Unparser(new Gl_Choice(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Choice::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Choice" << x << " " << y << "\n";
 cout << "Choice" << h << " " << w << "\n";
#endif
if(textual)
   {			
   SetTerminalPos();
   x1 = xl + Tree_Node::inUseFont()->Width("[] ");
   bex1->SetPosition(x1,y - border);
   bex2->SetPosition(x1, yl);
   }
 else {		/* graphical		*/
      if(alignement == HOR)
         {
         if(!havebox)
            x1 = x;
          else x1 = x + BORDER;
         y1 = round(y - (h - bex1->Get_H())/2.0);
         bex1->SetPosition(x1,y1);
         xl = x1 + bex1->Get_W() + BORDER;
         x1 = xl + BORDER;
         y1 = round(y - (h - bex2->Get_H())/2.0);
         bex2->SetPosition(x1,y1);
         }
        else {
             if(!havebox)
               y1 = y;
              else y1 = y - BORDER;
             x1 = round(x + (w - bex1->Get_W())/2.0);
             bex1->SetPosition(x1,y1);
             xl = y1 - bex1->Get_H() - BORDER;
             y1 = xl - BORDER;
             x1 = round(x + (w - bex2->Get_W())/2.0);
             bex2->SetPosition(x1,y1);
             }
       }
delta = (!havebox && (father->GetType() != CHOICE))?
           (father->GetType() == DEFINITION)?
               round((father->GetFather()->Get_W() - w)/2.0)
            : round((father->Get_W() - w)/2.0)
         : BORDER;
}

void Choice::SetDimensions(void)
{
bex1->SetDimensions();
bex2->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
 			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL ))
 	       		     {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
		           }
   			 else {
                              w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
      			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 
 	 if(textual)
	    {
	    h = Get_Textual_H(); w = Get_Textual_W();
	    if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	    }
          else {	/* graphical		*/
	       if(alignement == HOR)
                  {
	          w = bex1->Get_W() + bex2->Get_W() + 2 * BORDER;
	          h = Max(bex1->Get_H(), bex2->Get_H());
	          }
	        else {	
	             h = bex1->Get_H() + bex2->Get_H() + 2 * BORDER;
	             w = Max(bex1->Get_W(), bex2->Get_W());
	             }
 	       if(havebox)
		 {w += 2 * BORDER; h += 2 * BORDER;}
                else {
                     if((bex1->GetType() == CHOICE) && (bex1->Collapsed() != COLLAPSED_VIS))
                       {
		       if(alignement == HOR)
                         ((Choice *)bex1)->ChangeH(h);
                        else ((Choice *)bex1)->ChangeW(w);
		       }
                     if((bex2->GetType() == CHOICE) && (bex2->Collapsed() != COLLAPSED_VIS))
                       {
  		       if(alignement == HOR)
                         ((Choice *)bex2)->ChangeH(h);
                        else ((Choice *)bex2)->ChangeW(w);
		       }
                     }
               }
	 break;
  }
aligline = round(h/2.0);
}

void Choice::ChangeH(int nh)
{
int bh;		/* it will be the forced height for the second choice son	*/
if(!(GetTextual()))
  {
  h = nh;
  bh = h - ((Choice *)bex1)->Get_H();
  if(alignement == HOR)
    {
    if((bex1->GetType() == CHOICE) && (bex1->Collapsed() != COLLAPSED_VIS))
       ((Choice *)bex1)->ChangeH(h);
    if((bex2->GetType() == CHOICE) && (bex2->Collapsed() != COLLAPSED_VIS))
       ((Choice *)bex2)->ChangeH(h);
    }
   else {
        if((bex1->GetType() == CHOICE) && (bex1->Collapsed() != COLLAPSED_VIS))
           ((Choice *)bex1)->ChangeW(w);
         else  bh = bh - 2 * BORDER;
        if((bex2->GetType() == CHOICE) && (bex2->Collapsed() != COLLAPSED_VIS))
           {
           ((Choice *)bex2)->ChangeW(w);
           ((Choice *)bex2)->ChangeH(bh); /* we have to remember the forced	*/
    					  /* height, even if the alignwement is	*/
    				  	  /* now vertical				*/
           }
        }
  }
}

void Choice::ChangeW(int nw)
{
int bw;
if(!(GetTextual()))
  {
  w = nw;
  bw =  w - ((Choice *)bex1)->Get_W();
  if(alignement != HOR)
     {
     if((bex1->GetType() == CHOICE) && (bex1->Collapsed() != COLLAPSED_VIS))
        ((Choice *)bex1)->ChangeW(w);
     if((bex2->GetType() == CHOICE) && (bex2->Collapsed() != COLLAPSED_VIS))
        ((Choice *)bex2)->ChangeW(w);
     }
   else {
        if((bex1->GetType() == CHOICE) && (bex1->Collapsed() != COLLAPSED_VIS))
           ((Choice *)bex1)->ChangeH(h);
         else bw = bw - 2 * BORDER;
        if((bex2->GetType() == CHOICE) && (bex2->Collapsed() != COLLAPSED_VIS))
           {
           ((Choice *)bex2)->ChangeH(h);
           ((Choice *)bex2)->ChangeW(bw);
           }
        }
  }
}

void Choice::SetFather(Tree_Node *f)
{
char ft;
ft = (f->GetType() == PARALLEL)? ((Parallel *)f)->GetOperType(): f->GetType();
havebox = HaveBox(CHOICE,ft);
father = f;
bex1->SetFather(this);
bex2->SetFather(this);
}

void Choice::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex1->SetTextual(t);
bex2->SetTextual(t);
}

void Choice::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
bex1->SetCollapsed(t);
bex2->SetCollapsed(t);
}

int Choice::Get_Textual_H()
{
if((father->GetType() == CHOICE) && father->GetTextual())
   return(bex1->Get_H() + bex2->Get_H() + VBORDER);
 else return(bex1->Get_H() + bex2->Get_H() + 2 * VBORDER
		 + Tree_Node::inUseFont()->Height());
}

int Choice::Get_Textual_W()
{
int b;
b = Max(bex1->Get_W(), bex2->Get_W());
if((father->GetType() == CHOICE) && father->GetTextual())
   return(b);
 else return(Tree_Node::inUseFont()->Width("[] ") + b);
}

void Choice::SetTerminalPos()
{

xl=yl=yl1=0;
if((father->GetType() == CHOICE) &&
    father->GetTextual())	/* xl, yl is the position of [] */
	{
	xl = x - Tree_Node::inUseFont()->Width("[] ") + border;
	yl = y - bex1->Get_H() - VBORDER - border;
	}
   else {				/* x,y is the position of ( */
	xl = x + border;
	yl = y - bex1->Get_H() - VBORDER - border;
	yl1 = yl - bex2->Get_H() - VBORDER;
	}
}


/*								*/
/*	implementation of class stop methods			*/
/*								*/

Stop::Stop()
{
type = STOP;
Set_Unparser(new Gl_Stop(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Stop::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
cout << "Stop" << x << " " << y << "\n";
cout << "Stop" << h << " " << w << "\n";
#endif
}

void Stop::SetDimensions(void)
{
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
 			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
      if(textual)
	{
	w = Tree_Node::inUseFont()->Width("stop");
	h = Tree_Node::inUseFont()->Height();
	if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	  {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	}
       else {
	    w = 2 * STOP_R;
	    h = 2 * STOP_R;
	    if(havebox)
	      {w += 2 * BORDER; h += 2 * BORDER;}
	    }
	break;
 }
aligline = round(h/2.0);
}

void Stop::SetFather(Tree_Node *f)
{
char ft;
ft = (f->GetType() == PARALLEL)? ((Parallel *)f)->GetOperType(): f->GetType();
havebox = HaveBox(STOP,ft);
father = f;
}

void Stop::SetTextual(char t, char s)
{ textual = t; selectable = (!t | s); }

void Stop::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
}


/*								*/
/*	implementation of class Exit methods			*/
/*								*/

Exit::Exit(ID_List *sl)
{
type = EXIT;
selectable = FALSE;
sort_list = sl;
Set_Unparser(new Gl_Exit(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Exit::SetPosition(Coord x1, Coord y1, Coord xx, Coord yy)
{
x = xx;
y = yy;
sort_list->SetPosition(x1,y1 - VBORDER);
#ifdef DEBUG
 cout << "Exit" << x << " " << y << "\n";
 cout << "Exit" << h << " " << w << "\n";
#endif
}

void Exit::SetDimensions(void)
{
sort_list->SetDimensions();
w = sort_list->Get_W();
h = (sort_list->Get_H() == 0)? EXIT_S: sort_list->Get_H() + 2 * VBORDER;
aligline = round(h/2.0);
}

void Exit::SetFather(Tree_Node *f)
{father = f;
}

/*								*/
/*	implementation of class Exit_Bex methods		*/
/*								*/

Exit_Bex::Exit_Bex(Exit_Entry_List *l)
{
type = EXIT;
alignement = glow->GetAligs(EXIT);
entry_list = l;

Set_Unparser(new Gl_Exit_Bex(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Exit_Bex::SetPosition(Coord xx, Coord yy)
{
Coord x1, y1;
x = xx;
y = yy;
if(textual)
   {
   x1 = x + border;
   y1 = y - border;
   if(entry_list->GetVisible())
      {
      Xopen = x1 + Tree_Node::inUseFont()->Width("exit");
      entry_list->SetPosition(Xopen + Tree_Node::inUseFont()->Width(" (") ,y1);
      Xclose = Xopen + Tree_Node::inUseFont()->Width(" (") + entry_list->Get_W();
      }
    else entry_list->SetPosition(x1 + Tree_Node::inUseFont()->Width("exit") ,y);
   }
 else if(havebox)
   	 entry_list->SetPosition(x + 2 * BORDER,y - VBORDER - BORDER);
       else entry_list->SetPosition(x + BORDER,y - VBORDER);

#ifdef DEBUG
 cout << "Exit_Bex" << x << " " << y << "\n";
 cout << "Exit_Bex" << h << " " << w << "\n";
#endif
}

void Exit_Bex::SetDimensions(void)
{
entry_list->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
 			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
			aligline = round(h/2.0);
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0; aligline = 0;
			break;
 case NOCOLLAPSED:
   	  if(textual)
	     {
	     w = (entry_list->Get_W() == 0)? Tree_Node::inUseFont()->Width("exit")
		     : entry_list->Get_W() + Tree_Node::inUseFont()->Width("exit")
	  	      + Tree_Node::inUseFont()->Width(" (") + Tree_Node::inUseFont()->Width(")");
	     h = Tree_Node::inUseFont()->Height();
	     if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
		{border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	     aligline = round(h/2.0);
	     }
	   else {
		if(alignement == HOR)
                   {
	           w = (entry_list->Get_W() == 0)? EXIT_S
		       : entry_list->Get_W() + 2 * BORDER + EXIT_S;
	           h = (entry_list->Get_H() == 0)? EXIT_S
		       : Max(entry_list->Get_H() + 2 * VBORDER,EXIT_S);
		   aligline = round(h/2.0);
		   if(havebox)
		     {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
                   }
          	 else {
	              w = (entry_list->Get_W() == 0)? EXIT_S
		          : Max(entry_list->Get_W() + 2 * BORDER,EXIT_S);
	              h = (entry_list->Get_H() == 0)? EXIT_S
		          : entry_list->Get_H() + 2 * VBORDER + EXIT_S;
		      aligline = (entry_list->Get_H() == 0)? round(EXIT_S/2.0)
				 : round((h - EXIT_S)/2.0);
		      if(havebox)
		         {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
                      }
	        }
 }
}

void Exit_Bex::SetFather(Tree_Node *f)
{
char ft;
ft = (f->GetType() == PARALLEL)? ((Parallel *)f)->GetOperType(): f->GetType();
havebox = HaveBox(EXIT,ft);
father = f; 
entry_list->SetFather(this);
}

void Exit_Bex::SetTextual(char t, char s)
{
textual = t; 
selectable = (!t | s);
entry_list->SetTextual(t);
}

void Exit_Bex::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
entry_list->SetCollapsed(t);
}

/*								*/
/*	implementation of class NoExit methods			*/
/*								*/

NoExit::NoExit()
{
type = NOEXIT;
selectable = FALSE;

Set_Unparser(new Gl_NoExit(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void NoExit::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "NoExit" << x << " " << y << "\n";
 cout << "NoExit" << h << " " << w << "\n";
#endif
}

void NoExit::SetDimensions(void)
{
w = NO_EXIT_S;
h = NO_EXIT_S;
aligline = round(h/2.0);
}

void NoExit::SetFather(Tree_Node *f)
{father = f;
}


/*								*/
/*	implementation of class ID_Place methods		*/
/*								*/

ID_Place::ID_Place()
{
type = IDPLACE;
visible = FALSE;
textual = TRUE;
RBubble = NORMAL;
selectable = FALSE;
str = new char[2];
str[0] = ' '; str[1] = '\0';

Set_Unparser(new Gl_Identifier(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void ID_Place::SetIdent(char *identifier)
{
int i;
visible = TRUE;

delete str;
str = new char[strlen(identifier) + 1];
for(i=0; i<= strlen(identifier); i++)
  *(str+i) = *(identifier+i);
}

void ID_Place::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "IDPlace" << x << " " << y << "\n";
 cout << "IDPlace" << h << " " << w << "\n";
 cout << "IDPlace" << visible << " " << collapsed << "\n";
#endif
}

void ID_Place::SetDimensions(void)
{
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 
		switch(RBubble)
		 {
		 case NORMAL:	w = Tree_Node::inUseFont()->Width(str);
				h = Tree_Node::inUseFont()->Height();
				break;
		 case ROUND:	w = Max(Tree_Node::inUseFont()->Width(str) + 
				         2 * ROUND_CORNER, SMALL_DIL);
				h = Tree_Node::inUseFont()->Height() + 
				 	  SYNCR_L + SMALL_PEN;
				break;
		 case ELLIPSE:
		 case F_ELLIPSE: w = Tree_Node::inUseFont()->Width(str) + 2 * ROUND_CORNER;
				 h = Tree_Node::inUseFont()->Height() + 2 * ROUND_CORNER;
				 break;
		 case D_ELLIPSE: w = Tree_Node::inUseFont()->Width(str) + 2 * ROUND_CORNER;
				 h = Tree_Node::inUseFont()->Height() + 2 * ROUND_CORNER 
					 + 2 *MAX_VIEWS;
				 break;	
 		}
 }
aligline = round(h/2.0);
}

void ID_Place::SetFather(Tree_Node *f)
{ father = f;
}

ID_Place::~ID_Place()
{ delete str; }

void ID_Place::SetVisible(char v)
{ 
if (strcmp(str, " ") != 0)
	visible = v;
}

void ID_Place::ClearID(void)
{
visible = FALSE;
delete str;
str = new char[2];
str[0] = ' '; str[1] = '\0';
}

/*								*/
/*	implementation of class ID_List methods			*/
/*								*/

ID_List::ID_List(ID_Place *el, ID_List *nxt)
{
type = IDLIST;
elem = el;
next = nxt;
alignement = HOR;
textual = TRUE;
selectable = FALSE;
visible = elem->GetVisible();

Set_Unparser(new Gl_Id_List(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void ID_List::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "IDList" << x << " " << y << "\n";
 cout << "IDList" << h << " " << w << "\n";
 cout << "IDList" << visible << " " << collapsed << "\n";
#endif
if (elem != NULL)
 {
 y1 = y;
 x1 = (alignement == HOR) ? x : x - round(elem->Get_W()/2.0);
		 /* x is the central axis for the Vertical list*/
 elem->SetPosition(x1,y1);
 if (next != NULL)
 {
  if (elem->GetRBubble() == NORMAL)
      {
      x1 = (alignement == VER) ? x:  
		x1 + elem->Get_W() + Tree_Node::inUseFont()->Width(", ");
      y1 = (alignement == VER) ? y1 - elem->Get_H() - VBORDER : y;
      }
   else
     x1 = x + elem->Get_W() + ROUND_CORNER;
 next->SetPosition(x1,y1);
 }

}
}

void ID_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	if (elem->GetRBubble() == NORMAL)
				{
				if (alignement == HOR)
				 {h = Tree_Node::inUseFont()->Height();
				 w = (next == NULL || next->next == NULL)
			            ? elem->Get_W() : elem->Get_W() + 
			            next->Get_W() + Tree_Node::inUseFont()->Width(", ");
				 }
				else /*op_id_list only*/
				 {
				 h = (next == NULL || next->next == NULL)
			            ? elem->Get_H() : elem->Get_H() + 
			            next->Get_H() + VBORDER;
				 w = (next == NULL || next->next == NULL)
			            ? elem->Get_W() : Max(elem->Get_W(), 
			            next->Get_W());
				 }
				
				}
			   else {
			  	 h =  elem->Get_H();	
				 w = (next == NULL || next->next == NULL)
                                    ? elem->Get_W() : elem->Get_W() + 
                                    next->Get_W() + ROUND_CORNER;
				}	
 }
aligline = round(h/2.0);
}

void ID_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

void ID_List::SetCollapsed(char t)
{
collapsed = t;
if(elem != NULL)
  elem->SetCollapsed(t);
if(next != NULL)
  next->SetCollapsed(t);
}

void ID_List::HideMe(void)
{
visible = FALSE;
if(elem != NULL)
  elem->SetVisible(FALSE);
if(next != NULL)
  next->HideMe();
}

void ID_List::SetRBubble(char r)
{
if(elem != NULL)
  elem->SetRBubble(r);
if(next != NULL)
  next->SetRBubble(r);
}

void ID_List::SetAlignement(char a)
{
alignement = a;
if(elem != NULL)
  elem->SetAlignement(a);
if(next != NULL)
  next->SetAlignement(a);
}

int ID_List::GetCardinality(int c)
{
int card;
card = c;
if(elem->GetVisible())
        card ++;
if(next != NULL)
  card = next->GetCardinality(card);
return(card);
}

void ID_List::SetVisible(char v)
{
if(elem != NULL)
  elem->SetVisible(v);  
visible = elem->GetVisible();       /*ID_List is set to visible only if elem is 
					not a placeholder */
if(next != NULL)
  next->SetVisible(v);
}

void ID_List::BuildSigSorts(char bubble, SignatureSorts **head, char type, Oper *op)
{
if((elem != NULL) && elem->GetVisible())
 {
 if(*head != NULL)
    (*head)->Append(bubble, elem, type, op);
   else {
	*head = new SignatureSorts(bubble, elem, NULL);
	switch(type)
         {
	 case '0': break;
         case '1': (*head)->cost = op;
		   break;
         case '2': (*head)->op_in_l = new OperSig(op, NULL);
		   break;
         case '3': (*head)->op_out_l = new OperSig(op, NULL);
  	 }	
	}
 }
if(next != NULL)
 next->BuildSigSorts(bubble, head, type, op);
}

void ID_List::ClearIDs(void)
{
visible = FALSE;
if(elem != NULL)
  elem->ClearID();
if(next != NULL)
  next->ClearIDs();
}

/*								*/
/*	implementation of class Id_Decl methods			*/
/*								*/

Id_Decl::Id_Decl(ID_List *l, ID_Place *s)
{
type = IDDECL;
textual = TRUE;
selectable = FALSE;
id_list = l;
sort_id = s;
if(l == NULL)
  id_list = new ID_List(new ID_Place(),NULL);
if(s == NULL)
  sort_id = new ID_Place();
visible = (id_list->GetVisible() && sort_id->GetVisible());

Set_Unparser(new Gl_Id_Decl(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Id_Decl::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "IDDecl" << x << " " << y << "\n";
 cout << "IDDecl" << h << " " << w << "\n";
#endif
if(id_list != NULL)
 {
 id_list->SetPosition(x,y);
 x1 = x + id_list->Get_W() + Tree_Node::inUseFont()->Width(": ");
 if(sort_id != NULL)
   sort_id->SetPosition(x1,y);
 }
}

void Id_Decl::SetDimensions(void)
{
id_list->SetDimensions();
sort_id->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = (visible)? id_list->Get_W()+sort_id->Get_W()
					               +Tree_Node::inUseFont()->Width(": ")
		                     : 0;
 }
aligline = round(h/2.0);
}

void Id_Decl::SetFather(Tree_Node *f)
{
father = f;
if(id_list != NULL)
  id_list->SetFather(this);
if(sort_id != NULL)
  sort_id->SetFather(this);
}

void Id_Decl::SetCollapsed(char t)
{
collapsed = t;
if(id_list != NULL)
  id_list->SetCollapsed(t);
if(sort_id != NULL)
  sort_id->SetCollapsed(t);
}


/*								*/
/*	implementation of class Id_Decl_List methods		*/
/*								*/

Id_Decl_List::Id_Decl_List(Id_Decl *el, Id_Decl_List *nxt)
{
type = IDDECLLIST;
textual = TRUE;
selectable = FALSE;
elem = el;
next = nxt;
visible = elem->GetVisible();

Set_Unparser(new Gl_Id_Decl_List(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Id_Decl_List::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "IDDeclList" << x << " " << y << "\n";
 cout << "IDDeclList" << h << " " << w << "\n";
#endif
if(elem != NULL)
 {
 elem->SetPosition(x,y);
 x1 = x + elem->Get_W() + Tree_Node::inUseFont()->Width(", ");
 if(next != NULL)
   next->SetPosition(x1,y);
 }
}

void Id_Decl_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = (next == NULL || next->next == NULL)? elem->Get_W()
			      : elem->Get_W() + next->Get_W() + Tree_Node::inUseFont()->Width(", ");
 }
aligline = round(h/2.0);
}

void Id_Decl_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

void Id_Decl_List::SetCollapsed(char t)
{
collapsed = t;
if(elem != NULL)
  elem->SetCollapsed(t);
if(next != NULL)
  next->SetCollapsed(t);
}



/*								*/
/*	implementation of class Comment methods			*/
/*								*/

Comment::Comment()
{
type = COMMENT;
textual = TRUE;
visible = FALSE;
selectable = FALSE;
comm = new char[2];
comm[0] = ' '; comm[1] = '\0';

Set_Unparser(new Gl_Comment(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Comment::SetComment(char *comment)
{
visible = TRUE;
delete comm;
comm = new char[strlen(comment)+7];
sprintf(comm, "(* %s *)",comment);
if(glow->ShowComments())
  {
  w = Tree_Node::inUseFont()->Width(comm);
  h = Tree_Node::inUseFont()->Height();
  }
 else {
      w = 0;
      h = 0;
      };
((glow->Get_Folder())->Current())->YesComments();
}

void Comment::SetFather(Tree_Node *f)
{father = f;
}

void Comment::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Comment" << x << " " << y << "\n";
 cout << "Comment" << h << " " << w << "\n";
#endif
}

void Comment::SetDimensions(void)
{
if(visible && glow->ShowComments())
  {
  w = Tree_Node::inUseFont()->Width(comm);
  h = Tree_Node::inUseFont()->Height();
  }
 else {
      w = 0;
      h = 0;
      };
aligline = round(h/2.0);
}

Comment::~Comment()
{ delete comm; }

/*								*/
/*	implementation of class Comment_List methods		*/
/*								*/

Comment_List::Comment_List(Comment *el, Comment_List *nxt)
{
type = COMMENTLIST;
textual = TRUE;
visible = TRUE;
selectable = FALSE;
elem = el;
next = nxt;
}

void Comment_List::SetPosition(Coord xx, Coord yy)
{
Coord y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "CommentList" << x << " " << y << "\n";
 cout << "CommentList" << h << " " << w << "\n";
#endif
if(elem != NULL)
 {
 elem->SetPosition(x,y);
 y1 = y - elem->Get_H();
 if(next != NULL)
   next->SetPosition(x,y1);
 }
}

void Comment_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
h = (next != NULL)? elem->Get_H() + next->Get_H()
		  : elem->Get_H();
w = (next != NULL)? Max(elem->Get_W(), next->Get_W())
		  : elem->Get_W();
aligline = round(h/2.0);
}

void Comment_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
   next->SetFather(this);
}


/*								*/
/*	implementation of class Parallel methods		*/
/*								*/

Parallel::Parallel(Tree_Node *b1, Tree_Node *op, Tree_Node *b2)
{
type = PARALLEL;
bex1 = b1;
bex2 = b2;
oper = op;

Set_Unparser(new Gl_Parallel(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Parallel::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Parallel" << x << " " << y << "\n";
 cout << "Parallel" << h << " " << w << "\n";
 cout << "Parallel" << visible << " " << collapsed << "\n";
#endif

if(textual)
   {			
   SetTerminalPos();
   x1 = (oper->GetType() == GEN_PARAL)? xl + Tree_Node::inUseFont()->Width("    ")
                                       : xl + oper->Get_W();
   bex1->SetPosition(x1, y - border);
   oper->SetPosition(xl, yl);
   if(oper->GetType() == GEN_PARAL)
       bex2->SetPosition(x1, yl - oper->Get_H() - VBORDER);	
    else bex2->SetPosition(x1, yl);	
   }
 else {
      if(alignement == HOR)
         {
         if(!havebox)
            x1 = x;
          else x1 = x + BORDER;
         y1 = y - aligline + bex1->GetAligLine();
         bex1->SetPosition(x1,y1);
         x1 = x1 + bex1->Get_W();
         y1 = y - aligline + oper->GetAligLine();
         oper->SetPosition(x1,y1);
      	 x1 = x1 + oper->Get_W();
         y1 = y - aligline + bex2->GetAligLine();
         bex2->SetPosition(x1,y1);
         }
        else {
             if(!havebox)
                y1 = y;
              else y1 = y - BORDER;
             x1 = round(x + (w - bex1->Get_W())/2.0);
             bex1->SetPosition(x1,y1);
             y1 = y1 - bex1->Get_H();
             x1 = round(x + (w - oper->Get_W())/2.0);
             oper->SetPosition(x1,y1);
      	     y1 = y1 - oper->Get_H();
             x1 = round(x + (w - bex2->Get_W())/2.0);
             bex2->SetPosition(x1,y1);
             }
       }
delta = (father->GetType() == DEFINITION)?
            round((father->GetFather()->Get_W() - w)/2.0)
         : BORDER;
}

void Parallel::SetDimensions(void)
{
bex1->SetDimensions();
bex2->SetDimensions();
oper->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
			aligline = round(h/2.0);
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0; aligline = 0;
			break;
 case NOCOLLAPSED: 
     if(textual)
	{
	h = Get_Textual_H(); w = Get_Textual_W();
	if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	   {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	aligline = round(h/2.0);
 	}
      else {
	   if(alignement == HOR)
              {
	      w = bex1->Get_W() + oper->Get_W() + bex2->Get_W();

	      aligline = Max(bex1->GetAligLine(),oper->GetAligLine());
	      aligline = Max(aligline,bex2->GetAligLine());
	      if(bex1->Get_H() - bex1->GetAligLine() >
                 oper->Get_H() - oper->GetAligLine())
                h = bex1->Get_H() - bex1->GetAligLine();
	       else h = oper->Get_H() - oper->GetAligLine();
	      if(bex2->Get_H() - bex2->GetAligLine() > h)
		h = bex2->Get_H() - bex2->GetAligLine();
	      h += aligline;
	      if(havebox)
               {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
 	      }
	    else {
	         h = bex1->Get_H() + oper->Get_H() + bex2->Get_H();
	         w = Max(bex1->Get_W(), bex2->Get_W());
                 w = Max(w, oper->Get_W());
	         if(havebox)
                  {h += 2 * BORDER; w += 2 * BORDER;}
		 aligline = round(h/2.0);
	         }
           }
 }
}

void Parallel::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex1->SetTextual(t);
oper->SetTextual(t);
bex2->SetTextual(t);
}

int Parallel::Get_Textual_W()
{
int b;
b = Max(bex1->Get_W(), bex2->Get_W());
if(oper->GetType() == GEN_PARAL)
   {
   b += Tree_Node::inUseFont()->Width("    ");
   b = Max(oper->Get_W(), b); return(b);
   }
 else 
      if((father->GetType() == PARALLEL) && (((Parallel *)father)->GetOperType() == oper->GetType()) 
         && father->GetTextual())
	  return(b);
       else return(oper->Get_W() + b); 
}

int Parallel::Get_Textual_H()
{
if(oper->GetType() == GEN_PARAL)
    return(bex1->Get_H() + bex2->Get_H() + oper->Get_H() +  3 *VBORDER
           + Tree_Node::inUseFont()->Height());
 else if((father->GetType() == PARALLEL) && (((Parallel *)father)->GetOperType() == oper->GetType())
	  &&  father->GetTextual())
	   return(bex1->Get_H() + bex2->Get_H() + VBORDER);
       else return(bex1->Get_H() + bex2->Get_H() + 2 *VBORDER + Tree_Node::inUseFont()->Height());
}

void Parallel::SetTerminalPos()
{
xl=yl=yl1=0;

yl =  y - bex1->Get_H() - VBORDER - border;
if(oper->GetType() == GEN_PARAL)
  {
  xl = x + border;
  yl1 =  yl - oper->Get_H() - bex2->Get_H() - 2 * VBORDER;
  }
 else 
      if((father->GetType() == PARALLEL) && (((Parallel *)father)->GetOperType() == oper->GetType())
          && father->GetTextual())
           xl = x - oper->Get_W() + border;
       else {
            xl = x + border;
            yl1 =  yl - bex2->Get_H() - VBORDER;
            }
}

void Parallel::SetFather(Tree_Node *f)
{
char ft;
ft = (f->GetType() == PARALLEL)? ((Parallel *)f)->GetOperType(): f->GetType();
havebox = HaveBox(oper->GetType(),ft);
father = f;
bex1->SetFather(this);
oper->SetFather(this);
bex2->SetFather(this);
}

void Parallel::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
bex1->SetCollapsed(t);
oper->SetCollapsed(t);
bex2->SetCollapsed(t);
}

/*								*/
/*	implementation of class Ident_Eq methods		*/
/*								*/

Ident_Eq::Ident_Eq(Id_Decl *idd, Value_Expr *ex)
{
type = IDEQ;
textual = TRUE;
selectable = FALSE;
iddecl = idd;
expr = ex;
if(idd == NULL)
  iddecl = new Id_Decl(NULL,NULL);
if(ex == NULL)
  expr = new Value_Expr;
visible = (iddecl->GetVisible() && expr->GetVisible());

Set_Unparser(new Gl_Ident_Eq(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Ident_Eq::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "IdentEq" << x << " " << y << "\n";
 cout << "IdentEq" << h << " " << w << "\n";
#endif

iddecl->SetPosition(x,y);
x1 = x + iddecl->Get_W() + Tree_Node::inUseFont()->Width(" = ");
expr->SetPosition(x1,y);
}

void Ident_Eq::SetDimensions(void)
{
iddecl->SetDimensions();
expr->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	w = iddecl->Get_W() + expr->Get_W() + Tree_Node::inUseFont()->Width(" = ");
			h = Tree_Node::inUseFont()->Height();
 }
aligline = round(h/2.0);
}

void Ident_Eq::SetFather(Tree_Node *f)
{
father = f;
iddecl->SetFather(this);
expr->SetFather(this);
}

void Ident_Eq::SetCollapsed(char t)
{
collapsed = t;
iddecl->SetCollapsed(t);
expr->SetCollapsed(t);
}


/*								*/
/*	implementation of class Ident_Eq_List methods		*/
/*								*/

Ident_Eq_List::Ident_Eq_List(Ident_Eq *el, Ident_Eq_List *nxt)
{
type = IDEQLIST;
textual = TRUE;
selectable = FALSE;
elem = el;
next = nxt;
visible = elem->GetVisible();

Set_Unparser(new Gl_Ident_Eq_List(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Ident_Eq_List::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "IdentEqList" << x << " " << y << "\n";
 cout << "IdentEqList" << h << " " << w << "\n";
#endif
if(elem != NULL)
 {
 elem->SetPosition(x,y);
 x1 = x + elem->Get_W() + Tree_Node::inUseFont()->Width(", ");
 if(next != NULL)
   next->SetPosition(x1,y);
 }
}

void Ident_Eq_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = (next == NULL || next->next == NULL)? elem->Get_W()
		             : elem->Get_W() + next->Get_W() + Tree_Node::inUseFont()->Width(", ");
 }
aligline = round(h/2.0);
}

void Ident_Eq_List::SetCollapsed(char t)
{
collapsed = t;
if(elem != NULL)
  elem->SetCollapsed(t);
if(next != NULL)
  next->SetCollapsed(t);
}

void Ident_Eq_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

/*								*/
/*	implementation of class Local_Def methods		*/
/*								*/

Local_Def::Local_Def(Ident_Eq_List *e_l, Tree_Node *b)
{
type = LOCALDEF;
equa_list = e_l;
bex = b;

Set_Unparser(new Gl_Local_Def(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Local_Def::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Local_Def" << x << " " << y << "\n";
 cout << "Local_Def" << h << " " << w << "\n";
#endif

if (textual)
	{					/* x is the position og LET */
	x1 = x + Tree_Node::inUseFont()->Width("let ") + border;
	equa_list->SetPosition(x1, y - border);
	yl = x1 + equa_list->Get_W();		/*yl is the position of IN */
	x1 = x + SMALL_BORDER + border;
	y1 = y - equa_list->Get_H() - VBORDER - border;
	bex->SetPosition(x1,y1);
	}
     else {
	x1 = x + BORDER;
	y1 = y - VBORDER;
	equa_list->SetPosition(x1,y1);
	yl = y1 - equa_list->Get_H() - VBORDER;
	y1 = yl - BORDER;
	x1 = x + round((w - bex->Get_W())/2.0);
	bex->SetPosition(x1,y1);
	}
}

void Local_Def::SetDimensions(void)
{
bex->SetDimensions();
equa_list->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:    if(textual)
                           {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
                           h = Tree_Node::inUseFont()->Height();
                           if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
                              {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
                         else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
                              h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
                        break;
 case COLLAPSED_INV:    w = 0; h = 0;
                        break;
 case NOCOLLAPSED:
	if(textual)
   	  {
 	  h = equa_list->Get_H() + VBORDER + bex->Get_H();
   	  w = Max(equa_list->Get_W() + Tree_Node::inUseFont()->Width("let ") + 
                  Tree_Node::inUseFont()->Width(" in "), bex->Get_W() + SMALL_BORDER);
          if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
            {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
          }
 	 else {
      	      w = Max(bex->Get_W(), equa_list->Get_W());
              w += 2 * BORDER;
              h = bex->Get_H() + equa_list->Get_H() + 2 * (VBORDER + BORDER);
              }
	break;
 }
aligline = round(h/2.0);
}

void Local_Def::SetFather(Tree_Node *f)
{
father = f;
equa_list->SetFather(this);
bex->SetFather(this);
}
	
void Local_Def::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
equa_list->SetCollapsed(t);
bex->SetCollapsed(t);
}

void Local_Def::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex->SetTextual(t);
}


/*								*/
/*	implementation of class Hide methods			*/
/*								*/

Hide::Hide(ID_List *g_l, Tree_Node *b)
{
type = HIDE;
alignement = VER;
gate_list = g_l;
bex = b;

Set_Unparser(new Gl_Hide(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Hide::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Hide" << x << " " << y << "\n";
 cout << "Hide" << h << " " << w << "\n";
#endif

if (textual)
	{					/* x is the position of Hide */
	x1 = x + Tree_Node::inUseFont()->Width("hide ") + border;
	gate_list->SetPosition(x1, y - border);
	yl = x1 + gate_list->Get_W();		/* position of IN */ 
	x1 = x + SMALL_BORDER + border;
	y1 = y - gate_list->Get_H() - VBORDER - border;
	bex->SetPosition(x1, y1);
	}
     else {
	x1 = x + HIDE_W + BORDER;
	y1 = y - VBORDER;
	gate_list->SetPosition(x1,y1);
	yl = y1 - gate_list->Get_H() - VBORDER;
	y1 = yl - BORDER;
	x1 = x + BORDER;
	bex->SetPosition(x1,y1);
	}
}

void Hide::SetDimensions(void)
{
int foo;

bex->SetDimensions();
gate_list->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED:
      if(textual == TRUE)
	{
	h = gate_list->Get_H() + VBORDER + bex->Get_H();
	foo = Tree_Node::inUseFont()->Width("hide ") + gate_list->Get_W() +
		Tree_Node::inUseFont()->Width(" in ");
	w = Max(foo, bex->Get_W() + SMALL_BORDER);
	if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	  {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	}
     else {
	w = Max(bex->Get_W(), HIDE_W + gate_list->Get_W());
	w += 2 * BORDER;
	h = bex->Get_H() + gate_list->Get_H() + 2 * (VBORDER + BORDER);
	}
 }
aligline = round(h/2.0);
}

void Hide::SetFather(Tree_Node *f)
{
father = f;
gate_list->SetFather(this);
bex->SetFather(this);
}

void Hide::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
gate_list->SetCollapsed(t);
bex->SetCollapsed(t);
}


void Hide::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex->SetTextual(t);
}

/*								*/
/*	implementation of class Interl methods		*/
/*								*/

Interl::Interl()
{
type = INTERL;
alignement = glow->GetAligs(INTERL);
selectable = FALSE;

Set_Unparser(new Gl_Interl(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Interl::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Interl" << x << " " << y << "\n";
 cout << "Interl" << h << " " << w << "\n";
 cout << "Interl" << visible << " " << collapsed << "\n";
#endif
}

void Interl::SetDimensions(void)
{
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED:
 	if(textual)
	  {
	  w = Tree_Node::inUseFont()->Width("||| ");
	  h = Tree_Node::inUseFont()->Height();
	  }
         else {
	      if(father->GetAlignement() == HOR)
                 {
	         w = 2 * BORDER;
	         h = 0;
	         }
               else {
	            w = 0;
	            h = 2 * BORDER;
	            }
	      }
 }
aligline = round(h/2.0);
}

void Interl::SetFather(Tree_Node *f)
{
father = f;
father->UpdateAlig(INTERL);
}

/*								*/
/*	implementation of class Syncr methods		*/
/*								*/

Syncr::Syncr()
{
type = SYNCR;
alignement = glow->GetAligs(SYNCR);
selectable = FALSE;

Set_Unparser(new Gl_Syncr(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Syncr::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Syncr" << x << " " << y << "\n";
 cout << "Syncr" << h << " " << w << "\n";
#endif
}

void Syncr::SetDimensions(void)
{
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED:
	if(textual)
 	  {
	  w = Tree_Node::inUseFont()->Width("|| ");
	  h = Tree_Node::inUseFont()->Height();
	  }
        else {
             if(father->GetAlignement() == HOR)
    	       {
	       w = 2 * (SYNCR_R + SYNCR_L);
	       h = 2 * SYNCR_R;
  	       }
	      else {
	           w = 2 * SYNCR_R;
	           h = 2 * (SYNCR_R + SYNCR_L);
      	           }
	     }
 }
aligline = round(h/2.0);
}

void Syncr::SetFather(Tree_Node *f)
{
father = f;
father->UpdateAlig(SYNCR);
}

/*                                                              */
/*      implementation of class Enable methods                  */
/*                                                              */


Enable::Enable(Tree_Node *b1, ID_List *g_i_l, Tree_Node *b2)
{
type = ENABLE;
alignement = glow->GetAligs(ENABLE);
bex1 = b1;
gate_id_list = g_i_l;
bex2 = b2;

Set_Unparser(new Gl_Enable(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Enable::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Enable" << x << " " << y << "\n";
 cout << "Enable" << h << " " << w << "\n";
#endif

if(textual)
   {
   SetTerminalPos();
   bex1->SetPosition(xl, y - border);
   if(gate_id_list->GetVisible())
      {
      gate_id_list->SetPosition(xl + Tree_Node::inUseFont()->Width("accept "),yid);
      bex2->SetPosition(xl, yid - gate_id_list->Get_H() - VBORDER);
      }
    else bex2->SetPosition(xl, yid);	
   }
  else {
       if(alignement == HOR)
          {
	  if(havebox)
  	    x1 = x + BORDER;
  	   else x1 = x;
	  y1 = y - aligline + bex1->GetAligLine();
	  bex1->SetPosition(x1,y1);

	  if(havebox)
	    xl = x + bex1->Get_W() + BORDER;
	   else xl = x + bex1->Get_W();
	  yl1 = y - aligline;

	  xid = xl + ENAB_L;
	  yid = yl1 + round(gate_id_list->Get_H()/2.0) + VBORDER;
	  gate_id_list->SetPosition(xid + BORDER,yid - VBORDER);

	  yl2 = (gate_id_list->Get_W() != 0)? 
		xid + gate_id_list->Get_W() + 2 * BORDER
		  : xid;
	  x1 = yl2 + ENAB_L + ENAB_S;
	  y1 = y - aligline + bex2->GetAligLine();
	  bex2->SetPosition(x1,y1);
          }
        else {
	     xl = x + round(w/2.0);
  	     x1 = x + round((w - bex1->Get_W())/2.0);
	     if(havebox)
	        y1 = y - BORDER;
	      else y1 = y;
	     bex1->SetPosition(x1,y1);
	     yl1 = y1 - bex1->Get_H();
	     xid = x + round((w - gate_id_list->Get_W())/2.0) - BORDER;
	     yid = yl1 - ENAB_L;
	     gate_id_list->SetPosition(xid + BORDER,yid - VBORDER);
	     yl2 = (gate_id_list->Get_H() != 0)? 
		   yid - gate_id_list->Get_H() - 2 * VBORDER
		     : yid - gate_id_list->Get_H();
	     x1 = x + round((w - bex2->Get_W())/2.0);
	     y1 = yl2 - ENAB_L - ENAB_S;
	     bex2->SetPosition(x1,y1);
             }
	}
}

void Enable::SetDimensions(void)
{
bex1->SetDimensions();
gate_id_list->SetDimensions();
bex2->SetDimensions();
border = 0;

switch(collapsed) 
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
			aligline = round(h/2.0);
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0; aligline = 0;
			break;
 case NOCOLLAPSED:
     if(textual)
	{
        h = bex1->Get_H() + bex2->Get_H() + 2*VBORDER + Tree_Node::inUseFont()->Height();
	w = Get_Textual_W();
        if(gate_id_list->GetVisible())
	 h += VBORDER + Tree_Node::inUseFont()->Height();
	if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	  {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	aligline = round(h/2.0);
	}
       else {
            if(alignement == HOR)
               {
	       int b1,b2;
	       aligline = Max((round(ENAB_S/2.0)),(round(gate_id_list->Get_H()/2.0) + VBORDER));
	       aligline = Max(aligline,bex1->GetAligLine());
	       aligline = Max(aligline,bex2->GetAligLine());

	       if((gate_id_list->Get_H() - gate_id_list->GetAligLine()) >
	    	  round(ENAB_S/2.0))
                  b1 = gate_id_list->Get_H() - gate_id_list->GetAligLine();
      	 	else b1 =  round(ENAB_S/2.0);
	       if((bex1->Get_H() - bex1->GetAligLine()) > 
		  (bex2->Get_H() - bex1->GetAligLine()))
		  b2 = bex1->Get_H() - bex1->GetAligLine();
		else b2 = bex2->Get_H() - bex1->GetAligLine();
	       if(b1 > b2)
		 h = aligline + b1;
		else h = aligline + b2;
  	       w = bex1->Get_W() + gate_id_list->Get_W() + bex2->Get_W();
	       w += (gate_id_list->Get_W() == 0)? 2 * ENAB_L + ENAB_S
				    : 2 * ENAB_L + ENAB_S + 2 * BORDER;
	       if(havebox)
		 {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
                }
             else {
	          w = Max4(bex1->Get_W(), gate_id_list->Get_W() + 2*BORDER, 
		           bex2->Get_W(), ENAB_S);
  	          h = bex1->Get_H() + gate_id_list->Get_H() + bex2->Get_H();
	          h += (gate_id_list == 0)? 2 * ENAB_L + ENAB_S
				       : 2 * ENAB_L + ENAB_S + 2 * VBORDER;
		  aligline = round(h/2.0);
		  if(havebox)
		    {w += 2 * BORDER; h += 2 * BORDER;}
                  }
	    }
 }
}

void Enable::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex1->SetTextual(t);
bex2->SetTextual(t);
}

void Enable::SetTerminalPos()

/* x, y are the coordinate of (					*/
/* xid, yid are the coordinate of >>				*/
/* xl, yl1 are the coordinate of accept 			*/
/* xl2, yl2 are the coordinate of in				*/
{
xid = x + border;
yid = y - bex1->Get_H() - VBORDER - border;
xl =  xid + Tree_Node::inUseFont()->Width(">> ");
if(gate_id_list->GetVisible())
  {
  yl1 = yid;
  xl2 = xl + Tree_Node::inUseFont()->Width("accept ") + gate_id_list->Get_W();
  yl2 = yid;
  Yclose = yid - gate_id_list->Get_H() -  bex2->Get_H() - 2 * VBORDER;
  }
 else Yclose = yid - bex2->Get_H() - VBORDER;
}

int Enable::Get_Textual_W()
{
int b;
b = (gate_id_list->GetVisible())? Max(bex2->Get_W(), gate_id_list->Get_W() + 
		Tree_Node::inUseFont()->Width("accept  in "))
    : bex2->Get_W();
b = Max(bex1->Get_W(), b);
return(Tree_Node::inUseFont()->Width(">> ") + b);

}

int Enable::Get_Textual_H()
{ return(bex1->Get_H() + bex2->Get_H() + 2*VBORDER + Tree_Node::inUseFont()->Height()); }

void Enable::SetFather(Tree_Node *f)
{
char ft;
ft = (f->GetType() == PARALLEL)? ((Parallel *)f)->GetOperType(): f->GetType();
if(gate_id_list->GetVisible())
   havebox = HaveBox(9,ft);
 else havebox = HaveBox(ENABLE,ft);
father = f;
bex1->SetFather(this);
gate_id_list->SetFather(this);
bex2->SetFather(this);
}

void Enable::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
bex1->SetCollapsed(t);
gate_id_list->SetCollapsed(t);
bex2->SetCollapsed(t);
}


/*                                                              */
/*      implementation of class Disable methods                 */
/*                                                              */


Disable::Disable(Tree_Node *b1, Tree_Node *b2)
{
type = DISABLE;
alignement = glow->GetAligs(DISABLE);
bex1 = b1;
bex2 = b2;

Set_Unparser(new Gl_Disable(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Disable::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Disable" << x << " " << y << "\n";
 cout << "Disable" << h << " " << w << "\n";
#endif
if(textual)
   {
   SetTerminalPos();
   x1 = xl + Tree_Node::inUseFont()->Width("[> ");
   bex1->SetPosition(x1, y - border);
   bex2->SetPosition(x1, yl);
   }
 else {
      if(alignement == HOR)
         {
	 if(havebox)
            x1 = x + BORDER;
          else x1 = x;
	 y1 = y - aligline + bex1->GetAligLine();
	 bex1->SetPosition(x1,y1);

	 x1 = x1 + bex1->Get_W() + SMALL_PEN + SYNCR_L + 2 * SYNCR_R;
	 y1 = y - aligline + bex2->GetAligLine();
	 bex2->SetPosition(x1,y1);
	 if(havebox)
	   xl = x + bex1->Get_W() + BORDER;
	  else xl = x + bex1->Get_W();
	 yl = y - aligline +  SYNCR_R;
	 }
       else {
	    if(havebox)
              y1 = y - BORDER;
             else y1 = y;
	    x1 = round(x + (w - bex1->Get_W())/2.0);
	    bex1->SetPosition(x1,y1);
	    yl = y1 - bex1->Get_H() - SMALL_PEN - SYNCR_L - 2 * SYNCR_R;
	    xl = round(x + (w - bex2->Get_W())/2.0);
	    bex2->SetPosition(xl,yl);
	    xl = x + round(w/2) - SYNCR_R;
	    if(havebox)
	       yl = y - bex1->Get_H() - BORDER;
	     else yl = y - bex1->Get_H();
            }
      }

}

void Disable::SetDimensions(void)
{
bex1->SetDimensions();
bex2->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
			aligline = round(h/2.0);
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0; aligline = 0;
			break;
 case NOCOLLAPSED:
 	if(textual)
	   {
	   h = Get_Textual_H();
	   w = Get_Textual_W(); 
	   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	   aligline = round(h/2.0);
	   }
         else {
	      if(alignement == HOR)
                 {
		 int b1;
	         w = bex1->Get_W() + bex2->Get_W();
      	         w += SYNCR_L + 2 * SYNCR_R + SMALL_PEN;

		 aligline = Max(bex1->GetAligLine(), bex2->GetAligLine());
		 if((bex1->Get_H() - bex1->GetAligLine()) >
		    (bex2->Get_H() - bex2->GetAligLine()))
  	            b1 = bex1->Get_H() - bex1->GetAligLine();
		  else  b1 = bex2->Get_H() - bex2->GetAligLine();
		 h = aligline + b1;
		 if(havebox)
		   {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
                 }
               else {
	            h = bex1->Get_H() + bex2->Get_H();
      	            h += SYNCR_L + 2 * SYNCR_R + SMALL_PEN;
      	            w = Max(bex1->Get_W(), bex2->Get_W());
		    aligline = bex1->GetAligLine();
		    if(havebox)
		      {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
		    }
	      }
 }
}

void Disable::SetFather(Tree_Node *f)
{
char ft;
ft = (f->GetType() == PARALLEL)? ((Parallel *)f)->GetOperType(): f->GetType();
havebox = HaveBox(DISABLE,ft);
father = f;
bex1->SetFather(this);
bex2->SetFather(this);
}

void Disable::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
bex1->SetCollapsed(t);
bex2->SetCollapsed(t);
}

void Disable::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex1->SetTextual(t);
bex2->SetTextual(t);
}

void Disable::SetTerminalPos()
{

if(father->GetType() == DISABLE && 
   father->GetTextual())        /* xl, yl is the position of [> */
   {
   xl = x - Tree_Node::inUseFont()->Width("[> ") + border;
   yl = y - bex1->Get_H() - VBORDER - border;
   }
  else {                               /* x,y is the position of ( */
       xl = x + border;
       yl = y - bex1->Get_H() - VBORDER - border;
       yl2 = yl - bex2->Get_H() - VBORDER;
       }
}

int Disable::Get_Textual_W()
{
int b;
b = Max(bex1->Get_W(), bex2->Get_W());
if((father->GetType() == DISABLE) && father->GetTextual())
   return(b);
return(Tree_Node::inUseFont()->Width("[> ") + b);
}

int Disable::Get_Textual_H()
{
if(father->GetType() == DISABLE && father->GetTextual())
   return(bex1->Get_H() + bex2->Get_H() + VBORDER);
 else return(bex1->Get_H() + bex2->Get_H() + 2 * VBORDER + Tree_Node::inUseFont()->Height());
}

/*                                                              */
/*      implementation of class Gen_Paral methods               */
/*                                                              */


Gen_Paral::Gen_Paral(ID_List *g_i_l)
{
type = GEN_PARAL;
alignement = glow->GetAligs(GEN_PARAL);
selectable = FALSE;
gate_id_list = g_i_l;
visible = g_i_l->GetVisible();

Set_Unparser(new Gl_Gen_Paral(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void Gen_Paral::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Gen_Paral" << x << " " << y << "\n";
 cout << "Gen_Paral" << h << " " << w << "\n";
#endif

if(textual)
   {			/* x is the position of |[ */
   x1 = x + Tree_Node::inUseFont()->Width(" |[");	
   gate_id_list->SetPosition(x1, y);
   Xclose = x1 + gate_id_list->Get_W();	/* position of ]| */
   }
 else {
      if(father->GetType() == PAR)
	   gate_id_list->SetPosition(x + ROUND_CORNER, y  - ROUND_CORNER);
       else {
            if(father->GetAlignement() == HOR)
               gate_id_list->SetPosition(x + SYNCR_L + ROUND_CORNER,y -ROUND_CORNER);
             else gate_id_list->SetPosition(x + ROUND_CORNER,y-SYNCR_L - ROUND_CORNER);
            }
	}
}

void Gen_Paral::SetDimensions(void)
{
gate_id_list->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED:
 	if(textual)
	   {
	   h = Tree_Node::inUseFont()->Height();
	   w = Tree_Node::inUseFont()->Width(" |[") + gate_id_list->Get_W() +
		 Tree_Node::inUseFont()->Width("]| ");
	   }
         else {
	      if(father->GetType() == PAR)
	        {
	        h = gate_id_list->Get_H() + 2 * ROUND_CORNER;
	        w = gate_id_list->Get_W() + 2 * ROUND_CORNER;
	        }
	       else {
                    if(father->GetAlignement() == HOR)
                       {
	               h = gate_id_list->Get_H() + 2 * ROUND_CORNER;
	               w = gate_id_list->Get_W() + 2 * ROUND_CORNER + 2 * SYNCR_L;
                       }
                     else {
	                  h = gate_id_list->Get_H() + 2 * ROUND_CORNER + 2 * SYNCR_L;
	                  w = gate_id_list->Get_W() + 2 * ROUND_CORNER;
                          }
	            }
	      }
 }
aligline = round(h/2.0);
}

void Gen_Paral::SetFather(Tree_Node *f)
{
father = f;
father->UpdateAlig(GEN_PARAL);
gate_id_list->SetFather(this);
}

void Gen_Paral::SetCollapsed(char t)
{
collapsed = t; 
gate_id_list->SetCollapsed(t);
}


/*                                                              */
/*      implementation of class Action_Pref methods             */
/*                                                              */


Action_Pref::Action_Pref(Tree_Node *a_d, Tree_Node *b)
{
type = ACTION_PREF;
alignement = glow->GetAligs(ACTION_PREF);
action_den = a_d;
bex = b;

Set_Unparser(new Gl_Action_Pref(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Action_Pref::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;

x = xx;
y = yy;
#ifdef DEBUG
 cout << "Action_Pref" << x << " " << y << "\n";
 cout << "Action_Pref" << h << " " << w << "\n";
#endif

if(textual)
   {
   action_den->SetPosition(x + border, y - border);
    y1 = y - action_den->Get_H() - VBORDER - border;
   x1 = x + SMALL_BORDER + border;
   bex->SetPosition(x1, y1);
   }
 else {
      if(alignement == HOR)
         {
         y1 = y - aligline + action_den->GetAligLine();
	 if(havebox)
           {
	   action_den->SetPosition(x + BORDER,y1);
	   x1 = x + action_den->Get_W() + BORDER;
	   }
          else 
           {
	   action_den->SetPosition(x,y1);
	   x1 = x + action_den->Get_W();
	   }
         y1 = y - aligline + bex->GetAligLine();
	 bex->SetPosition(x1,y1);
         }
       else {
            x1 = round(x + (w - action_den->Get_W())/2.0);
	    if(havebox)
	      y1 = y - BORDER;
	     else y1 = y;
	    action_den->SetPosition(x1,y1);
	    x1 = round(x + (w - bex->Get_W())/2.0);
	    y1 = y1 - action_den->Get_H();
	    bex->SetPosition(x1,y1);
            }
	}

}

void Action_Pref::SetDimensions(void)
{
action_den->SetDimensions();
bex->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
			aligline = round(h/2.0);
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0; aligline = 0;
			break;
 case NOCOLLAPSED:
 	if(textual)
	   {
	   h = action_den->Get_H() + bex->Get_H() + VBORDER;
	   w = Max(action_den->Get_W(), bex->Get_W() + SMALL_BORDER);
	   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	   aligline = round(h/2.0);
 	   }
         else {
              if(alignement == HOR)
                 {
	         w = action_den->Get_W() + bex->Get_W();
		 aligline = Max(action_den->GetAligLine(),bex->GetAligLine());
		 if((action_den->Get_H() - action_den->GetAligLine()) >
                     (bex->Get_H() - bex->GetAligLine()))
		    h = aligline + action_den->Get_H() - action_den->GetAligLine();
                  else  h = aligline + bex->Get_H() - bex->GetAligLine();
		 if(havebox)
		   {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
                 }
               else {
	            h = action_den->Get_H() + bex->Get_H();
	            w = Max(action_den->Get_W(), bex->Get_W());
		    aligline = action_den->GetAligLine();
		    if(havebox)
		      {w += 2 * BORDER; h += 2 * BORDER; aligline += BORDER;}
                    }
	      }
 }
}

void Action_Pref::SetFather(Tree_Node *f)
{
char ft;
ft = (f->GetType() == PARALLEL)? ((Parallel *)f)->GetOperType(): f->GetType();
havebox = HaveBox(ACTION_PREF,ft);
father = f;
action_den->SetFather(this);
bex->SetFather(this);
}

void Action_Pref::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
action_den->SetCollapsed(t);
bex->SetCollapsed(t);
}

void Action_Pref::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
action_den->SetTextual(t);
bex->SetTextual(t);
}


/*                                                              */
/*      implementation of class Internal methods                */
/*                                                              */


Internal::Internal()
{
type = INTERNAL;
selectable = FALSE;
visible = TRUE;

Set_Unparser(new Gl_Internal(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void Internal::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Internal" << x << " " << y << "\n";
 cout << "Internal" << h << " " << w << "\n";
#endif
}

void Internal::SetDimensions(void)
{
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
        if(textual)
	   {
	   h = Tree_Node::inUseFont()->Height();
   	   w = Tree_Node::inUseFont()->Width("i ; ");
	   }
         else {
              if(father->GetAlignement() == HOR)
                 {
	         h = INT_EV_H;
	         w = INT_EV_W + INT_EV_L + SMALL_PEN;
                 }
               else {
	            h = INT_EV_H + INT_EV_L + SMALL_PEN;
	            w = INT_EV_W;
                    }
	      }
 }
aligline = round(h/2.0);
}

void Internal::SetFather(Tree_Node *f)
{ father = f;
}


/*                                                              */
/*      implementation of class Communication methods           */
/*                                                              */


Communication::Communication(ID_Place *i, Tree_Node *ex_op)
{
type = COMMUNICATION;
selectable = FALSE;
gate_identifier = i;
experiment_option = ex_op;

Set_Unparser(new Gl_Communication(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Communication::SetPosition(Coord xx, Coord yy)
{
int dum;
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Communication" << x << " " << y << "\n";
 cout << "Communication" << h << " " << w << "\n";
#endif

if(textual)
   {
   gate_identifier->SetPosition(x, y);
   dum = x + gate_identifier->Get_W() + Tree_Node::inUseFont()->Width(" ");
   experiment_option->SetPosition(dum, y) ;
   yl = dum + experiment_option->Get_W();	/*position of ; */
   }
 else {
      if(father->GetAlignement() == HOR)
         {
         int w1;
         w1 = w - SMALL_PEN - INT_EV_L;
         if(experiment_option->GetType() == EXPERIMENT &&
	   ((Experiment *)experiment_option)->GetGuard()->Get_W() == w1) 
	   {
	   dum = round((w1 - (gate_identifier->Get_W() +
		      experiment_option->Get_W() + ROUND_CORNER))/2.0);
	   x1 = x + dum;
	   }
	  else x1 = x + ROUND_CORNER; 
        gate_identifier->SetPosition(x1, y - ROUND_CORNER);
	yl = y - round(gate_identifier->Get_H()/2) - ROUND_CORNER;
	xl = x + w1;
        if(experiment_option->GetType() == EXPERIMENT)
          {
          dum =round((w1 -
		 ((Experiment *)experiment_option)->GetGuard()->Get_W())/2.0);
          x1 = x1 + gate_identifier->Get_W() + ROUND_CORNER;
	  experiment_option->SetPosition(x1, y - ROUND_CORNER);
	  ((Experiment *)experiment_option)->GetGuard()->SetPosition(x + dum,
                                       y - gate_identifier->Get_H() - 2 * ROUND_CORNER);
	  }
        }
      else {
           if(experiment_option->GetType() == EXPERIMENT &&
	      ((Experiment *)experiment_option)->GetGuard()->Get_W() == w) 
	    {
	    dum = round((w - (gate_identifier->Get_W() +
		 experiment_option->Get_W() + ROUND_CORNER))/2.0);
	    x1 = x + dum;
	    }
	   else x1 = x + ROUND_CORNER; 
	  gate_identifier->SetPosition(x1, y - ROUND_CORNER);
          yl = y - gate_identifier->Get_H() - 2 * ROUND_CORNER;
          xl = x + round(w/2.0);
	  if(experiment_option->GetType() == EXPERIMENT)
	    {
	    dum =round((w -
		 ((Experiment *)experiment_option)->GetGuard()->Get_W())/2.0);
	    x1 = x1 + gate_identifier->Get_W() + ROUND_CORNER;
	    experiment_option->SetPosition(x1, y - ROUND_CORNER);
	    ((Experiment *)experiment_option)->GetGuard()->SetPosition(x + dum,
                                       y - gate_identifier->Get_H() - 2 * ROUND_CORNER);
	    yl -= ((Experiment *)experiment_option)->GetGuard()->Get_H();
	    }
          }
        }
}

void Communication::SetDimensions(void)
{
gate_identifier->SetDimensions();
experiment_option->SetDimensions();

switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0; aligline = 0;
			break;
 case NOCOLLAPSED:
	if(textual)
	  {
	  h = Tree_Node::inUseFont()->Height();
  	  w = gate_identifier->Get_W() + experiment_option->Get_W() +
		Tree_Node::inUseFont()->Width(" ") + Tree_Node::inUseFont()->Width(" ; ");
	  aligline = round(h/2.0);
	  }
         else {
	      hr  = gate_identifier->Get_H() + 2 * ROUND_CORNER;
              if(father->GetAlignement() == HOR)
	          h = gate_identifier->Get_H() + 2 * ROUND_CORNER;
	       else h = gate_identifier->Get_H() + 2 * ROUND_CORNER  + SMALL_PEN + INT_EV_L;
	      w = gate_identifier->Get_W() + 2 * ROUND_CORNER;
	      if(experiment_option->GetType() == EXPERIMENT)
	        {
	        w += experiment_option->Get_W() + ROUND_CORNER;
	        if(((Experiment *)experiment_option)->GetGuard()->Get_W() > w)
	           w = ((Experiment *)experiment_option)->GetGuard()->Get_W();
	        h += ((Experiment *)experiment_option)->GetGuard()->Get_H();
	        }
               if(father->GetAlignement() == HOR)
	   	 w += SMALL_PEN + INT_EV_L;
	       aligline = round(gate_identifier->Get_H()/2.0) + ROUND_CORNER;
               }
 }
}

void Communication::SetFather(Tree_Node *f)
{
father = f;
gate_identifier->SetFather(this);
experiment_option->SetFather(this);
}

void Communication::SetCollapsed(char t)
{
collapsed = t;
gate_identifier->SetCollapsed(t);
experiment_option->SetCollapsed(t);
}

void Communication::SetTextual(char t, char)
{
textual = t;
experiment_option->SetTextual(t);
}


/*                                                              */
/*      implementation of class NoGuard methods         	*/
/*                                                              */


NoGuard::NoGuard()
{
type = NO_GUARD;
visible = FALSE;
selectable = FALSE;
w = 0; h = 0;
aligline = 0;

Set_Unparser(new Gl_NoGuard(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void NoGuard::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "NoGuard" << x << " " << y << "\n";
 cout << "NoGuard" << h << " " << w << "\n";
#endif
}

void NoGuard::SetDimensions(void)
{ }

void NoGuard::SetFather(Tree_Node *f)
{ father = f; }


/*                                                              */
/*      implementation of class Guard methods           */
/*                                                              */


Guard::Guard(Equality *eq)
{
type = GUARD;
visible = TRUE;
selectable = FALSE;
equality = eq;

Set_Unparser(new Gl_Guard(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void Guard::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Guard" << x << " " << y << "\n";
 cout << "Guard" << h << " " << w << "\n";
#endif
if (textual)
	equality->SetPosition(x + Tree_Node::inUseFont()->Width(" ["), y);
      else
	equality->SetPosition(x+BORDER, y - VBORDER);
}

void Guard::SetDimensions(void)
{
equality->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
      if(textual)
	{
	h = Tree_Node::inUseFont()->Height();
	w = equality->Get_W() + Tree_Node::inUseFont()->Width(" [") + Tree_Node::inUseFont()->Width("]");
	}
       else {
	    h = equality->Get_H() + 2 * VBORDER;
	    w = equality->Get_W() + 2 * BORDER;
	    }
 }
aligline = round(h/2.0);
}

void Guard::SetFather(Tree_Node *f)
{
father = f;
equality->SetFather(this);
}

void Guard::SetCollapsed(char t)
{
collapsed = t;
equality->SetCollapsed(t);
}


/*                                                              */
/*      implementation of class NoExperiment methods            */
/*                                                              */


NoExperiment::NoExperiment()
{
type = NO_EXPERIMENT;
visible = FALSE;
selectable = FALSE;
w = 0; h = 0;
aligline = 0;

Set_Unparser(new Gl_NoExperiment(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void NoExperiment::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "NoExperiment" << x << " " << y << "\n";
 cout << "NoExperiment" << h << " " << w << "\n";
#endif
}

void NoExperiment::SetDimensions(void)
{ }

void NoExperiment::SetFather(Tree_Node *f)
{ father = f; }


/*                                                              */
/*      implementation of class Experiment methods              */
/*                                                              */


Experiment::Experiment(Exper_Off_List *e_of_l, Tree_Node *g_op)
{
type = EXPERIMENT;
visible = TRUE;
selectable = FALSE;
exp_offer_list = e_of_l;
guard_option = g_op;

Set_Unparser(new Gl_Experiment(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void Experiment::SetPosition(Coord xx, Coord yy)
{

x = xx;
y = yy;
#ifdef DEBUG
 cout << "Experiment" << x << " " << y << "\n";
 cout << "Experiment" << h << " " << w << "\n";
#endif
if (textual)
	{
	exp_offer_list->SetPosition(x , y); 
	guard_option->SetPosition(x + exp_offer_list->Get_W(), y);
	}
      else
	exp_offer_list->SetPosition(x + ROUND_CORNER, y);
}

void Experiment::SetDimensions(void)
{
exp_offer_list->SetDimensions();
guard_option->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	if (textual)
	{
	h = Tree_Node::inUseFont()->Height();
	w = exp_offer_list->Get_W() + guard_option->Get_W();
	}
      else {
 	   h = exp_offer_list->Get_H();
	   w = exp_offer_list->Get_W() + ROUND_CORNER; 
	   }
 }
aligline = round(h/2.0);
}

void Experiment::SetFather(Tree_Node *f)
{
father = f;
exp_offer_list->SetFather(this);
guard_option->SetFather(this);
}

void Experiment::SetCollapsed(char t)
{
collapsed = t;
exp_offer_list->SetCollapsed(t);
guard_option->SetCollapsed(t);
}


void Experiment::SetTextual(char t, char)
{
textual = t;
guard_option->SetTextual(t);
}

/*								*/
/*	implementation of class Proc_Inst methods		*/
/*								*/

Proc_Inst::Proc_Inst(ID_Place *id, ID_List *g_i_l, Value_Expr_List *v_l)
{
Gl_Sheet *sh;
type = PROCINST;
visible = TRUE;

ident = id;
gate_list = g_i_l;
value_expr_list = v_l;

Set_Unparser(new Gl_Proc_Inst(this));
sh = (glow->Get_Folder())->Current();
sh->Add(Get_Unparser());
sh->SetRoot(this);
}

void Proc_Inst::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Proc Inst" << x << " " << y << "\n";
 cout << "Proc Inst" << h << " " << w << "\n";
#endif

if ( textual)
	{
	ident->SetPosition(x + border,y - border);
	yp = yl1 = x + ident->Get_W() + border;  /* yp position of [*/	
	if (gate_list->GetVisible())
		{
		x1 = yp + Tree_Node::inUseFont()->Width(" [");
		gate_list->SetPosition(x1, y - border);
		yl1 = x1 + gate_list->Get_W();       /* yl1 position of ] */
		}
	if (value_expr_list->GetVisible())
		{
		Xopen = yl1 + Tree_Node::inUseFont()->Width("] "); /*position of ( */
		x1 = Xopen + Tree_Node::inUseFont()->Width(" (");
		value_expr_list->SetPosition(x1, y - border);
		Xclose = x1 + value_expr_list->Get_W(); /*poistion of )*/
		}
	}

	else {
	  x1 = x + round((w - gate_list->Get_W()) / 2.0);
	  gate_list->SetPosition(x1,y);

	  x1 = x + round((w - ident->Get_W()) / 2.0);
	  yp = y - gate_list->Get_H();
	  y1 = yp  - VBORDER;
	  ident->SetPosition(x1,y1);
	  yl1 = y1 - ident->Get_H() - VBORDER; 
	  x1 = x + round((w - value_expr_list->Get_W()) / 2.0);
	  y1 = yl1  - VBORDER;
	  value_expr_list->SetPosition(x1,y1);
	  }

}

void Proc_Inst::SetDimensions(void)
{
ident->SetDimensions();
gate_list->SetDimensions();
value_expr_list->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
		 	      }
			aligline = round(h/2.0);
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0; aligline = 0;
			break;
 case NOCOLLAPSED: 	
     if(textual)
	{
	h = Tree_Node::inUseFont()->Height();
	w = ident->Get_W() + gate_list->Get_W() + value_expr_list->Get_W(); 
	w += (gate_list->GetVisible())? Tree_Node::inUseFont()->Width(" [") + 
		Tree_Node::inUseFont()->Width("] "):0; 
	w += (value_expr_list->GetVisible())? Tree_Node::inUseFont()->Width(" (") + 
		Tree_Node::inUseFont()->Width(") "):0; 
	if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	  {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	aligline = round(h/2.0);
	}
	else {
	  w = Max(ident->Get_W(),value_expr_list->Get_W());
	  w = Max(w, gate_list->Get_W());
	  w += 2 * BORDER;
	  if(value_expr_list->Get_H() != 0)
	    hp = ident->Get_H() + value_expr_list->Get_H() + 4 * VBORDER;
	   else hp = ident->Get_H() + 2 * VBORDER;
	  h = hp + gate_list->Get_H();
	  aligline = gate_list->Get_H() + round(ident->Get_H()/2.0) + VBORDER;
	  }
 }
}

void Proc_Inst::SetFather(Tree_Node *f)
{
father = f;
ident->SetFather(this);
gate_list->SetFather(this);
value_expr_list->SetFather(this);
}

void Proc_Inst::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
ident->SetCollapsed(t);
gate_list->SetCollapsed(t);
value_expr_list->SetCollapsed(t);
}

void Proc_Inst::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
gate_list->SetTextual(t);
}


/*								*/ 
/*	implementation of class Value_Expr methods		*/ 
/*								*/ 

Value_Expr::Value_Expr() 
{ 
type = VALEXPR; 
textual = TRUE;
visible = FALSE; 
selectable = FALSE;
w = 0; h = 0;
aligline = 0;

Set_Unparser(new Gl_Value_Expr(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Value_Expr::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "ValueExpr" << x << " " << y << "\n";
 cout << "ValueExpr" << h << " " << w << "\n";
 cout << "ValueExpr" << visible << " " << collapsed << "\n";
#endif
}

void Value_Expr::SetDimensions(void)
{ }

void Value_Expr::SetFather(Tree_Node *f)
{ father = f; }


/*								*/
/*	implementation of class Value_Expr_List methods		*/
/*								*/

Value_Expr_List::Value_Expr_List(Tree_Node *el, Value_Expr_List *nxt)
{
type = VALUEEXPRLIST;
textual = TRUE;
selectable = FALSE;
elem = el;
next = nxt;
visible = elem->GetVisible();

Set_Unparser(new Gl_Value_Expr_List(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Value_Expr_List::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "ValueExprList" << x << " " << y << "\n";
 cout << "ValueExprList" << h << " " << w << "\n";
#endif
if(elem != NULL)
 {
 elem->SetPosition(x,y);
 x1 = x + elem->Get_W() + Tree_Node::inUseFont()->Width(", ");
 if(next != NULL)
   next->SetPosition(x1,y);
 }
}

void Value_Expr_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = (next == NULL || next->next == NULL)? elem->Get_W()
		      		: elem->Get_W() + next->Get_W() + Tree_Node::inUseFont()->Width(", ");
 }
aligline = round(h/2.0);
}

void Value_Expr_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

void Value_Expr_List::SetCollapsed(char t)
{
collapsed = t;
if(elem != NULL)
  elem->SetCollapsed(t);
if(next != NULL)
  next->SetCollapsed(t);
}


/*								*/
/*	implementation of class Sum_Ident methods			*/
/*								*/


Sum_Ident::Sum_Ident(Id_Decl_List *i_d_l, Tree_Node *b)
{
type = SUM_IDENT;
visible = TRUE;
alignement = VER;
ident_decl_list = i_d_l;
bex = b;

Set_Unparser(new Gl_Sum_Ident(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Sum_Ident::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Sum_Ident" << x << " " << y << "\n";
 cout << "Sum_Ident" << h << " " << w << "\n";
#endif
if (textual)
	{		
	SetTerminalPos();
	x1 = Xch + Tree_Node::inUseFont()->Width("choice ");
	y1 = Ych;
        ident_decl_list->SetPosition(x1, y1);
	x1 = x + SMALL_BORDER;
	y1 = y - ident_decl_list->Get_H() - VBORDER - border;
	bex->SetPosition(x1, y1);
	}
     else {
	x1 = x + round((w - ident_decl_list->Get_W() - 
			2 * LINE_SPACE)/2.0) + BORDER;
	y1 = y - 2 * VBORDER;
	ident_decl_list->SetPosition(x1, y1);
	yl = y1 - 2 * VBORDER - ident_decl_list->Get_H();
	x1 = x + round((w - bex->Get_W())/2.0);
	y1 = yl - BORDER; 
	bex->SetPosition(x1,y1);
	}
}

void Sum_Ident::SetDimensions(void)

{
ident_decl_list->SetDimensions();
bex->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
     if(textual)
	{
	h = ident_decl_list->Get_H() + bex->Get_H() +
		 Tree_Node::inUseFont()->Height() + 2 * VBORDER;
	w = Max4(Tree_Node::inUseFont()->Width("(choice ") +
		  ident_decl_list->Get_W() + Tree_Node::inUseFont()->Width(" []"),
		 bex->Get_W() + SMALL_BORDER, Tree_Node::inUseFont()->Width(")"), 0);
        if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
           {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	}
      else {
	   w = Max(ident_decl_list->Get_W() + 2 * LINE_SPACE, bex->Get_W());
	   w += 2*BORDER;
           h = ident_decl_list->Get_H() + bex->Get_H() + 4*VBORDER + 2*BORDER; 
           }
 }
aligline = round(h/2.0);
}

void Sum_Ident::SetFather(Tree_Node *f)
{
father = f;
ident_decl_list->SetFather(this);
bex->SetFather(this);
}

void Sum_Ident::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
ident_decl_list->SetCollapsed(t);
bex->SetCollapsed(t);
}

void Sum_Ident::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex->SetTextual(t);
}

void Sum_Ident::SetTerminalPos()

/* x, y is the position of (					*/
/* Xch, Ych is the position of choice				*/
/* xl, yl is the position of []					*/
/* Xclose, Yclose is the position of )				*/
{
Ych = yl = y - border;

Xch = x + Tree_Node::inUseFont()->Width("(") + border; 
xl = Xch + Tree_Node::inUseFont()->Width("choice ") + ident_decl_list->Get_W();
Xclose = x + border;
Yclose = y - ident_decl_list->Get_H() - bex->Get_H() - 2 * VBORDER - border;
}


/*								*/
/*	implementation of class Value methods			*/
/*								*/

Value::Value(ID_Place *i, ID_Place *s)
{
type = VALUE;
textual = TRUE;
visible = TRUE;
selectable = FALSE;
ident = i;
sort_id = s;
if(glow->ShowOfSort())
   sort_id->SetVisible(TRUE);
 else sort_id->SetVisible(FALSE);

Set_Unparser(new Gl_Value(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
((glow->Get_Folder())->Current())->YesOfSort();
}

void Value::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Value" << x << " " << y << "\n";
 cout << "Value" << h << " " << w << "\n";
#endif
ident->SetPosition(x,y);
x1 = x + ident->Get_W() + Tree_Node::inUseFont()->Width(" of ");
sort_id->SetPosition(x1,y);
}

void Value::SetDimensions(void)
{
ident->SetDimensions();
sort_id->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			if(glow->ShowOfSort())
   			   {
   			   sort_id->SetVisible(TRUE);
 			   w = ident->Get_W() + sort_id->Get_W() + Tree_Node::inUseFont()->Width(" of ");
   			   }
 			 else {
      			      w = ident->Get_W();
      			      sort_id->SetVisible(FALSE);
      			      }
 }
aligline = round(h/2.0);
}

void Value::SetFather(Tree_Node *f)
{
father = f;
ident->SetFather(this);
sort_id->SetFather(this);
}

void Value::SetCollapsed(char t)
{
collapsed = t;
ident->SetCollapsed(t);
sort_id->SetCollapsed(t);
}



/*								*/
/*	implementation of class Term methods			*/
/*								*/

Term::Term(ID_Place *op, Value_Expr_List *el, ID_Place *s)
{
type = TERM;
textual = TRUE;
visible = TRUE;
selectable = FALSE;
op_ident = op;
expr_list = el;
sort_id = s;
visible = TRUE;

if(glow->ShowOfSort())
   sort_id->SetVisible(TRUE);
 else sort_id->SetVisible(FALSE);

Set_Unparser(new Gl_Term(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
((glow->Get_Folder())->Current())->YesOfSort();
}

void Term::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Term" << x << " " << y << "\n";
 cout << "Term" << h << " " << w << "\n";
#endif
op_ident->SetPosition(x,y);
if(expr_list->Get_W() == 0)
  {
  x1 = x + op_ident->Get_W();
  expr_list->SetPosition(x1,y);
  x1 = x1 + Tree_Node::inUseFont()->Width(" of ");
  sort_id->SetPosition(x1,y);
  }
 else {
      x1 = x + op_ident->Get_W() + Tree_Node::inUseFont()->Width("(");
      expr_list->SetPosition(x1,y);
      x1 = x1 + expr_list->Get_W() + Tree_Node::inUseFont()->Width(") of ");
      sort_id->SetPosition(x1,y);
      }
}

void Term::SetDimensions(void)
{
op_ident->SetDimensions();
expr_list->SetDimensions();
sort_id->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			if(glow->ShowOfSort())
   			   {
			   sort_id->SetVisible(TRUE);
			   w = op_ident->Get_W() + sort_id->Get_W() + Tree_Node::inUseFont()->Width(" of ");
			   }
			 else {
			      w = op_ident->Get_W();
			      sort_id->SetVisible(FALSE);
			      }
			w = (expr_list->Get_W() == 0)?w : w + expr_list->Get_W() + 
					  2 * Tree_Node::inUseFont()->Width(")");
 }
aligline = round(h/2.0);
}

void Term::SetFather(Tree_Node *f)
{
father = f;
op_ident->SetFather(this);
expr_list->SetFather(this);
sort_id->SetFather(this);
}

void Term::SetCollapsed(char t)
{
collapsed = t;
op_ident->SetCollapsed(t);
expr_list->SetCollapsed(t);
sort_id->SetCollapsed(t);
}



/*								*/ 
/*	implementation of class Exit_Entry methods		*/ 
/*								*/ 

Exit_Entry::Exit_Entry() 
{ 
type = EXITENTRY; 
textual = TRUE;
visible = FALSE; 
selectable = FALSE;
w = 0; h = 0;
aligline = 0;

Set_Unparser(new Gl_Exit_Entry(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Exit_Entry::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "ValueExpr" << x << " " << y << "\n";
 cout << "ValueExpr" << h << " " << w << "\n";
 cout << "ValueExpr" << visible << " " << collapsed << "\n";
#endif
}

void Exit_Entry::SetDimensions(void)
{ }

void Exit_Entry::SetFather(Tree_Node *f)
{ father = f; }


/*								*/
/*	implementation of class Exit_Entry_List methods		*/
/*								*/

Exit_Entry_List::Exit_Entry_List(Tree_Node *el, Exit_Entry_List *nxt)
{
type = EXITENTRYLIST;
selectable = FALSE;
elem = el;
next = nxt;
visible = elem->GetVisible();

Set_Unparser(new Gl_Exit_Entry_List(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Exit_Entry_List::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "ExitEntryList" << x << " " << y << "\n";
 cout << "ExitEntryList" << h << " " << w << "\n";
 cout << "ExitEntryList" << visible << " " << collapsed << "\n";
#endif
if(elem != NULL)
 {
 elem->SetPosition(x,y);
 x1 = x + elem->Get_W() + Tree_Node::inUseFont()->Width(", ");
 if(next != NULL)
   next->SetPosition(x1,y);
 }
}

void Exit_Entry_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
                        w = (next == NULL || next->next == NULL)? elem->Get_W()
		            : elem->Get_W() + next->Get_W() + Tree_Node::inUseFont()->Width(", ");
 }
aligline = round(h/2.0);
}

void Exit_Entry_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

void Exit_Entry_List::SetCollapsed(char t)
{
collapsed = t;
if(elem != NULL)
  elem->SetCollapsed(t);
if(next != NULL)
  next->SetCollapsed(t);
}



/*								*/
/*	implementation of class Sum_Gate methods			*/
/*								*/

Sum_Gate::Sum_Gate(Gate_Decl_List *g_d_l, Tree_Node *b)
{
type = SUM_GATE;
visible = TRUE;
alignement = VER;
gate_decl_list = g_d_l;
bex = b;
Set_Unparser(new Gl_Sum_Gate(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Sum_Gate::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Sum_Gate" << x << " " << y << "\n";
 cout << "Sum_Gate" << h << " " << w << "\n";
#endif

if (textual)
	{		
	SetTerminalPos();
	x1 = Xch + Tree_Node::inUseFont()->Width("choice ");
	y1 = Ych;
        gate_decl_list->SetPosition(x1, y1);
	x1 = x + SMALL_BORDER + border;
	y1 = y - gate_decl_list->Get_H() - VBORDER - border;
	bex->SetPosition(x1, y1);
	}
     else {
	x1 = x + round((w - gate_decl_list->Get_W() - 2 * LINE_SPACE)/2.0) 
			+ BORDER;
	y1 = y - 2 * VBORDER;

	gate_decl_list->SetPosition(x1, y1);

	yl = y1 - 2 * VBORDER - gate_decl_list->Get_H();

	y1 = yl - BORDER; 
	x1 = x + round((w - bex->Get_W())/2.0);
	bex->SetPosition(x1,y1);
	}

}

void Sum_Gate::SetDimensions(void)
{
gate_decl_list->SetDimensions();
bex->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
     if(textual)
	{
	h = gate_decl_list->Get_H() + bex->Get_H() +
		 Tree_Node::inUseFont()->Height() + 2 * VBORDER;
	w = Max4(Tree_Node::inUseFont()->Width("(choice ") +
		gate_decl_list->Get_W() + Tree_Node::inUseFont()->Width(" []"),
		bex->Get_W() + SMALL_BORDER, Tree_Node::inUseFont()->Width(")"), 0);
        if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
           {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	}
     else {
	  w =  Max(gate_decl_list->Get_W() + 2 * LINE_SPACE, bex->Get_W());
	  w += 2*BORDER;
	  h = gate_decl_list->Get_H() + bex->Get_H() + 4*VBORDER + 2*BORDER; 
  	  }
 }
aligline = round(h/2.0);
}

void Sum_Gate::SetFather(Tree_Node *f)
{
father = f;
gate_decl_list->SetFather(this);
bex->SetFather(this);
}

void Sum_Gate::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
gate_decl_list->SetCollapsed(t);
bex->SetCollapsed(t);
}

void Sum_Gate::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex->SetTextual(t);
}

void Sum_Gate::SetTerminalPos()

/* x, y is the position of (                                    */
/* Xch, Ych is the position of choice                           */
/* xl, yl is the position of []                                 */
/* Xclose, Yclose is the position of )                          */
{
Ych = yl = y;

Xch = x + Tree_Node::inUseFont()->Width("(") + border;
xl = Xch + Tree_Node::inUseFont()->Width("choice ") + gate_decl_list->Get_W();
Xclose = x + border;
Yclose = y - gate_decl_list->Get_H() - bex->Get_H() - 2 * VBORDER - border;
}



/*								*/
/*	implementation of class Gate_Decl methods			*/
/*								*/

Gate_Decl::Gate_Decl(ID_List *g_id_l1, ID_List *g_id_l2)
{
type = GATE_DECL;
textual = TRUE;
selectable = FALSE;
gate_id_list1 = g_id_l1;
gate_id_list2 = g_id_l2;

if(g_id_l1 == NULL)
  gate_id_list1 = new ID_List(new ID_Place(),NULL);
if(g_id_l2 == NULL)
  gate_id_list2 = new ID_List(new ID_Place(), NULL);
visible = (gate_id_list1->GetVisible() && gate_id_list2->GetVisible());

Set_Unparser(new Gl_Gate_Decl(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Gate_Decl::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Gate_Decl" << x << " " << y << "\n";
 cout << "Gate_Decl" << h << " " << w << "\n";
#endif
if(gate_id_list1 != NULL)
 {
 gate_id_list1->SetPosition(x,y);
 x1 = x + gate_id_list1->Get_W() + Tree_Node::inUseFont()->Width(" in [");
 if(gate_id_list2 != NULL)
   gate_id_list2->SetPosition(x1,y);
 }
}

void Gate_Decl::SetDimensions(void)
{
gate_id_list1->SetDimensions();
gate_id_list2->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = (visible)? gate_id_list1->Get_W() + gate_id_list2->Get_W()
				+ Tree_Node::inUseFont()->Width("in []  ") : 0;
 }
aligline = round(h/2.0);
}

void Gate_Decl::SetFather(Tree_Node *f)
{
father = f;
if(gate_id_list1 != NULL)
  gate_id_list1->SetFather(this);
if(gate_id_list2 != NULL)
  gate_id_list2->SetFather(this);
}

void Gate_Decl::SetCollapsed(char t)
{
collapsed = t;
if(gate_id_list1 != NULL)
  gate_id_list1->SetCollapsed(t);
if(gate_id_list2 != NULL)
  gate_id_list2->SetCollapsed(t);
}


/*								*/
/*	implementation of class Gate_Decl_List methods		*/
/*								*/

Gate_Decl_List::Gate_Decl_List(Gate_Decl *el, Gate_Decl_List *nxt)
{
type = GATE_DECL_LIST;
textual = TRUE;
selectable = FALSE;
elem = el;
next = nxt;
visible = elem->GetVisible();

Set_Unparser(new Gl_Gate_Decl_List(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Gate_Decl_List::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Gate_Decl_List" << x << " " << y << "\n";
 cout << "Gate_Decl_List" << h << " " << w << "\n";
#endif
if(elem != NULL)
 {
 elem->SetPosition(x,y);
 x1 = x + elem->Get_W() + Tree_Node::inUseFont()->Width(", ");
 if(next != NULL)
   next->SetPosition(x1,y);
 }
}

void Gate_Decl_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = (next == NULL || next->next == NULL)? elem->Get_W()
		             : elem->Get_W() + next->Get_W() + Tree_Node::inUseFont()->Width(", ");
 }
aligline = round(h/2.0);
}

void Gate_Decl_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

void Gate_Decl_List::SetCollapsed(char t)
{
collapsed = t;
if(elem != NULL)
  elem->SetCollapsed(t);
if(next != NULL)
  next->SetCollapsed(t);
}



/*								*/
/*	implementation of class Par methods			*/
/*								*/

Par::Par(Gate_Decl_List *g_d_l, Tree_Node *op, Tree_Node *b)
{
type = PAR;
visible = TRUE;
alignement = VER;
gate_decl_list = g_d_l;
bex = b;
oper = op;
if (textual)
 oper->SetVisible(TRUE);
  else {
	oper->SetVisible(FALSE);
	if(oper->GetType() == GEN_PARAL)
	     oper->SetVisible(TRUE);
	}

Set_Unparser(new Gl_Par(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Par::SetPosition(Coord xx, Coord yy)
{
Coord x1,y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Par" << x << " " << y << "\n";
 cout << "Par" << h << " " << w << "\n";
#endif

if (textual)
	{			
	x1 = x + Tree_Node::inUseFont()->Width("(par ") + border;
	gate_decl_list->SetPosition(x1, y - border);
	x1 = x1 + gate_decl_list->Get_W();
	oper->SetPosition(x1, y - border);
	y1 = y - gate_decl_list->Get_H() - VBORDER - border;
	x1 = x + SMALL_BORDER + border;
	bex->SetPosition(x1, y1);
	xl = x + border;
	yl = y1 - bex->Get_H() - VBORDER;
	}
     else {
	if(oper->GetType() == GEN_PARAL)
	  {
	  x1 = x + SMALL_BORDER;
	  y1 = y - ROUND_CORNER;
	  oper->SetPosition(x1,y1);
	  x1 = x1 + oper->Get_W() + BORDER; 
	  y1 = y1 - ROUND_CORNER;
	  gate_decl_list->SetPosition(x1,y1); 
	  yl = y1 - gate_decl_list->Get_H() - 2 * ROUND_CORNER;
	  y1 = yl - BORDER;
	  x1 = x + round((w - bex->Get_W())/2.0);
	  bex->SetPosition(x1,y1);
	  }
	 else {
	      x1 = x + round((w - gate_decl_list->Get_W() - 2 * LINE_SPACE)/2.0) 				+ BORDER;
	      y1 = y - 2 * VBORDER;
	      gate_decl_list->SetPosition(x1, y1);
	      yl = y1 - 2 * VBORDER - gate_decl_list->Get_H();
	      x1 = x + round((w - bex->Get_W())/2.0);
	      y1 = yl - BORDER; 
	      bex->SetPosition(x1,y1);
	      }
	}
}

void Par::SetDimensions(void)
{
bex->SetDimensions();
oper->SetDimensions();
gate_decl_list->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
		  	      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
     if(textual)
	{
	h = gate_decl_list->Get_H() + bex->Get_H() + Tree_Node::inUseFont()->Height() +
	       2 * VBORDER; 
	w = Max4(Tree_Node::inUseFont()->Width("(par ") + gate_decl_list->Get_W() +
		 oper->Get_W(), bex->Get_W(),
		Tree_Node::inUseFont()->Width(")"), 0);
	if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	  {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	}
      else {
	   if(oper->GetType() == GEN_PARAL)
	     {
	     w = Max(bex->Get_W(), oper->Get_W() + gate_decl_list->Get_W() 
				      + 2 * BORDER + SMALL_BORDER);
	     h = gate_decl_list->Get_H() + bex->Get_H() + 2*BORDER + 
			4*ROUND_CORNER; 
	     }
	    else {
	         w =  Max(gate_decl_list->Get_W() + 2 * LINE_SPACE, bex->Get_W());
	         w += 2*BORDER;
	         h = gate_decl_list->Get_H() + bex->Get_H() + 4*VBORDER + 2*BORDER; 
	          }
	}
 }
aligline = round(h/2.0);
}

void Par::SetFather(Tree_Node *f)
{
father = f;
gate_decl_list->SetFather(this);
bex->SetFather(this);
oper->SetFather(this);
}

void Par::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
gate_decl_list->SetCollapsed(t);
bex->SetCollapsed(t);
oper->SetCollapsed(t);
}


void Par::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
gate_decl_list->SetTextual(t);
bex->SetTextual(t);
oper->SetTextual(t);
}

/*								*/
/*	implementation of class Sort_Id_Exit methods		*/
/*								*/

Sort_Id_Exit::Sort_Id_Exit(ID_Place *s)
{
type = SORTIDEXIT;
textual = TRUE;
selectable = FALSE;
sort_id = s;
visible = TRUE;

Set_Unparser(new Gl_Sort_Id_Exit(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Sort_Id_Exit::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Sort_Id_Exit" << x << " " << y << "\n";
 cout << "Sort_Id_Exit" << h << " " << w << "\n";
#endif
x1 = x + Tree_Node::inUseFont()->Width("any ");
sort_id->SetPosition(x1,y);
}

void Sort_Id_Exit::SetDimensions(void)
{
sort_id->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = sort_id->Get_W() + Tree_Node::inUseFont()->Width("any ");
 }
aligline = round(h/2.0);
}

void Sort_Id_Exit::SetFather(Tree_Node *f)
{
father = f;
sort_id->SetFather(this);
}

void Sort_Id_Exit::SetCollapsed(char t)
{
collapsed = t;
sort_id->SetCollapsed(t);
}


/*                                                              */
/*      implementation of class Equality methods          	*/
/*                                                              */

Equality::Equality()
{
type = EQUALITY;
textual = TRUE;
selectable = FALSE;
visible = FALSE;
express1 = NULL;
express2 = NULL;

Set_Unparser(new Gl_Equality(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

Equality::Equality(Tree_Node *ex1, Tree_Node *ex2)
{
type = EQUALITY;
textual = TRUE;
visible = TRUE;
selectable = FALSE;
express1 = ex1;
express2 = ex2;

Set_Unparser(new Gl_Equality(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Equality::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Equality" << x << " " << y << "\n";
 cout << "Equality" << h << " " << w << "\n";
#endif
if(visible)
  {
  express1->SetPosition(x, y);
  x1 = x + express1->Get_W() + Tree_Node::inUseFont()->Width(" = ");
  express2->SetPosition(x1, y);
  }
}

void Equality::SetDimensions(void)
{
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
     if(visible)
	{
	express1->SetDimensions();
	express2->SetDimensions();
	h = express1->Get_H();
	w = express1->Get_W() + express2->Get_W() + Tree_Node::inUseFont()->Width(" = ");
	}
      else { h = 0; w = 0; }
 }
aligline = round(h/2.0);
}

void Equality::SetFather(Tree_Node *f)
{
father = f;
if(visible)
   {
   express1->SetFather(this);
   express2->SetFather(this);
   }
}

void Equality::SetCollapsed(char t)
{
collapsed = t;
express1->SetCollapsed(t);
express2->SetCollapsed(t);
}



/*                                                              */
/*      implementation of class Guarded methods          	*/
/*                                                              */


Guarded::Guarded(Equality *eq, Tree_Node *b)
{
type = GUARDED;
visible = TRUE;
alignement = VER;
equality = eq;
bex = b;

Set_Unparser(new Gl_Guarded(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Guarded::SetPosition(Coord xx, Coord yy)
{
Coord x1, y1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Guarded" << x << " " << y << "\n";
 cout << "Guarded" << h << " " << w << "\n";
#endif
if (textual)
	{
	x1 = x + Tree_Node::inUseFont()->Width("[") + border;
	equality->SetPosition(x1, y - border);
	Xclose = x1 + equality->Get_W();	/*position of ] -> */
	y1 = y - equality->Get_H() - VBORDER - border;
	x1 = x + SMALL_BORDER + border;
	bex->SetPosition(x1, y1);
	}
     else {	
	y1 = y - VBORDER;
	x1 = x + round((w - equality->Get_W())/2.0);
	equality->SetPosition(x1, y1);
	y1 = y1 - equality->Get_H() -  VBORDER - BORDER;
	x1 = x + round((w - bex->Get_W())/2.0);
	bex->SetPosition(x1, y1);
	}
}

void Guarded::SetDimensions(void)
{
equality->SetDimensions();
bex->SetDimensions();
border = 0;

switch(collapsed)
 {
 case COLLAPSED_VIS:	if(textual)
			   {
			   w = Tree_Node::inUseFont()->Width("<Bex>");
			   h = Tree_Node::inUseFont()->Height();
	       		   if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	       		      {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
			   }
   			 else {
			      w = round((Tree_Node::inUseFont()->Width("Bex"))*20.0/9.0);
			      h = round((Tree_Node::inUseFont()->Width("Bex"))*14.0/9.0);
			      }
 			break; 
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	
     if(textual)
	{
	h = equality->Get_H() + bex->Get_H() + VBORDER;

	w = Max(equality->Get_W() + Tree_Node::inUseFont()->Width("[") +
	             Tree_Node::inUseFont()->Width("] -> "), bex->Get_W());
	if(!(father->GetTextual()) && (father->GetType() <= PARALLEL))
 	  {border = VBORDER; w += 2 * VBORDER; h += 2 * VBORDER; }
	}
      else {
	   h = equality->Get_H() + bex->Get_H() + 2 * VBORDER + 2 * BORDER;
   	   w = Max(equality->Get_W(), bex->Get_W() + 2 * BORDER);
	   w +=  2 * BORDER;
	   }
 }
aligline = round(h/2.0);
}

void Guarded::SetFather(Tree_Node *f)
{
father = f;
equality->SetFather(this);
bex->SetFather(this);
}

void Guarded::SetCollapsed(char t)
{
collapsed = t;
if(t)
  t = COLLAPSED_INV;
equality->SetCollapsed(t);
bex->SetCollapsed(t);
}


void Guarded::SetTextual(char t, char s)
{
textual = t;
selectable = (!t | s);
bex->SetTextual(t);
}

/*                                                              */
/*      implementation of class Exper_Off methods               */
/*                                                              */

Exper_Off::Exper_Off()
{
type = EXPER_OFF;
textual = TRUE;
visible = FALSE;
selectable = FALSE;
w = 0; h = 0;
aligline = 0;

Set_Unparser(new Gl_Exper_Off(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Exper_Off::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "ExperOffer" << x << " " << y << "\n";
 cout << "ExperOffer" << h << " " << w << "\n";
#endif
}

void Exper_Off::SetDimensions(void)
{ }

void Exper_Off::SetFather(Tree_Node *f)
{ father = f; }


/*                                                              */
/*      implementation of class Exper_Off_List methods          */
/*                                                              */

Exper_Off_List::Exper_Off_List(Exper_Off *el, Exper_Off_List *nxt)
{
type = EXPER_OFF_L;
textual = TRUE;
selectable = FALSE;
elem = el;
next = nxt;
visible = elem->GetVisible();

Set_Unparser(new Gl_Exper_Off_List(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());
}

void Exper_Off_List::SetPosition(Coord xx, Coord yy)
{
Coord x1;
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Exper_Off_List" << x << " " << y << "\n";
 cout << "Exper_Off_List" << h << " " << w << "\n";
#endif
if(elem != NULL)
 {
 elem->SetPosition(x,y);
 x1 = x + elem->Get_W() + Tree_Node::inUseFont()->Width(", ");
 if(next != NULL)
   next->SetPosition(x1,y);
 }
}

void Exper_Off_List::SetDimensions(void)
{
elem->SetDimensions();
if(next!=NULL)
   next->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = Tree_Node::inUseFont()->Height();
			w = (next == NULL || next->next == NULL)? elem->Get_W()
		      	     : elem->Get_W() + next->Get_W() + Tree_Node::inUseFont()->Width(", ");
 }
aligline = round(h/2.0);
}

void Exper_Off_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

void Exper_Off_List::SetCollapsed(char t)
{
collapsed = t;
if(elem != NULL)
  elem->SetCollapsed(t);
if(next != NULL)
  next->SetCollapsed(t);
}


/*                                                              */
/*      implementation of class Exclam methods               */
/*                                                              */


Exclam::Exclam(Tree_Node *v_e)
{
type = EXCLAM;
textual = TRUE;
selectable = FALSE;
value_exp = v_e;
visible = TRUE;

Set_Unparser(new Gl_Exclam(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void Exclam::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Exclam" << x << " " << y << "\n";
 cout << "Exclam" << h << " " << w << "\n";
#endif

value_exp->SetPosition(x + Tree_Node::inUseFont()->Width("!"),y);
}

void Exclam::SetDimensions(void)
{
value_exp->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = value_exp->Get_H();
			w = value_exp->Get_W() + Tree_Node::inUseFont()->Width("!");
 }
aligline = round(h/2.0);
}

void Exclam::SetFather(Tree_Node *f)
{
father = f;
value_exp->SetFather(this);
}

void Exclam::SetCollapsed(char t)
{
collapsed = t;
value_exp->SetCollapsed(t);
}


/*                                                              */
/*      implementation of class Query methods               */
/*                                                              */


Query::Query(Id_Decl *i_d)
{
type = QUERY;
textual = TRUE;
selectable = FALSE;
id_decl = i_d;
visible = TRUE;

Set_Unparser(new Gl_Query(this));
((glow->Get_Folder())->Current())->Add(Get_Unparser());

}

void Query::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Query" << x << " " << y << "\n";
 cout << "Query" << h << " " << w << "\n";
#endif

id_decl->SetPosition(x + Tree_Node::inUseFont()->Width("?"),y );
}

void Query::SetDimensions(void)
{
id_decl->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = id_decl->Get_H();
			w = id_decl->Get_W() + Tree_Node::inUseFont()->Width("?");
 }
aligline = round(h/2.0);
}

void Query::SetFather(Tree_Node *f)
{
father = f;
id_decl->SetFather(this);
}

void Query::SetCollapsed(char t)
{
collapsed = t;
id_decl->SetCollapsed(t);
}


/*                                                              */
/*      implementation of class Definition                      */
/*                                                              */

Definition::Definition(Tree_Node *b, Proc_List *p, Data_List *d)
{
selectable = FALSE;
bex = b;
process_list = p;
data_list = d;

type = DEFINITION;
visible = TRUE;

}

void Definition::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "Definition" << x << " " << y << "\n";
 cout << "Definition" << h << " " << w << "\n";
#endif
bex->SetPosition(x, y);
}

void Definition::SetDimensions(void)
{
bex->SetDimensions();
switch(collapsed)
 {
 case COLLAPSED_VIS:
 case COLLAPSED_INV:	w = 0; h = 0;
			break;
 case NOCOLLAPSED: 	h = bex->Get_H();
			w = bex->Get_W();
 }
aligline = round(h/2.0);
}

void Definition::SetFather(Tree_Node *f)
{
father = f;
bex->SetFather(this);
if(process_list != NULL)
   process_list->SetFather(this);
if(data_list != NULL)
   data_list->SetFather(this);
}


void Definition::SetPath(char *p, char n, int& np, int& nd)
{
strcpy(path,p);
if(process_list != NULL)
  process_list->SetPath(path, n, np, nd);
if(data_list != NULL)
  data_list->SetPath(path, n, np, nd);
}

/*								*/
/*	implementation of class Proc_List methods		*/
/*								*/

Proc_List::Proc_List(Process *el, Proc_List *nxt)
{
type = PROCLIST;
selectable = FALSE;
elem = el;
next = nxt;
visible = TRUE;
}

void Proc_List::SetPosition(Coord xx, Coord yy)
{
x = xx;
y = yy;
#ifdef DEBUG
 cout << "ProcList" << x << " " << y << "\n";
 cout << "ProcList" << h << " " << w << "\n";
#endif
}

void Proc_List::SetDimensions(void)
{
h = 0; w = 0; 
aligline = round(h/2.0);
}

void Proc_List::SetFather(Tree_Node *f)
{
father = f;
if(elem != NULL)
  elem->SetFather(this);
if(next != NULL)
  next->SetFather(this);
}

void Proc_List::SetPath(char *p, char n, int& np, int& nd)
{
if(elem != NULL)
  elem->SetPath(p, n, np, nd);
if(next != NULL)
  next->SetPath(p, n, np, nd);
}

char *Proc_List::GetPath()
{
if(father->GetType() == PROCLIST)
  return(((Proc_List *)father)->GetPath());

if(father->GetType() == DEFINITION)
  return(((Definition *)father)->GetPath());
 else return(" ");
}
