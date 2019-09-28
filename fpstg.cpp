/*
 * fpstg.c
 *
 *  Created on: Sept 2, 2019
 *      Author: Mike
 */
#include <iostream>
#include <fstream>
#include <list>
#include <map>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
#include <stddef.h> // official home of ptrdiff_t
#include <assert.h>
#include "Token.h"
#include "Expr.h"
#include "util.h"

using namespace std;



struct Definition {
    string name;
    list<string> args;
    Expr* body;
};

Expr *parse_expr();

void pprint_expr(int col, const Expr *e);
list<ExprLet::Binding> parse_let_exprs();
Expr* parse_let();
list<string> parse_names(void) {
	list<string> names;
	while (token.type == T_NAME) {
		names.push_back(token.s);
		next();
	}
	return names;
}
Definition parse_def(void) {
	Definition r;

	if (token.type != T_NAME)
		return r;
	r.name = token.s;

	next();

	r.args = parse_names();

	assert(token.type == T_EQUALS);
	next();

	if (token.type == T_LET)
		r.body = parse_let();
	else
		r.body = parse_expr();

	assert(token.type == T_SEMI || token.type == T_EOF);
	next();


	return r;
}

list<Definition> parse_defs()
{
	list<Definition> defs;

	while (token.type != T_EOF) {
		defs.push_back(parse_def());
	}
	return defs;
}

void pprint_def(int col, const Definition& def)
{
	cout << def.name;
	if (def.args.size())
		cout << ' ' << join(def.args, ' ');
	cout << " :=" << endl;
    indent(col+2);
    cout << def.body->to_string(col+2) << endl;
}

void pprint_defs(int col, const list<Definition>& defs)
{
	for (auto def : defs)
        pprint_def(col, def);
}

/*
<O,ADD:C,S,i2:iI:V,G,D>=z-<0,C,S,(iI+i2):V,G,D>
<O,ALLOC:C,S,V,G,D> = <O,C,n:S,V,G+(n=HOLE),D>
<O,CHR:C,S,i:V,G,D> = <O,C,S,c:V,GP>
<O,CONS:C,n1:n2:S,V,G,D> = <O,C,n:S,V,G+(n=CONS nl n2)P>
<O,DIV:C,S,i1:i2:V,G,D> = <O,C,S,(il div i2):V,G,D>
<O,EQ:C ,S ,i2:i l:V ,G P > =z ~0 ,C ,S ,(i 1=i2):V ,G P >
<O ,GE:C,S,i&,:V,GP> + <O,C,S,(i,>i,):V,GP>
<O,GET:C,n:S,V,G[n=INTi]P>+<O,C,S,i:V,GP> similarly for nodes BOOL b and CHAR c
<O,GLOBSTARTf a:C,S,V,GP> * cO,C,S,V,GP>
<0,GT:C,S,i2:il:V,GP>* <O,C,S,(il>i$:V.G,D>
<O,HD:C,n:S,V,G[n=CONS nl nJP>* cO,C,nl:S,V,GP>
<O,JFALSE I:C,S,true:V,GP>+cO,C,S,V,G,D>
<O,JFALSE I:C,S,false:V,G,D>* c0,JM.P l:C,S,V,GP>
<O,JMPl: . * + :LABEL 1 :C ,S ,V,G ,D > + <O ,C ,S ,V,G P> backward jumps can also occur
<O,LABEL 1 :C,S,V,G P> =s CO ,C,S,V,G P>
<O,LE:C,S,i2:il:V,GP> + <O,C,S,(iIli&V,GP>
<O,LT:C ,S ,i,:i l:V,G ,D > + <O ,C ,S ,(i I<i2):V,G P>
<O,MKAP:C,n1:n2:S,V,GP>+ <O,C,n:S,V,G+(n=APnl n2],D>
<O,MKBOOL:C ,S ,b :V ,G P > * ~0 ,C ,n :S ,V,G+(n=BOOL b 1 P >
<O,MKCHAR:C,S,C:V,GP>*~O,C,~:S,V,G+(~=CHAJ~~~P>
<O,MKINT:C,S,i:V,GP> * cO,C,n:S,V,G+(n=INT i),D>
<O,MOD:C,S,i2:il:V,GP> + cO,C,S,(i1 mod iJ:V,G P>
<O,MUL:C,S,i2:il:V,GP~ += <0,C,S,(iIxi2):V,GP>
<O,NE:C ,S ,iz:i l:V,G P> * ~0 ,C ,S ,(il#i2):V,G P >
<O,NEG:C ,S ,i :V,G P > =s CO ,C ,S ,(-i):V ,G P >
<O,NOT:C ,S ,b :V ,G P > + ~0 ,C ,S ,(not b):V,G P >
<O,NULL:C,n:S,V,G[n=CONS nl nJP:>+<O,C,Sfalse:V,GP>
<O,NULL:C,n:S,V,G[n=NIL]D> * cO,C,S,true:V,GP>
<O,ORD:C,S,c:V,GP>+ <O,C,S,i:V,GP>
<O,POP m:C,nl: * + * :n,:S,V,GP>+csO,C,S,V,GP>
<O,PUSH m:C,no: . * * :n,:S,V,GP>+ <O,C,n,:na: . . . :n,:S,V,GP>
<O,PUSHBASIC v:C,S,V,GP> * <O,C.,S,v:V,GP>
<O,PUSHBOOL b:C,S,V,GP> =s <O,C,n:S,V,G+(n=BOOL b),D>
<O,PUSHCHAR c:C,S,V,GP> +- <O,C,n:S,V,G+(n=CHAR c)P>
<O,PUSHGLOBAL n:C,S,V,G[n=N]P> 3 <O,C,n:S,V,GP>
<O,PUSHINTi:C,S,V,G,D>~cO,C,n:S,V,G+(n=INTi),D>
<O,PUSHNIL:C,S,V,G,D>+=~O,C,n:S,1~,G+(n=NIL),D>
<O,RETURNtC ,n :[],V,G ,(C’,S’):D > * 4 ,C’ ,n :S’ ,V,G P >
<O,SCALL m:C,nl: . . . :n,:S,V,GP> +
<O,SUNWIND:U,nl: . . . :n,:[l,G,(C,S):D>
<O,SEVAL:UPDATE 1:C ,n :S ,V ,G [n =INT i ]P > =s <O ,C ,n :S ,V ,G P > similarly for nodes BOOL b, CHAR c , CONS n 1 n2, NIL, and FUN u g
<O,SEVAL:UPDATE 1:C f :S,V,G Cf=CAF g]P> a <O ,g f :[I,V,G,(C,S):D>
<O ,SEVAL:UPDATE 1 :C ,n :S ,V ,G [n =AP n 1 n21 P > =s <0,SUNWIND:[],nI:n2:[],V,G,(C,S):D>
<O ,SEVAL:UPDATE 1:C ,n :S ,V ,G [n =SAl? n 1 n2] P > ea <0,SUNWIND:[],n,:n2:[],V,G,(UPDATE l:C,n:S):D>
<O ,SLIDE m :C ,nO: . . . :n,:S,V,GP>+ cO,C,no:S,V,GP>
<0,SMKAP:C,nI:n2:S,V,GP>+ <O,C,n:S,V,G+(n=SAPnl nz)P>
<O,SPRINT:C,n:S,V,G[n=INT il,D> + <:O;i,C,S,V,G P> similarly for nodes BOOL b and CHAR c
<O,SPRINT:C,n:S,V,G[n =CONS n 1 n dP > a <O ,SEVAL:UPDATE 1:SPRINT:SEVAL:UPDATE 1:SPRINT:C ,nl:n2:S ,V,G P>
<O,SPRINT:C,n:S,V,G [n=NIL]P> 3 <G’,C,S,V,G P>
<O,SQUEEZE k d:C,nl: . * . :nk:pl: . . . :pd:S,V,G,D>+ <O,C,nl: . . . :nk:S,V,GP>
<O,STOP:C,S ,V,G P > = <O ,C ,S ,V,G P> and the machine stops
<0,SUB:C,S,i2:i1:V,GP> + <O,C,S,(il--i2):V,GP>
<O,SUNWIND:[l,n:S,V,G[n=INT i],(C’,S’):D> * <O,C’,n:S’,V,G P> similarly for nodes BOOL b, CHAR c 1 CONS n 1 n2, and NIL
<O,SUNWIND:~f:S,V,G~=CAFgl,D> a <O,gf:S,V,GP>
<O,SUNWIND:O,n:S,V,G [n=AP nl n2],D> a
<O,SUNWIND:[I,n~:n~:S,V,G P>
<O,SUNWIND:O,n:S,V,G[n=SAP nl n2]P> =s
<O,SCALL 2:UPDATE 1:SUNWIND:[l,nI:n2:n:S,V,G P>
<O ,SUNWIND:[lf xl: . * . :n,:S,V,G[f=:FUNa g]p>+cO,g,n1: . . - :n,:S,V,G,D>
<O ,SUNWIND: of :n 1: - - . :n ,:[],V,G Cf:=FUN a g]P> and mea ea <O ,MKAP: . . . :MKAP:RETURN:[]f:nl: * * . :n,:[],V,G P> - m times -
<O,TL:C,n:S,V,Gln=CONS nl ndP> + <0,C,n2:S,V,GP>
<O,UPDATE m:C,nO,nl: . . . :n,:S ,V,G [n~Ng,n,=N,lP> * <O,C,nl: . . . :n,:S ,V,G [no=No,n,=No]P>
*/

enum InstructionType {
	ADD,
	ALLOC,
	CHR,
	CONS,
	DIV,
	EQ,
	GE,
	GET,
	GLOBSTART,
	GT,
	HD,
	JFALSE,
	JMP,
	LABEL,
	LE,
	LT,
	MKAP,
	MKBOOL,
	MKCHAR,
	MKINT,
	MOD,
	MUL,
	NE,
	NEG,
	NOT,
	NULLinst,
	ORD,
	POP,
	PUSH,
	PUSHBASIC,
	PUSHBOOL,
	PUSHCHAR,
	PUSHGLOBAL,
	PUSHINT,
	PUSHNIL,
	RETURN,
	SCALL,
	SEVAL,
	SLIDE,
	SMKAP,
	SPRINT,
	SQUEEZE,
	STOP,
	SUB,
	SUNWIND,
	TL,
	UPDATE,
	zzzmaxInstr
};
const char* insNames[] = {
		"ADD",
		"ALLOC",
		"CHR",
		"CONS",
		"DIV",
		"EQ",
		"GE",
		"GET",
		"GLOBSTART",
		"GT",
		"HD",
		"JFALSE",
		"JMP",
		"LABEL",
		"LE",
		"LT",
		"MKAP",
		"MKBOOL",
		"MKCHAR",
		"MKINT",
		"MOD",
		"MUL",
		"NE",
		"NEG",
		"NOT",
		"NULLinst",
		"ORD",
		"POP",
		"PUSH",
		"PUSHBASIC",
		"PUSHBOOL",
		"PUSHCHAR",
		"PUSHGLOBAL",
		"PUSHINT",
		"PUSHNIL",
		"RETURN",
		"SCALL",
		"SEVAL",
		"SLIDE",
		"SMKAP",
		"SPRINT",
		"SQUEEZE",
		"STOP",
		"SUB",
		"SUNWIND",
		"TL",
		"UPDATE"
};
const char* insToString(InstructionType ins) {
	if (0<=ins && ins<zzzmaxInstr)
		return insNames[ins];
	return "inxxx";
}
typedef enum AddressModeMode {
	Arg, Label, Super, Num, List, Marker
} AddressModeMode;
const char* amToString(AddressModeMode am) {
	switch (am) {
	case Arg: return "Arg";
	case Label: return "Label";
	case Super: return "Super";
	case Num: return "Num";
	case List: return "List";
	case Marker: return "Marker";
	}
	return "amxxx";
}
#if 0
typedef enum OpType {
	Add, Sub, Mul, Div, Mod, Lt
} OpType;
const char* opToString(OpType op) {
	switch (op) {
	case Add: return "Add";
	case Sub: return "Sub";
	case Mul: return "Mul";
	case Div: return "Div";
	case Mod: return "Mod";
	case Lt: return "Lt";
	}
	return "opxxx";
}
#endif
struct CodeArray;
struct AddressMode{
	AddressModeMode mode;
	union {
		ptrdiff_t address;
		//CodeArray* code;
		unsigned arg;
	} params;
};
struct Instruction {
	InstructionType ins;
	union {
		unsigned n;
		int i;
		bool b;
	};
	ptrdiff_t dest;
};
string instructionToString(Instruction* ins) {
	string rv = insToString(ins->ins);
	switch (ins->ins) {
	case GLOBSTART: rv += " " + ::to_string(ins->dest)+" "+::to_string(ins->n); break;
	case JFALSE:
	case JMP:
	case PUSHGLOBAL: rv += " " + ::to_string(ins->dest); break;
	case PUSHINT: rv += " " + ::to_string(ins->i); break;
	case PUSHCHAR: rv += " " + ::to_string(ins->i); break;
	case PUSHBOOL: rv += " " + ::to_string(ins->i); break;
	case POP:
	case SLIDE:
	case UPDATE: rv += " " + ::to_string(ins->n); break;
	default: break;
	}
	return rv;
}
struct CodeArray {
	vector<Instruction> code;
	void add(const Instruction& i) {
		code.push_back(i);
	}
};
#if 0
static
void append(CodeArray& code, CodeArray* target) {
	// Amenable to an obvious optimization in the future.
	for (unsigned i=0; i<target->code.size(); ++i) {
		addInstruction(code, target->code[i]);
	}
}
#endif
struct EnvItem {
	int args;
	AddressMode mode;
};
typedef map<string,EnvItem> Env;
static
Env envAddArgs(Env env, const list<string> args) {
	int kArg = 0;
	for (auto arg : args) {
        EnvItem entry;
        //entry.name = arg;
        entry.mode.mode = Label;
        entry.mode.params.address = 2*kArg;
        kArg++;
        env[arg] = entry;
	}
    //printf("envAddArgs "); pprint_env(env);
	return env;
}
static
void envAddVar(Env& env, const string& name, AddressMode mode) {
	EnvItem entry;
	entry.mode = mode;
	env[name] = entry;
}
static
Env envDup(const Env& env) {
	Env dup ;
	for (auto envItem: env) {
		EnvItem d = envItem.second;
		dup[envItem.first] = d;
	}
    //printf("envDup "); pprint_env(dup);
	return dup;
}
static
bool find_mode(const Env& env, const string& var, AddressMode* mode) {
	auto p = env.find(var);
	if (p != env.end()) {
		*mode = p->second.mode;
		return true;
	}
	return false;
}

typedef struct {
	CodeArray* code;
	int d;
} code_regs_t;
typedef struct {
	string name;
	ptrdiff_t ref;
} forward_t;
list<forward_t> forwards;
void adjustForwardReferences(CodeArray* code, ptrdiff_t newStart) {
	for (auto forward : forwards) {
		if (forward.ref < (ptrdiff_t)code->code.size()) {
			forward.ref += newStart;
		}
	}
}
//void pprint_expr(int col, const Expr *e);
static
unsigned countLocalsExp(const Expr* exp)
{
	auto app = dynamic_cast<const ExprApp*>(exp);
	if (app)
		return 0;// FIXME compileRApp(code, d, exp->app, env);
	auto num = dynamic_cast<const ExprNum*>(exp);
	if (num)
		return 0;
	auto var = dynamic_cast<const ExprVar*>(exp);
	if (var)
		return 0;
	auto str = dynamic_cast<const ExprStr*>(exp);
	if (str)
		return 0;
	auto sub = dynamic_cast<const ExprSub*>(exp);
	if (sub)
		return 0;
	auto mul = dynamic_cast<const ExprMul*>(exp);
	if (mul)
		return 0;
	auto add = dynamic_cast<const ExprAdd*>(exp);
	if (add)
		return 0;
#if 0
	case EXPR_LET: {
		unsigned vars = length(exp->letx.let_defs) + countLocalsExp(exp->letx.let_value);
		return vars;
	}
#endif
	cout << exp->to_string(0) << " Locals unknown " << endl;
	return 9999;
}
static
unsigned countLocals(const Definition& def)
{
	return countLocalsExp(def.body);
}
Instruction Globstart(ptrdiff_t dest, int args) {
	Instruction i;
	i.ins = GLOBSTART;
	i.n = args;
	i.dest = dest;
	return i;
}
Instruction SUnwind() {
	Instruction i;
	i.ins = SUNWIND;
	return i;
}
Instruction Update(int n) {
	Instruction i;
	i.ins = UPDATE;
	i.n = n;
	return i;
}
Instruction SqueezeInstruction(int, int) {
	Instruction i;
	i.ins = SQUEEZE;
	return i;
}
Instruction ReturnInstruction() {
	Instruction i;
	i.ins = RETURN;
	return i;
}
Instruction JFalseInstruction(unsigned dest) {
	Instruction i;
	i.ins = JFALSE;
	i.dest = dest;
	return i;
}
Instruction JumpInstruction(unsigned dest) {
	Instruction i;
	i.ins = JMP;
	i.dest = dest;
	return i;
}
Instruction PushIntInstruction(int val) {
	Instruction i;
	i.ins = PUSHINT;
	i.i = val;
	return i;
}
Instruction PushBoolInstruction(bool val) {
	Instruction i;
	i.ins = PUSHBOOL;
	i.b = val;
	return i;
}
Instruction PushCharInstruction(int ch) {
	Instruction i;
	i.ins = PUSHCHAR;
	i.i = ch;
	return i;
}
Instruction PushNilInstruction() {
	Instruction i;
	i.ins = PUSHNIL;
	return i;
}
Instruction PushGlobalInstruction(unsigned dest) {
	Instruction i;
	i.ins = PUSHGLOBAL;
	i.dest = dest;
	return i;
}
Instruction PushInstruction(int d) {
	Instruction i;
	i.ins = PUSH;
	i.n = d;
	return i;
}
Instruction PushBasic(int d) {
	Instruction i;
	i.ins = PUSHBASIC;
	i.n = d;
	return i;
}
Instruction LtInstruction() {
	Instruction i;
	i.ins = LT;
	return i;
}
Instruction MulInstruction() {
	Instruction i;
	i.ins = MUL;
	return i;
}
Instruction GetInstruction() {
	Instruction i;
	i.ins = GET;
	return i;
}
Instruction SEvalInstruction() {
	Instruction i;
	i.ins = GET;
	return i;
}
Instruction MkintInstruction() {
	Instruction i;
	i.ins = MKINT;
	return i;
}
Instruction MkapInstruction() {
	Instruction i;
	i.ins = MKAP;
	return i;
}
Instruction SMkapInstruction() {
	Instruction i;
	i.ins = SMKAP;
	return i;
}
Instruction PopInstruction(int n) {
	Instruction i;
	i.ins = POP;
	i.n = n;
	return i;
}
Instruction SlideInstruction(int n) {
	Instruction i;
	i.ins = SLIDE;
	i.n = n;
	return i;
}
static
void bScheme(CodeArray& code, Expr* e, const Env& r, int n);
static
void eScheme(CodeArray& code, Expr* e, const Env& r, int n);
static
void rScheme(CodeArray& code, Expr* e, const Env& r, int n);
static
void cScheme(CodeArray& code, Expr* e, const Env& r, int n);
struct ESchemeVisitor : public ExprVisitor {
	ESchemeVisitor(CodeArray& code, const Env& r, int& n)
	: code(code), r(r), n(n)
	{}
	virtual void visitExprVar(ExprVar* v) {
		AddressMode m;
		auto p = r.find(v->var);
		if (p != r.end()) {
			m = p->second.mode;
			if (m.mode == Arg) {
				code.add(PushInstruction(n-m.params.arg));
			} else {
				code.add(PushGlobalInstruction(m.params.address));
			}
			code.add(SEvalInstruction());
			code.add(Update(1));
		} else {
			cout << __PRETTY_FUNCTION__ << " Can't find " << v->to_string(0) << endl;
		}
		return;
		cout << __PRETTY_FUNCTION__ << " to come" << endl;
	}
	virtual void visitExprNum(ExprNum* n) {
		code.add(PushIntInstruction(n->value));
	}
	virtual void visitExprChar(ExprChar* n) {
		code.add(PushCharInstruction(n->ch));
	}
	virtual void visitExprApp(ExprApp* e) {
		for (auto p = e->args.rbegin(); p!=e->args.rend(); p++) {
			auto arg = *p;
			cout << __PRETTY_FUNCTION__ << " Arg: " << arg->to_string(0) << endl;
			cScheme(code, *p, r, n);
		}
		auto f = dynamic_cast<ExprVar*>(e->fun);
		auto pm = r.find(f->var);
		if (pm == r.end()) {
			cout << __PRETTY_FUNCTION__ << ": no destination for " << f->var << endl;
		} else {
			cout << __PRETTY_FUNCTION__ << ": destination for " << f->var << " to come" << endl;
		}
	}
	virtual void visitExprStr(ExprStr*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNil(ExprNil*) {
		code.add(PushNilInstruction());
	}
	virtual void visitExprBool(ExprBool* b) {
		code.add(PushBoolInstruction(b->value));
	}
	virtual void visitExprIf(ExprIf* cond) {
		bScheme(code, cond->cond, r, n);
		auto fix1 = code.code.size();
		code.add(JFalseInstruction(0));
		eScheme(code, cond->trueExpr, r, n);
		code.add(JumpInstruction(0));
		auto fix2 = code.code.size();
		// fix1 fix jump TODO FIXME
		eScheme(code, cond->falseExpr, r, n);
		// fix2 fix jump TODO FIXME
	}
	virtual void visitExprCons(ExprCons*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprHd(ExprHd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprTl(ExprTl*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNull(ExprNull*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLet(ExprLet* e) {
		cScheme(code, e->bindings.front().value, r, n);
		eScheme(code, e->value, r/*modify FIXME*/, n+1);
		code.add(SlideInstruction(1));
	}
	virtual void visitExprAdd(ExprAdd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprSub(ExprSub* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprMul(ExprMul* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprDiv(ExprDiv* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprMod(ExprMod* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprEq(ExprEq* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprNe(ExprNe* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprLt(ExprLt* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprGt(ExprGt* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprLe(ExprLe* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprGe(ExprGe* m) {
		bScheme(code, m, r, n);
		code.add(MkintInstruction());
	}
	virtual void visitExprNeg(ExprNeg*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNot(ExprNot*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	CodeArray& code;
	const Env& r;
	int& n;
};
struct BSchemeVisitor : public ExprVisitor {
	BSchemeVisitor(CodeArray& code, const Env& r, int& n)
	: code(code), r(r), n(n)
	{}
	virtual void visitExprVar(ExprVar* e) {
		eScheme(code, e, r, n);
		code.add(GetInstruction());
	}
	virtual void visitExprNum(ExprNum* e) {
		code.add(PushBasic(e->value));
	}
	virtual void visitExprApp(ExprApp* e) {
		eScheme(code, e, r, n);
		code.add(GetInstruction());
	}
	virtual void visitExprStr(ExprStr*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNil(ExprNil*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprBool(ExprBool*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprChar(ExprChar*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprIf(ExprIf*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprCons(ExprCons*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprHd(ExprHd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprTl(ExprTl*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNull(ExprNull*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLet(ExprLet* e) {
		cScheme(code, e->bindings.front().value, r, n);
		bScheme(code, e->value, r/*modify FIXME*/, n+1);
		code.add(PopInstruction(1));
	}
	virtual void visitExprAdd(ExprAdd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprSub(ExprSub*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprMul(ExprMul* e) {
		bScheme(code, e->left, r, n);
		bScheme(code, e->right, r, n);
		code.add(MulInstruction());
		return;
		cout << __PRETTY_FUNCTION__ << " to come" << endl;
	}
	virtual void visitExprDiv(ExprDiv*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprMod(ExprMod*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprEq(ExprEq*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNe(ExprNe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLt(ExprLt* e) {
		bScheme(code, e->left, r, n);
		bScheme(code, e->right, r, n);
		code.add(LtInstruction());
	}
	virtual void visitExprGt(ExprGt*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLe(ExprLe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprGe(ExprGe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNeg(ExprNeg*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNot(ExprNot*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	CodeArray& code;
	const Env& r;
	int& n;
};
struct RSchemeVisitor : public ExprVisitor {
	RSchemeVisitor(CodeArray& code, const Env& r, int& n)
	: code(code), r(r), n(n)
	{}
	virtual void visitExprVar(ExprVar* var) {
		eScheme(code, var, r, n);
		code.add(SqueezeInstruction(1, n));
		code.add(ReturnInstruction());
	}
	virtual void visitExprNum(ExprNum* num) {
		eScheme(code, num, r, n);
		code.add(SqueezeInstruction(1, n));
		code.add(ReturnInstruction());
	}
	virtual void visitExprBool(ExprBool* b) {
		eScheme(code, b, r, n);
		code.add(SqueezeInstruction(1, n));
		code.add(ReturnInstruction());
	}
	virtual void visitExprChar(ExprChar* c) {
		eScheme(code, c, r, n);
		code.add(SqueezeInstruction(1, n));
		code.add(ReturnInstruction());
	}
	virtual void visitExprApp(ExprApp* e) {
		for (auto p = e->args.rbegin(); p!=e->args.rend(); p++) {
			cScheme(code, *p, r, n);
		}
		ExprVar* v = dynamic_cast<ExprVar*>(e->fun);
		auto pf = r.find(v->var);
		if (pf != r.end()) {
			auto m = pf->second.mode;
			if (m.mode == Arg) {
				cout << __PRETTY_FUNCTION__ << "Dest Pushing " << v->to_string(0) << endl;
				code.add(PushInstruction(n-m.params.arg));
			} else {
				cout << __PRETTY_FUNCTION__ << "Dest Pushing " << v->to_string(0) << endl;
				code.add(PushGlobalInstruction(m.params.address));
			}
			code.add(SqueezeInstruction(e->args.size()+1,n));
			code.add(SUnwind());
		} else {
			cout << "Didn't find thing to call with "<<e->fun->to_string(0)<<" in " << __PRETTY_FUNCTION__ << endl;
		}
		return;
		cout << __PRETTY_FUNCTION__ << " to come" << endl;
	}
	virtual void visitExprStr(ExprStr*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNil(ExprNil* var) {
		eScheme(code, var, r, n);
		code.add(SqueezeInstruction(1, n));
		code.add(ReturnInstruction());
	}
	virtual void visitExprIf(ExprIf* cond) {
		bScheme(code, cond->cond, r, n);
		auto fix = code.code.size();
		code.add(JFalseInstruction(0));
		rScheme(code, cond->trueExpr, r, n);
		code.code[fix];// FIXME fix destination of jump
		rScheme(code, cond->falseExpr, r, n);
	}
	virtual void visitExprCons(ExprCons*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprHd(ExprHd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprTl(ExprTl*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNull(ExprNull*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLet(ExprLet* e) {
		cScheme(code, e->bindings.front().value, r, n);
		rScheme(code, e->value, r/*modify FIXME*/, n+1);
	}
	virtual void visitExprAdd(ExprAdd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprSub(ExprSub*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprMul(ExprMul*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprDiv(ExprDiv*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprMod(ExprMod*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprEq(ExprEq*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNe(ExprNe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLt(ExprLt*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprGt(ExprGt*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLe(ExprLe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprGe(ExprGe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNeg(ExprNeg*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNot(ExprNot*) {cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	CodeArray& code;
	const Env& r;
	int& n;
};
struct CSchemeVisitor : public ExprVisitor {
	CSchemeVisitor(CodeArray& code, const Env& r, int& n)
	: code(code), r(r), n(n)
	{}
	virtual void visitExprVar(ExprVar* e) {
		AddressMode m;
		auto p = r.find(e->var);
		if (p != r.end()) {
			m = p->second.mode;
			if (m.mode == Arg) {
				code.add(PushInstruction(n-m.params.arg));
			} else {
				code.add(PushGlobalInstruction(m.params.address));
			}
		} else {
			cout << __PRETTY_FUNCTION__ << " Can't find " << e->to_string(0) << endl;
		}
	}
	virtual void visitExprNum(ExprNum* e) {
		code.add(PushIntInstruction(e->value));
	}
	virtual void visitExprChar(ExprChar* e) {
		code.add(PushCharInstruction(e->ch));
	}
	virtual void visitExprBool(ExprBool* e) {
		code.add(PushBoolInstruction(e->value));
	}
	virtual void visitExprApp(ExprApp* e) {
		for (auto p = e->args.rbegin(); p!=e->args.rend(); p++) {

			cScheme(code, *p, r, n);
		}
		cScheme(code, e->fun, r, n);
		if (e->args.size()>1) {
			for (unsigned i=0; i<e->args.size()-2; i++)
				code.add(MkapInstruction());
		}
		code.add(SMkapInstruction());
	}
	virtual void visitExprStr(ExprStr*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNil(ExprNil*) {
		code.add(PushNilInstruction());
	}
	virtual void visitExprIf(ExprIf*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprCons(ExprCons*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprHd(ExprHd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprTl(ExprTl*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNull(ExprNull*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNot(ExprNot*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLet(ExprLet* e) {
		cScheme(code, e->bindings.front().value, r, n);
		cScheme(code, e->value, r/*modify FIXME*/, n+1);
		code.add(SlideInstruction(1));
	}
	virtual void visitExprAdd(ExprAdd*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprSub(ExprSub*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprMul(ExprMul* e) {
		cout << __PRETTY_FUNCTION__ << " Generating mul code? NO! " << e->to_string(0) << endl;
		throw "Code gen";
		bScheme(code, e->left, r, n);
		bScheme(code, e->right, r, n);
		code.add(MulInstruction());
	}
	virtual void visitExprDiv(ExprDiv*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprMod(ExprMod*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprEq(ExprEq*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNe(ExprNe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLt(ExprLt* e) {
		bScheme(code, e->left, r, n);
		bScheme(code, e->right, r, n);
		code.add(LtInstruction());
	}
	virtual void visitExprGt(ExprGt*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprLe(ExprLe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprGe(ExprGe*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	virtual void visitExprNeg(ExprNeg*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
	CodeArray& code;
	const Env& r;
	int& n;
};
static
void bScheme(CodeArray& code, Expr* e, const Env& r, int n) {
	BSchemeVisitor bScheme(code, r, n);
	e->visit(&bScheme);
}
static
void cScheme(CodeArray& code, Expr* e, const Env& r, int n) {
	CSchemeVisitor cScheme(code, r, n);
	e->visit(&cScheme);
}
static
void eScheme(CodeArray& code, Expr* e, const Env& r, int n) {
	ESchemeVisitor eScheme(code, r, n);
	e->visit(&eScheme);
}
static
void rScheme(CodeArray& code, Expr* e, const Env& r, int n) {
	RSchemeVisitor rScheme(code, r, n);
	e->visit(&rScheme);
}
static
void fScheme(CodeArray& code, const Definition& def, const Env& env) {
	if (def.args.size()) {
		code.add(Globstart(code.code.size(), def.args.size()));
		eScheme(code, def.body, env, 0);
		code.add(Update(1));
		code.add(SUnwind());
	} else {
		code.add(Globstart(code.code.size(), def.args.size()));
		rScheme(code, def.body, env, def.args.size());
	}
}
static
CodeArray& compileDef(CodeArray& code, const Definition& def, const Env& env) {
	//def->args;
	//def->name;
	//def->body; // Expr
	//unsigned nArgs = def.args.size();
	//unsigned nLocals = countLocals(def);
	Env new_env = envDup(env);
	new_env = envAddArgs(new_env, def.args);
	fScheme(code, def, new_env);
	return code;
}

struct ComputeNode {
	virtual ~ComputeNode() {}
};
struct APNode : ComputeNode {
	ComputeNode* fun;
	ComputeNode* arg;
};
struct IntNode : ComputeNode {
	int value;
};
struct CharNode : ComputeNode {
	int value;
};
struct BoolNode : ComputeNode {
	bool value;
};
struct ConsNode : ComputeNode {
	ComputeNode* head;
	ComputeNode* tail;
};
struct NilNode : ConsNode {
	NilNode() {
		head=nullptr; tail=nullptr;
	}
};
struct State {
	int pc;
};
State state;
char* pcToString(ptrdiff_t pc)
{
	static char text[50];
		sprintf(text, "%ld", pc);
	return text;
}
void step(const CodeArray& code) {
	//if (state.pc < 0)
	//	return; // We don't run SELF or MARKER yet.
	if (code.code[state.pc].ins == STOP)
		return;
	ptrdiff_t old_pc = state.pc;
	state.pc ++;

	switch (code.code[old_pc].ins) {
	default:
		break;
	}
}
struct Atomicness : public ExprVisitor {
	Atomicness() : r(true) {}
	void visitExprVar(ExprVar* e) {(void)e;}
	void visitExprNum(ExprNum* e) {(void)e;}
	void visitExprBool(ExprBool* e) {(void)e;}
	void visitExprChar(ExprChar* e) {(void)e;}
	void visitExprApp(ExprApp*) { r=false; }
	void visitExprStr(ExprStr* e) {(void)e;}
	void visitExprNil(ExprNil* e) { (void)e; r=false; }
	void visitExprIf(ExprIf* e) { (void)e; r=false; }
	void visitExprCons(ExprCons* e) { (void)e; r=false; }
	void visitExprHd(ExprHd* e) { (void)e; r=false; }
	void visitExprTl(ExprTl* e) { (void)e; r=false; }
	void visitExprNull(ExprNull* e) { (void)e; r=false; }
	void visitExprLet(ExprLet* e) { (void)e; }
	void visitExprAdd(ExprAdd* e) { (void)e; r=false; }
	void visitExprSub(ExprSub* e) { (void)e; r=false; }
	void visitExprMul(ExprMul* e) { (void)e; r=false; }
	void visitExprDiv(ExprDiv* e) { (void)e; r=false; }
	void visitExprMod(ExprMod* e) { (void)e; r=false; }
	void visitExprEq(ExprEq* e) { (void)e; r=false; }
	void visitExprNe(ExprNe* e) { (void)e; r=false; }
	void visitExprLt(ExprLt* e) { (void)e; r=false; }
	void visitExprGt(ExprGt* e) { (void)e; r=false; }
	void visitExprLe(ExprLe* e) { (void)e; r=false; }
	void visitExprGe(ExprGe* e) { (void)e; r=false; }
	void visitExprNeg(ExprNeg* e) { (void)e; r=false; }
	void visitExprNot(ExprNot* e) { (void)e; r=false; }
	bool r;
};
bool atomicExpr(Expr* e){
	Atomicness a;
	e->visit(&a);
	return a.r;
}
static int uniqueCount=0;
string makeFreshLabel() {
	return "UU"+::to_string(uniqueCount++);
}
struct LoweringVisitor : public ExprVisitor {
	LoweringVisitor(list<Definition>& defs, Env& env) : defs(defs), env(env) {}
	void visitExprVar(ExprVar* e) {(void)e;}
	void visitExprNum(ExprNum* e) {(void)e;}
	void visitExprBool(ExprBool* e) {(void)e;}
	void visitExprChar(ExprChar* e) {(void)e;}
	void visitExprApp(ExprApp* e) {
		//cout << __PRETTY_FUNCTION__ << " Checking " << e->to_string(0) << " for lowering needs" << endl;
		list<Expr*> newArgs;
		bool updated = false;
		for (auto pArg = e->args.begin(); pArg!=e->args.end(); ++pArg) {
			if (!atomicExpr(*pArg)) {
				auto nn = makeFreshLabel();
				auto na = new ExprVar(nn);
				auto nl = new ExprLet();
				ExprLet::Binding b;
				b.name = nn;
				b.value = *pArg;
				nl->bindings.push_back(b);
				nl->value = na;
				newArgs.push_back(nl);
				updated = true;
			} else {
				newArgs.push_back(*pArg);
			}
		}
		if (updated) {
			e->args = newArgs;
			cout << __PRETTY_FUNCTION__ << " Expression update: " << e->to_string(0) << endl;
		}
	}
	void visitExprStr(ExprStr* e) { (void)e;}
	void visitExprNil(ExprNil* e) {(void)e;}
	void visitExprIf(ExprIf* e) {
		//cout << __PRETTY_FUNCTION__ << " Checking " << e->to_string(0) << " for lowering needs" << endl;
		e->cond->visit(this);
		e->trueExpr->visit(this);
		e->falseExpr->visit(this);
	}
	void visitExprCons(ExprCons* e) {(void)e;}
	void visitExprHd(ExprHd* e) {(void)e;}
	void visitExprTl(ExprTl* e) {(void)e;}
	void visitExprNull(ExprNull* e) {(void)e;}
	void visitExprNot(ExprNot* e) { (void)e; }
	void visitExprLet(ExprLet* e) {
		cout << __PRETTY_FUNCTION__ << " Checking " << e->to_string(0) << " for lowering needs" << endl;
		for (auto binding : e->bindings) {
			binding.value->visit(this);
		}
		e->value->visit(this);
	}
	void visitExprAdd(ExprAdd* e) {(void)e;}
	void visitExprSub(ExprSub* e) {
		cout << __PRETTY_FUNCTION__ << " Checking " << e->to_string(0) << " for lowering needs" << endl;
		//e->left->visit(this);
		//e->right->visit(this);
		if (!atomicExpr(e->left)) {
			auto nn = makeFreshLabel();
			auto na = new ExprVar(nn);
			auto nl = new ExprLet();
			ExprLet::Binding b;
			b.name = nn;
			b.value = e->left;
			nl->bindings.push_back(b);
			nl->value = na;
			e->left = nl;
		}
		if (!atomicExpr(e->right)) {
			auto nn = makeFreshLabel();
			auto na = new ExprVar(nn);
			auto nl = new ExprLet();
			ExprLet::Binding b;
			b.name = nn;
			b.value = e->right;
			nl->bindings.push_back(b);
			nl->value = na;
			e->right = nl;
		}
	}
	void visitExprMul(ExprMul* e) {
		//cout << __PRETTY_FUNCTION__ << " Checking " << e->to_string(0) << " for lowering needs" << endl;
		//e->left->visit(this);
		//e->right->visit(this);
		bool update = false;
		if (!atomicExpr(e->left)) {
			auto nn = makeFreshLabel();
			auto na = new ExprVar(nn);
			auto nl = new ExprLet();
			ExprLet::Binding b;
			b.name = nn;
			b.value = e->left;
			nl->bindings.push_back(b);
			nl->value = na;
			e->left = nl;
			e->left->visit(this);
			update = true;
		}
		if (!atomicExpr(e->right)) {
			auto nn = makeFreshLabel();
			auto na = new ExprVar(nn);
			auto nl = new ExprLet();
			ExprLet::Binding b;
			b.name = nn;
			b.value = e->right;
			nl->bindings.push_back(b);
			nl->value = na;
			e->right = nl;
			e->right->visit(this);
			update =true;
		}
		if (update)
			cout << __PRETTY_FUNCTION__ << " Updated to " << e->to_string(0) << endl;
	}
	void visitExprDiv(ExprDiv* e) {(void)e;}
	void visitExprMod(ExprMod* e) {(void)e;}
	void visitExprEq(ExprEq* e) {(void)e;}
	void visitExprNe(ExprNe* e) {(void)e;}
	void visitExprLt(ExprLt* e) {(void)e;}
	void visitExprGt(ExprGt* e) {(void)e;}
	void visitExprLe(ExprLe* e) {(void)e;}
	void visitExprGe(ExprGe* e) {(void)e;}
	void visitExprNeg(ExprNeg* e) {(void)e;}
	list<Definition>& defs;
	Env& env;
};
static
void lowerDefs(list<Definition>& defs, Env& env) {
	LoweringVisitor lowering(defs, env);
	for (auto def: defs) {
		def.body->visit(&lowering);
	}
}
int main(int argc, char** argv)
{
    // Initialize input
	ifstream input;
    if (argc>1)
    {
    	streambuf* orig_cin;
    	if (strcmp(argv[1], "test")==0) {
    	    CodeArray code;
    	    return 0;
    	}
    	input.open(argv[1],ios::in);// = fopen(argv[1],"rt");
		if (!input)
			return -1;
		orig_cin = cin.rdbuf(input.rdbuf());
		cin.tie(0);
    }

    nextChar();
	next();
    auto defs = parse_defs();

    if(defs.size()==0) {
        fprintf(stderr, "no parse\n");
        return 1;
    }

    pprint_defs(0, defs);
    Env env;
    lowerDefs(defs, env);
    pprint_defs(0, defs);

    CodeArray code;
    for (auto def : defs) {
    	EnvItem item;
		item.args = def.args.size();
		item.mode.mode = Super;
		// If there are no arguments, the entry point should be
		// the first instruction we will emit.
		// If there are arguments, the entry point should be
		// the check instruction, which is after n-1 push-labels.
		//if (item.args == 0)
			item.mode.params.address = code.code.size();
		//else
		//	item.mode.params.address = code.code.size() + (item.args-1);
		env[def.name] = item;
		compileDef(code, def, env);
    }
    //fixForwardReferences(&code, env); // Go back and fix the forward references
    for (auto def: defs) {
        cout << def.name << " args " << def.args.size() << " locals " << countLocals(def) << endl;
    }
    for (auto entry : env) {
        ptrdiff_t addr = entry.second.mode.params.address;
        cout << entry.first << " = " << addr << endl;
    }
    //unsigned haltat = code.code.size();
    //haltInstruction(&code);
    Instruction instr; instr.ins = STOP;
    code.code.push_back(instr);
    for (unsigned i=0; i<code.code.size(); ++i) {
		cout << i <<": " << instructionToString(&code.code[i]) << endl;
    }
    AddressMode start_am;
    if (!find_mode(env, "main", &start_am))
    {
		puts("main not found");
		return 1;
    }
    state.pc = start_am.params.address;
	while (state.pc >= 0 && code.code[state.pc].ins != STOP) {
		cout << pcToString(state.pc) << ": " << instructionToString(&code.code[state.pc]) << endl;
		//cout << "S:";printStack();
		step(code);
	}
	if (state.pc < 0)
		cout << "Error: PC is Non-executable: " << pcToString(state.pc) << endl;
	else
		cout << pcToString(state.pc) << ": " << instructionToString(&code.code[state.pc]) << endl;
	//printf("F:"); showMainFrame();
	//printf("S:"); printStack();


	return 0;
}
