/*
 * g1.cpp
 *
 *  Created on: Sep 18, 2019
 *      Author: menright
 */

#include <iostream>
#include <fstream>
#include <list>
#include <map>
#include <stack>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
#include <stddef.h> // official home of ptrdiff_t
#include <assert.h>
#include <sstream>
#include "Token.h"
#include "Expr.h"
#include "util.h"

using namespace std;

void f() {
	list<int> ints;
	fold(ints, 0, [](int a,int b){ return a+b; });
}

struct Definition {
    string name;
    list<string> args;
    Expr* body;
};

list<string> parse_names(void) {
	list<string> names;
	while (token.type == T_NAME) {
		names.push_back(token.s);
		next();
	}
	return names;
}
Expr* parse_let();
Expr* E(int);
Expr* parse_expr() {
	if (token.type == T_LET) {
		return parse_let();
	}
	 return E(0);
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
	cerr << def.name;
	if (def.args.size())
		cerr << ' ' << join(def.args, ' ');
	cerr << " :=" << endl;
    indent(col+2);
    cerr << def.body->to_string(col+2) << endl;
}

void pprint_defs(int col, const list<Definition>& defs)
{
	for (auto def : defs)
        pprint_def(col, def);
}

enum InstructionType {
	ADD,
	ALLOC,
	CONS,
	DIV,
	EQ,
	EVAL,
	GE,
	GET,
	GT,
	HD,
	JFALSE,
	JMP,
	LABEL,
	LE,
	LT,
	MKAP,
	MKBOOL,
	MKINT,
	MOD,
	MUL,
	NE,
	NEG,
	NOT,
	NULLinst,
	PRINT,
	PUSH,
	PUSHBASIC,
	PUSHBOOL,
	PUSHFUN,
	PUSHINT,
	PUSHNIL,
	RET,
	SLIDE,
	SUB,
	TL,
	UNWIND,
	UPDATE,
	STOP,
	zzzmaxInstr
};
/*
 *
1. <0,PRINT.c, n.s, v, G[n=INT i], E, D> => <o;i, c, s, v, G[n=INT i], E, D>
2. <0,PRINT.c, n.s, v, G[n=BOOL b], E, D> => <o;b, c, s, v, G[n=BOOL b], E, D>
3. <o,PRINT.c, n.s, v, GIn=CONS nln2] , E, D> =>
	<o, EVAL.PRINT.EVAL.PRINT.c, n..n^.s, v, GIn=CONS n.n^], E, D>
4. <o,PRINT.c, n.s, v, GIn=NIL], E, D> => <o, c, s, v, G[n=NILJ, E, D>
5. <o,EVAL.c, n.s, v; G[n=AP n.n^],1 z E, D> => <o, UNWIND.(), n.(), v, G[n=AP nln2] , E, (c,s).D>
6. <o,EVAL.c, n.s, v, G[n=INT i], E, D> => <o, c, n.s, v, G[n=INT i], E, D>,
	similarly for nodes BOOL b, NIL, CONS nln 2 and FUN f.
7.<O,UNWIND.(), n.s, v, G[n=AP nln~] , E, D> => <o, U~IND~(), nl.n.s, G[n=AP nlne], E, D>
8.<O,UNWIND.()[ no.n1[ :.nk.s , v, GLno=FUN f, nI=AP n I n I ,...nk=AP,n k[,~''], E[f=(k,c)], D> =>
	<0, C, n .... n k .s, v, G[no=FUN f, nI=AP n 1'n1'',...nk=AP h nk ']' E[f=(k'c )]' D>
9.<O,UNWIND.(1, no.nl...nk.() , v, G[no=FUN f], E[f=(a,c')], (c ,s').D> and k < a =>
<o, c , nk.s , V, G[no=FUN f], E[f=(k,c')], D>
10.<O,RET m.c, v, n I ... nm.n.() , G[n=INT i], E, (c',s').D> =>
<o, c', n.s', v, G[n=INT i], E, D>, similarly for nodes BOOL b, NIL and CONS nln 2.
11.<0,RET m.c, n I .. nm.n.s, ,v, G[n=AP nln2] , E, D> =>
    <O, UNWIND. i)I n.s, v, G[n=AP n I n2] , E, D>, similarly for n = FUN f.
12.<O,PUSHINT i.c, s, v, G, E, D> => <o, c, n'.s, v, G[n'=INT i], E, D>
13.<o,PUSHBOOL b.c, s, v, G, E, D> => <o, c, n'.s, v, G[n'=BOOL b], E, D>
14.<o,PUSHNIL.c, s, v, G, E, D> => <o, c, n'.s, v, GIn'=NIL], E, D>
15.<o,PUSHFUN f.c, s, v, G, E, D> => <o, c, n'.s, v, G[n'=FUN f], E, D>
16.<o,PUSH m.c, n O ..... nm.S, v, G, E, D> => <o, c, nm.n 0 .... nm.S, v, G, E, D>
17.<o,MKINT.c, s, i.v, G, E, D> => <o, c, n'.s, v, G[n'=INT i], E, D>
18.<o,MKBOOL.c, s, b.v, G, E, D> => <o, c, n'.s, v, G[n'=BOOL b], E, D>
19.<o,MKAP.c, nl.n2.s , G, E, D> => <o, c, n'.s, v, G[n'=AP n2nl] , E, D>
20.<o,CONS.c, nl.n2.s , G, E, D> => <o, c, n'.s, v, G[n'=CONS n2nl] , E, D>
21.<o,ALLOC m.c, s, v, G, E, D> => <o, c, n 1'...n '.s, v, Gin I '= HOLE, ... n ': HOLE], E, D>
22.<o,UPDATE m.c, n O . • n .s, v, Gin O" No, n m" Nm] , E, D> => <o, c, s, v, n I. .. nm.S, G[no: NO, nm: NO] , E, D>
"" m _" _ _" _
m m
23.<O,SLIDE m.c, n O ..... nm.S , v, G, E, D> => <o, c, nO.s , v, G, E, D>
24.<O,GET. c, n.s, v, G[n=INT i], E, D> => <o, c, s, i.v, G[n=INT i], E, D>
25.<o,GET.c, n.s, v, G[n=BOOL b], E, D> => <o, c, s, b.v, G[n=BOOL b], E, D>
26.<O,PUSHBASIC i.c, s, v, G, E, D> --> <o, c, s, i.v, G, E, D>
27.<o,ADD.c, s, i2.i 1.v, G, E, D> => <o, c, s, (i1+i2).v , G, E, D>, similarly for SUB, MUL, DIV,
EQ, NE, LT, GT, LE and GE, the last six putting boolean values on V
28.<O,NEG.c, s, i.v, G, E, D> => <o, c, s, (-i).v, G, E, D>
29.<o,NOT.c, s, b.v, G, E, D> => <o, c, s, (not b).v, G, E, D>
30.<o,JFALSE 1.c, s, true.v, G, E, D> => <o, c, s, G, E, D>
31.<o,JFALSE 1.c, s, false.v, G, E, D> => <o, JMP 1.c, s, v, G, E, D>
32.<o,JMP 1 .... LABEL 1.c, s, v, G, E, D> => <o, c, s, v, G, E, D>
33.<o,LABEL 1.c, s, G, E, D> => <o, c, s, G, E, D>
34.<o,HD.c, n.s, v, G[n=CONS nln2] , E, D> => <o, c, nl.s, v, GIn=CONS nln2] , E, D>,
similarly for TL
35.<O,NULL.c, n.s, v, G[n=CONS nln2] , E, D> => <o, c, s, false.v, GIn=CONS nln2] , E, D>
36.<O,NULL.c, n.s, v, G[n=NIL], E, D> => <o, c, s, true.v, GIn=NIL], E, D>


*/
const char* insNames[] = {
		"ADD",
		"ALLOC",
		"CONS",
		"DIV",
		"EQ",
	"EVAL",
	"GE",
	"GET",
		"GT",
	"HD",
	"JFALSE",
	"JMP",
	"LABEL",
	"LE",
	"LT",
	"MKAP",
	"MKBOOL",
	"MKINT",
	"MOD",
	"MUL",
	"NE",
	"NEG",
	"NOT",
	"NULL",
	"PRINT",
	"PUSH",
	"PUSHBASIC",
	"PUSHBOOL",
	"PUSHFUN",
	"PUSHINT",
	"PUSHNIL",
	"RET",
	"SLIDE",
	"SUB",
	"TL",
	"UNWIND",
		"UPDATE",
		"STOP"
};
const char* insToString(InstructionType ins) {
	if (0<=ins && ins<zzzmaxInstr)
		return insNames[ins];
	return "inxxx";
}
struct NFun;
struct NInt;
struct NBool;
struct NAp;
struct NCons;
struct NNil;
struct NHole;
struct NodeVisitor {
	virtual ~NodeVisitor() {}
	virtual void visitNFun(NFun* n) = 0;
	virtual void visitNInt(NInt* n) = 0;
	virtual void visitNBool(NBool* n) = 0;
	virtual void visitNAp(NAp* n) = 0;
	virtual void visitNCons(NCons* n) = 0;
	virtual void visitNNil(NNil* n) = 0;
	virtual void visitNHole(NHole* n) = 0;
};
struct Node {
	virtual ~Node() {}
	virtual string to_string() const = 0;
	virtual void visit(NodeVisitor* v) = 0;
};
struct NHole : public Node {
	NHole() {}
	string to_string() const { return "NHole"; }
	void visit(NodeVisitor* v) { v->visitNHole(this); }
};
struct NFun : public Node {
	NFun(ptrdiff_t address, unsigned args) : address(address), args(args) {}
	string to_string() const {
		return "NFun " + ::to_string(address);
	}
	void visit(NodeVisitor* v) { v->visitNFun(this); }
	ptrdiff_t address;
	unsigned args;
};
struct NInt : public Node {
	NInt(ptrdiff_t i) : i(i) {}
	string to_string() const {
		return "NInt " + ::to_string(i);
	}
	void visit(NodeVisitor* v) { v->visitNInt(this); }
	ptrdiff_t i;
};
struct NBool : public Node {
	NBool(bool b) : b(b) {}
	string to_string() const {
		return "NBool " + ::to_string(b);
	}
	void visit(NodeVisitor* v) { v->visitNBool(this); }
	bool b;
};
struct NAp : public Node {
	NAp(Node* a1, Node* a2) : a1(a1), a2(a2) {}
	string to_string() const {
		std::ostringstream os;
		os << "NAp (" << a1->to_string() << " " << a2->to_string() << ")";
		return os.str();
	}
	void visit(NodeVisitor* v) { v->visitNAp(this); }
	Node* a1;
	Node* a2;
};
struct NCons: public Node {
	NCons(Node* hd, Node* tl) : hd(hd), tl(tl) {}
	string to_string() const {
		return "("+hd->to_string()+":" + tl->to_string() +")";
	}
	void visit(NodeVisitor* v) { v->visitNCons(this); }
	Node* hd;
	Node* tl;
};
struct NNil: public Node {
	NNil() {}
	string to_string() const {
		return "nil";
	}
	void visit(NodeVisitor* v) { v->visitNNil(this); }
};
struct Instruction {
	Instruction():ins(STOP),dest(0),node(nullptr) {}
	Instruction(InstructionType ins) : ins(ins),dest(0),node(nullptr) {}
	Instruction(InstructionType ins, bool b) : ins(ins),dest(0),node(nullptr),b(b) {}
	Instruction(InstructionType ins, unsigned n) : ins(ins),dest(0),node(nullptr),n(n) {}
	Instruction(InstructionType ins, size_t n) : ins(ins),dest(0),node(nullptr),n(n) {}
	Instruction(InstructionType ins, ptrdiff_t dest) : ins(ins),dest(dest),node(nullptr) {}
	Instruction(InstructionType ins, NFun* node) : ins(ins),dest(0),node(node) {}
	InstructionType ins;
	ptrdiff_t dest;
	NFun* node;
	union {
		unsigned n;
		int i;
		bool b;
	};
};
struct CodeArray {
	vector<Instruction> code;
	void add(const Instruction& i) {
		code.push_back(i);
	}
	void add(InstructionType ins)                 { add(Instruction(ins)); }
	void add(InstructionType ins, bool b)         { add(Instruction(ins,b)); }
	void add(InstructionType ins, unsigned n)     { add(Instruction(ins,n)); }
	void add(InstructionType ins, size_t n)       { add(Instruction(ins,n)); }
	void add(InstructionType ins, ptrdiff_t dest) { add(Instruction(ins,dest)); }
	void add(InstructionType ins, NFun* node)     { add(Instruction(ins,node)); }
};
struct AddressMode {
	enum Mode { Local, Global };
	Mode mode;
	union { unsigned localIndex; /*ptrdiff_t address;*/ NFun* node; };
	AddressMode(NFun* node) : mode(Global), node(node) {}
	AddressMode() : mode(Local) {}
};
string instructionToString(const Instruction& ins) {
	string rv = insToString(ins.ins);
	switch (ins.ins) {
	case JFALSE:
	case JMP:
	case PUSHBASIC: rv += " " + ::to_string(ins.dest); break;
	case PUSHBOOL: rv += " " + ::to_string(ins.b); break;
	case PUSHINT: rv += " " + ::to_string(ins.dest); break;
	case PUSHFUN: rv += " " + ins.node->to_string(); break;
	case ALLOC:
	case PUSH:
	case UPDATE:
	case SLIDE:
	case RET:
		rv += " " + ::to_string(ins.n); break;
	case UNWIND:
	case STOP:
		break;
	default:
		break;
	}
	return rv;
}
string instructionToString(const Instruction* ins) {
	return instructionToString(*ins);
}
struct EnvItem {
	EnvItem() : args(), mode() {}
	int args;
	AddressMode mode;
};
typedef map<string,EnvItem> Env;

string amToString(AddressMode m) {
	switch (m.mode) {
	case AddressMode::Local: return "local "+::to_string(m.localIndex);
	case AddressMode::Global: return "global "+::to_string(m.node->address);
	}
	return "--";
}
static
void pprint_env(const Env& env) {
        for (auto i: env) {
                //cerr << "env "<<i.first <<" entry "<<i.second << flush;/*, env, entry); fflush(stdout);*/
                //printf(" %s args %d mode %s\n", entry->name, entry->args, amToString(entry->mode.mode));
                cerr << i.first << " args " << i.second.args << ' ' << amToString(i.second.mode) << endl;
        }
}

typedef list<Node*> GmStack;
GmStack nodeStack;
typedef list<ptrdiff_t> ValueStack;
ValueStack valueStack;
struct DumpItem {
	GmStack nodeStack;
	ptrdiff_t code;
};
typedef list<DumpItem> Dump;
Dump dump;
void showStack(const string& label) {
	cerr << label << endl;
	for (auto s : nodeStack) {
		cerr << /*s << ' ' <<*/ '[' << s->to_string() << "] ";
	}
	cerr << endl;
}
void showValues(const string& label) {
	cerr << label << endl;
	for (auto s : valueStack) {
		cerr << /*s << ' ' <<*/ '[' << ::to_string(s) << "] ";
	}
	cerr << endl;
}
void stepPushBool(const Instruction& ins) {
	showStack("Stack before pushBool");
	nodeStack.push_front(new NBool(ins.b));
	showStack("Stack after pushBool");
}
void stepPushFun(NFun* sc) {
	showStack("Stack before pushFun");
	nodeStack.push_front(sc);
	showStack("Stack after pushFun");
}
void stepPushInt(ptrdiff_t i) {
	showStack("Stack before pushInt");
	nodeStack.push_front(new NInt(i));
	showStack("Stack after pushInt");
}
void stepPushNil() {
	showStack("Stack before pushNil");
	nodeStack.push_front(new NNil());
	showStack("Stack after pushNil");
}
void stepMkAp() {
	showStack("Stack before mkap");
	Node* a2 = nodeStack.front(); nodeStack.pop_front();
	Node* a1 = nodeStack.front(); nodeStack.pop_front();
	nodeStack.push_front(new NAp(a1, a2));
	showStack("Stack after mkap");
}
void stepPush(unsigned n) {
	showStack("Stack before push");
	unsigned i=0;
	for (const auto& se : nodeStack) {
		cerr << i << ": " << se << ' ' << se->to_string() << endl;
		++i;
	}
	auto p = nodeStack.begin();
#if 1
	advance(p,n);
	Node* node = *p;
	//auto ap = dynamic_cast<NAp*>(node);
	//assert(ap);
	//auto arg = ap->a2;
	//nodeStack.push_front(arg);
	nodeStack.push_front(node);
#else
	advance(p,n);
	Node* node = *p;
	cerr << "Push: About to pull " << node->to_string() << " up to top " << endl;
	auto ap = dynamic_cast<NAp*>(node);
	assert(ap);
	auto arg = ap->a2;
	nodeStack.push_front(arg);
#endif
	cerr << "Stack after push " << endl;
	i=0;
	for (const auto& se : nodeStack) {
		cerr << i << ": " << se << ' ' << se->to_string() << endl;
		++i;
	}
}
void stepSlide(int n) {
	throw __PRETTY_FUNCTION__ + string(" rewrite for G");
	showStack("stack before slide "+::to_string(n));
	auto a0 = nodeStack.front();
	nodeStack.pop_front();
	for (int i=1; i<=n; ++i)
		nodeStack.pop_front();
	nodeStack.push_front(a0);
	showStack("Stack after slide");
}
void stepUpdate(unsigned m) {
	//      <o, UPDATE m.c, n0 n1 ... nm.s, v, G[n0:N0, nm:Nm], E, D>
	//   => <o,          c,    n1 ... nm.s, v, G[n0:N0, nm:N0], E, D>
	// Meaning,
	//	Take the node from the top of the stack and put it at the mth position on the stack
	//  Drop the top node from the stack
	//  The instruction is done.
	//
	// This means that there must be m+1 stack entries available.
	// The dump does not save you.

	showStack("stack before update "+::to_string(m));
	if (nodeStack.size()<(m+1)) {
		cerr << "Judgment " << nodeStack.size() << " vs " << (m+1) << " is " << (nodeStack.size()>(m+1)) << " Stack size " << nodeStack.size() << " not big enough for update " << m << endl;
		throw "Bad Update";
	}
	Node* tos = nodeStack.front(); nodeStack.pop_front();
	showStack("stack during update "+::to_string(m));
	auto p = nodeStack.begin();
	advance(p, m-1);
	//cerr << "distance(gmStack.begin(),p) " << distance(nodeStack.begin(),p)
	//	<< " gmStack.size() " << nodeStack.size() << endl;
	//assert((unsigned)distance(gmStack.begin(),p) < gmStack.size());
	*p = tos;//new NInd(tos);
	showStack("Stack after update");
}
list<Node*> tl(list<Node*> x) {
	auto r = x;
	r.pop_front();
	return r;
}
list<Node*> take(unsigned t, list<Node*> x) {
	list<Node*> r;
	list<Node*>::const_iterator px = x.begin();
	for (unsigned i=0; i<t; i++)
	{
		r.push_back(*px++);
	}
	return r;
}
list<Node*> drop(unsigned d, list<Node*> x) {
	list<Node*> r = x;
	auto px = next(r.begin(),d);
	r.erase(r.begin(), px);
	return r;
}
list<Node*> concat(list<Node*> a, list<Node*> b) {
	list<Node*> r = a;
	r.insert(r.end(), b.begin(), b.end());
	return r;
}
list<Node*> rearrange(unsigned n, list<Node*> as) {
	auto asp = mapf(tl(as), [](Node* el) {
		auto ap=dynamic_cast<NAp*>(el);
		if (ap) {
			return ap->a2;
		}
		return el;
	});
	return concat(take(n, asp), drop(n, as));
}
struct UnwindNodeVisitor : public NodeVisitor {
	UnwindNodeVisitor(ptrdiff_t& pc) : pc(pc),done(false) {}
	ptrdiff_t& pc;
	bool done;
	void visitNFun(NFun* gtop) {
		if (nodeStack.size() < gtop->args) {
			cerr << __PRETTY_FUNCTION__ << ": stack " << nodeStack.size() << " not enough for " << gtop->args << " arguments" << endl;
		}
		//    <o, UNWIND.(), n0...nk.s,    v, G[n0=FUN f,n1...nk=AP], E[f=(k,c)], D> ;; i.e. there are k+1 stack nodes at least.
		// => <o, c,         arg1...argk.s,v, G,                      E,          D>
		//    Enough args:
		//      Drop all the nodes n0..nk, push the arguments and jump to the function code.
		//
		//    <o, UNWIND.(), n0...nk.(),   v, G[n0=FUN f,n1...nk=AP], E[f=(a,c)], (c,s').D> ;; k<a not enough
		// => <o, c,         nk.s',        v, G,                      E,                 D>
		//    NOT enough args:
		//      Pop a dump item.
		//      Switch to dump item stack
		//      push nk from previous stack.
		//      Execute code from dump item.
		if (nodeStack.size() >= (gtop->args+1)) {
			showStack("Stack before fun unwind");
			GmStack spine;
			GmStack::const_iterator p=nodeStack.begin(); // where NFun is.
			++p; // first argument;
			for (unsigned i=0; i<gtop->args; ++i) {
				Node* a = *p++;
				NAp* c = dynamic_cast<NAp*>(a);
				if (c == nullptr) {
					cerr << "argument should be NAp but isn't" << endl;
					throw "Bad arg";
				}
				spine.push_back(c->a2);
			}
			cerr << __PRETTY_FUNCTION__<< " Spine [";
			for (const auto& se : spine) {
				cerr << ' ' << se->to_string();
			}
			cerr << "] " << spine.size() << " elements. We need " << gtop->args << " of these." << endl;
			cerr << __PRETTY_FUNCTION__ << " Fun " << gtop->to_string() << endl;
			cerr << __PRETTY_FUNCTION__ << " "; showStack("original stack before rearranging");
			nodeStack = concat(take(gtop->args, spine), drop(gtop->args,nodeStack));
			showStack("Stack during fun unwind");

			pc = gtop->address;
		} else {
			showStack("Stack before fun unwind given "+::to_string(nodeStack.size())+" needing "+::to_string(gtop->args));
			DumpItem d = dump.front(); dump.pop_front();
			auto nodeK = nodeStack.back();
			nodeStack = d.nodeStack;
			nodeStack.push_back(nodeK);
			//throw "Unwind needs to use dump";
			pc = d.code;
			showStack("Stack during fun unwind with grab from dump");
		}
		done = true;
	}
	void visitNAp(NAp* aptop) {
		//    <o, UNWIND.(),    n.s, v, G[n=Ap n1,n2], E, D>
		// => <o, UNWIND.(), n1.n.s, v, G[n=Ap n1,n2], E, D>
		// That is, Take the fun graph from the Nap and push it to the node stack.
		// Then repeat UNWIND
		//cerr << "aptop " << aptop->to_string() << endl;
		//cerr << "aptop.a1 " << aptop->a1->to_string() << " aptop.a2 " << aptop->a2->to_string() << endl;
		nodeStack.push_front(aptop->a1);
		showStack("Stack during ap unwind");
		//pc--; // instead of backing up, we reiterate directly.
	}
	void visitNInt(NInt*) { throw "don't unwind INT"; }
	void visitNBool(NBool*) { throw "don't unwind BOOL"; }
	void visitNCons(NCons*) { throw "don't unwind Cons"; }
	void visitNNil(NNil*) { throw "don't unwind Nil"; }
	void visitNHole(NHole*) { throw "Don't unwind a hole";}
};
void stepEval(/*const Instruction& instr,*/ ptrdiff_t& pc);
void stepPrint(const Instruction& ins, ptrdiff_t& pc);
struct PrintNodeVisitor : public NodeVisitor {
	PrintNodeVisitor() : done(false) {}
	bool done;
	void visitNInt(NInt* n) {
		cout << n->i;//n->to_string();
		nodeStack.pop_front();
		done = true;
	}
	void visitNBool(NBool* n) {
		cout << (n->b?"true":"false");//n->to_string();
		nodeStack.pop_front();
		done = true;
	}
	void visitNCons(NCons* n) {
#if 1
		nodeStack.push_front(n->tl);
		nodeStack.push_front(n->hd);
		ptrdiff_t pc = 0;
		Instruction p(PRINT);
		stepEval(pc);
		stepPrint(p, pc);
		stepEval(pc);
		stepPrint(p,pc);
		nodeStack.pop_front();
		done = true;
#endif
		//throw "don't print Cons yet";
	}
	void visitNNil(NNil*) { /*cerr << n->to_string();*/ nodeStack.pop_front(); done=true;}
	void visitNFun(NFun*) {
		throw "Don't print a fun";
	}
	void visitNAp(NAp*) {
		throw "Don't print an apply";
	}
	void visitNHole(NHole*) {
		throw "Don't print a hole";
	}
};
// Unwind will cause a jump if the top node is an NFun.
void stepUnwind(ptrdiff_t& pc) {
	showStack("Stack before unwind");
	UnwindNodeVisitor visitor(pc);
	while (!visitor.done) {
		Node* top = nodeStack.front();
		cerr << "Unwind viewing " << top->to_string() << " from top of stack of size " << nodeStack.size() << endl;
		top->visit(&visitor);
	}
	showStack("Stack after unwind");
}
void stepPrint(const Instruction& ins, ptrdiff_t& pc) {
	showStack("Stack before print");
	PrintNodeVisitor visitor;
	while (!visitor.done) {
		nodeStack.front()->visit(&visitor);
	}
	showStack("Stack after print");
}
void stepPushBasic(const Instruction& ins) {
	showValues("Stack before pushbasic");
	valueStack.push_front(ins.dest);
	showValues("Stack after pushbasic");
}
void stepUnop(const Instruction& ins){
	showValues("Stack before "+instructionToString(ins));
	if (valueStack.size() < 1)
		throw string(insToString(ins.ins))+" Value stack doesn't have an operand";
	switch (ins.ins) {
	case NEG: valueStack.front() = -valueStack.front(); break;
	case NOT: valueStack.front() = !valueStack.front(); break;
	case NULLinst: {
		auto nilp = dynamic_cast<NNil*>(nodeStack.front());
		auto res = (nilp != NULL);
		nodeStack.pop_front();
		valueStack.push_front(res);
		break;
	}
	case HD: {
		auto cons = dynamic_cast<NCons*>(nodeStack.front());
		nodeStack.front() = cons->hd;
		break;
	}
	case TL: {
		auto cons = dynamic_cast<NCons*>(nodeStack.front());
		nodeStack.front() = cons->tl;
		break;
	}
	default: throw "Unimplemented math in stepBinop";
	}
	showValues("Stack after "+instructionToString(ins));
}
void stepBinop(const Instruction& ins) {
	showValues("Stack before "+instructionToString(ins));
	if (valueStack.size() < 2)
		throw string(insToString(ins.ins))+" Value stack doesn't have two operands";
	auto right = valueStack.front(); valueStack.pop_front();
	auto left = valueStack.front();
	switch (ins.ins) {
	case ADD: valueStack.front() = left+right; break;
	case SUB: valueStack.front() = left-right; break;
	case MUL: valueStack.front() = left*right; break;
	case DIV: valueStack.front() = left/right; break;
	case MOD: valueStack.front() = left%right; break;
	case LT: valueStack.front() = (left<right); break;
	case GT: valueStack.front() = (left>right); break;
	case LE: valueStack.front() = (left<=right); break;
	case GE: valueStack.front() = (left>=right); break;
	case NE: valueStack.front() = (left!=right); break;
	case EQ: valueStack.front() = (left==right); break;
	default: throw "Unimplemented math in stepBinop";
	}
	showValues("Stack after "+instructionToString(ins));
}
void stepMkBool() {
	showValues("Stack before MKBOOL");
	nodeStack.push_front(new NBool(!!valueStack.front()));
	valueStack.pop_front();
	showStack("Stack after MKBOOL");
}
void stepMkInt() {
	showValues("Stack before MKINT");
	nodeStack.push_front(new NInt(valueStack.front()));
	valueStack.pop_front();
	showStack("Stack after MKINT");
}
void stepRet(const Instruction& ins, ptrdiff_t& pc) {
	showStack("Before RET "+::to_string(ins.n));
	if (nodeStack.size() < ins.n)
		throw "Stack underflow";
	for (unsigned n=0; n<ins.n; n++)
		nodeStack.pop_front();
	showStack("RET after popping "+::to_string(ins.n));
	if (nodeStack.size()<1)
		throw "No whnf node left on stack";
	while (true) {
		auto ap = dynamic_cast<NAp*>(nodeStack.front());
		if (ap) {
			pc = 1;
			break;
		}
		auto fn = dynamic_cast<NFun*>(nodeStack.front());
		if (fn) {
			pc = 1;
			break;
		}
		// New way: Assume that nodes that arent FUN, IND or AP are
		// whnf already
		if (dump.size()==0)
		{
			pc = 0; // halt when there's no dump.
			break;
		}
		// Pop the dump and push the whnf node onto that stack.
		// Then continue from PC saved in dump.
		auto n = nodeStack.front();
		auto d = dump.front(); dump.pop_front();
		nodeStack = d.nodeStack;
		pc = d.code;
		nodeStack.push_front(n);
		break;
		// Old way
		auto i = dynamic_cast<NInt*>(nodeStack.front());
		if (i) {
			// If the DUMP is empty we stop
			pc = 0;
			break;
		}
		auto b = dynamic_cast<NBool*>(nodeStack.front());
		if (b) {
			pc = 0;
			break;
		}
		break;
	};
	showStack("After RET");
}
void stepEval(/*const Instruction& instr,*/ ptrdiff_t& pc) {
	while (true) {
		showStack("Before iteration of EVAL");
		auto ap = dynamic_cast<NAp*>(nodeStack.front());
		if (ap) {
			//throw "Eval of apply needs to add to dump";
			DumpItem e;
			e.code = pc; // instruction after Eval.
			e.nodeStack = nodeStack;
			e.nodeStack.pop_front();
			dump.push_front(e);
			if (nodeStack.size()>1) {
				nodeStack.erase(next(nodeStack.begin()),nodeStack.end());
			}
			pc = 1; // Go to unwind
			break;
		}
		// All other nodes are whnf and need no further effort.
		break;
	}
	showStack("After EVAL");
}
void stepGet(/*const Instruction& instr*/) {
	showStack("Before GET");
	while (true)
	{
		auto i = dynamic_cast<NInt*>(nodeStack.front());
		if (i) {
			// If the DUMP is empty we stop
			nodeStack.pop_front();
			valueStack.push_front(i->i);
			break;
		}
		auto b = dynamic_cast<NBool*>(nodeStack.front());
		if (b) {
			nodeStack.pop_front();
			valueStack.push_front(b->b);
			break;
		}
		throw "GET wrong node type";
	}
	showValues("After GET");
}
void stepJFalse(const Instruction& ins, ptrdiff_t& pc) {
	auto truth = valueStack.front(); valueStack.pop_front();
	if (!truth)
		pc = ins.dest;
}
void stepJmp(const Instruction& ins, ptrdiff_t& pc) {
	pc = ins.dest;
}
void stepCons(const Instruction& ins) {
	showStack("Before CONS");
	auto tl = nodeStack.front(); nodeStack.pop_front();
	auto hd = nodeStack.front(); nodeStack.pop_front();
	nodeStack.push_front(new NCons(hd, tl));
	showStack("After CONS");
}
void stepAlloc(const Instruction& ins) {
	cerr << __PRETTY_FUNCTION__ << " Allocating local vars " << ins.n << endl;
	for (unsigned i=0; i<ins.n; i++) {
		nodeStack.push_back(new NHole());
	}
}
bool step(CodeArray& code, ptrdiff_t& pc) {
	Instruction instr = code.code[pc++];
	switch (instr.ins) {
	/* Label is for over-completeness. It's used in the paper but not here */
	case LABEL: throw "Unknown instruction in step"; break;
	case ALLOC: stepAlloc(instr); break;
	case ADD: stepBinop(instr); break;
	case CONS: stepCons(instr); break;
	case DIV:
	case EQ: stepBinop(instr); break;
	case EVAL: stepEval(/*instr,*/ pc); break;
	case GE: stepBinop(instr); break;
	case GET: stepGet(/*instr*/); break;
	case GT: stepBinop(instr); break;
	case HD: stepUnop((instr)); break;
	case JFALSE: stepJFalse(instr, pc); break;
	case JMP: stepJmp(instr, pc); break;
	case LE:
	case LT: stepBinop(instr); break;
	case MKAP: stepMkAp(); break;
	case MKBOOL: stepMkBool(); break;
	case MKINT: stepMkInt(); break;
	case MOD:
	case MUL:
	case NE: stepBinop(instr); break;
	case NEG: stepUnop(instr); break;
	case NOT: stepUnop(instr); break;
	case NULLinst: stepUnop(instr); break;
	case PRINT: stepPrint(instr,pc); break;
	case PUSH: stepPush(instr.n); break;
	case PUSHBASIC: stepPushBasic(instr); break;
	case PUSHBOOL: stepPushBool(instr); break;
	case PUSHFUN: stepPushFun(instr.node); break;
	case PUSHINT: stepPushInt(instr.dest); break;
	case PUSHNIL: stepPushNil(); break;
	case RET: stepRet(instr, pc); break;
	case SLIDE: stepSlide(instr.n); break;
	case SUB: stepBinop(instr); break;
	case TL: stepUnop(instr); break;
	case UPDATE: stepUpdate(instr.n); break;
	case UNWIND: stepUnwind(pc); break;
	case STOP: break;
	case zzzmaxInstr: break; // dummy value
	}
	return instr.ins != STOP;
}
static
void envAddArgs(Env& env, const list<string> args) {
	int kArg = args.size()+1;
	for (auto arg : args) {
        EnvItem entry;
        //entry.name = arg;
        entry.mode.mode = AddressMode::Local;
        entry.mode.localIndex = kArg;
        kArg--;
        env[arg] = entry;
	}
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

//F[f x1 ... xm = e] = E[e] r (m+1); UPDATE (m+1); RET m;
//  r = [x1=m+1, x2=m, ..., xm = 2]

unsigned r(unsigned i, unsigned m) {
	return m+1-i;
}
void compileC(CodeArray& code, Expr* expr, const Env& env, unsigned depth);
void compileE(CodeArray& code, Expr* expr, Env& env, unsigned depth);
void compileB(CodeArray& code, Expr* expr, Env& env, unsigned depth);
struct CompileCVisitor : public ExprVisitor {
	CompileCVisitor(CodeArray& code, const Env& env, unsigned depth) : code(code), env(env), depth(depth) {}
	CodeArray& code;
	const Env& env;
	unsigned depth;
	void visitExprNum(ExprNum* eint) {
		code.add(Instruction(PUSHINT,eint->value));
	}
	void visitExprVar(ExprVar* evar) {
		auto pm = env.find(evar->var);
		if (pm != env.end()) {
			switch (pm->second.mode.mode) {
			case AddressMode::Local: {
				code.add(Instruction(PUSH,depth-pm->second.mode.localIndex+1));
				break;
			}
			case AddressMode::Global: code.add(Instruction(PUSHFUN,pm->second.mode.node)); break;
			}
			return;
		}
		cerr << __PRETTY_FUNCTION__ << " Can't find " << evar->var << " in "; pprint_env(env);
	}
	void visitExprApp(ExprApp* eapp) {
		/*
		 * C[e1 e2] r n = C[e1] r n; C[e2] r n+1; MKAP
		 */
#ifdef BIG_LIST_AP
		int shiftCount = 0;
		compileC(code, eapp->fun, env, depth);
		shiftCount++;
		auto pArg = eapp->args.begin();
		while (pArg != eapp->args.end()) {
			compileC(code, *pArg++, env, depth+shiftCount); shiftCount++;
			code.add(Instruction(MKAP));
		}
#else
		compileC(code, eapp->fun, env, depth);
		compileC(code, eapp->arg, env, depth+1);
		code.add(Instruction(MKAP));
#endif
	}
	pair<Env,unsigned> compileXr(list<ExprLet::Binding>& d, const Env& env, unsigned depth) {
		Env env2 = env;
		unsigned a = 1;
		for (auto binding: d) {
			EnvItem e; e.args = 0; e.mode.mode = AddressMode::Local; e.mode.localIndex = a++;
			env2[binding.name] = e;
		}
		auto m = d.size()+depth;
		return make_pair(env2,m);
	}
	void compileCletrec(list<ExprLet::Binding>& d, const Env& env, unsigned n, unsigned m) {
		code.add(ALLOC,m); // Allocate holes for every binding.
		auto u = m;
		for (auto binding: d) {
			compileC(code,binding.value, env, n+m); // not quite
			code.add(UPDATE,u);
			u--;
		}
	}
	void visitExprLet(ExprLet* e) {
		auto d=e->bindings;
		auto rpdepthp = compileXr(d, env, depth);
		//code.add(STOP);
		compileCletrec(d, rpdepthp.first, depth,rpdepthp.second);
		compileE(code,e->value,rpdepthp.first, rpdepthp.second);
		code.add(SLIDE, rpdepthp.second-depth);
		throw __PRETTY_FUNCTION__;
	}
	void visitExprBool(ExprBool* e) {
		code.add(Instruction(PUSHBOOL,e->value));
	}
	void visitExprChar(ExprChar*) { throw __PRETTY_FUNCTION__; }
	void visitExprStr(ExprStr*) { throw __PRETTY_FUNCTION__; }
	void visitExprNil(ExprNil*) {
		code.add(Instruction(PUSHNIL));
	}
	void visitExprIf(ExprIf* e) {
		cerr << __PRETTY_FUNCTION__ << " Not to be handled: " << e->to_string(0) << endl;
	}
	void visitExprCons(ExprCons* e) {
		compileC(code,e->hd, env,depth);
		compileC(code,e->tl, env, depth+1);
		code.add(Instruction(CONS));
	}
	void visitExprHd(ExprHd*) { throw __PRETTY_FUNCTION__; }
	void visitExprTl(ExprTl*) { throw __PRETTY_FUNCTION__; }
	void visitExprNull(ExprNull*) { throw __PRETTY_FUNCTION__; }
	void visitExprAdd(ExprAdd* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("add"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("add"), e->left), e->right), env, depth);
#endif
	}
	void visitExprSub(ExprSub* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("sub"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("sub"), e->left), e->right), env, depth);
#endif
	}
	void visitExprMul(ExprMul* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("mul"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("mul"), e->left), e->right), env, depth);
#endif
	}
	void visitExprDiv(ExprDiv* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("div"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("div"), e->left), e->right), env, depth);
#endif
	}
	void visitExprMod(ExprMod* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("mod"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("mod"), e->left), e->right), env, depth);
#endif
	}
	void visitExprEq(ExprEq* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("__eq"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("__eq"), e->left), e->right), env, depth);
#endif
	}
	void visitExprNe(ExprNe* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("__ne"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("__ne"), e->left), e->right), env, depth);
#endif
	}
	void visitExprLt(ExprLt* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("__lt"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("__lt"), e->left), e->right), env, depth);
#endif
	}
	void visitExprGt(ExprGt* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("__gt"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("__gt"), e->left), e->right), env, depth);
#endif
	}
	void visitExprLe(ExprLe* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("__le"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("__le"), e->left), e->right), env, depth);
#endif
	}
	void visitExprGe(ExprGe* e) {
#ifdef BIG_LIST_AP
		auto e2 = new ExprApp(new ExprVar("__ge"),e->left);
		e2->args.push_back(e->right);
		compileC(code, e2, env,depth);
#else
		compileC(code, new ExprApp(new ExprApp(new ExprVar("__ge"), e->left), e->right), env, depth);
#endif
	}
	void visitExprNeg(ExprNeg* e) {
		auto e2=new ExprApp(new ExprVar("__neg"),e->subj);
		compileC(code, e2, env, depth);
	}
	void visitExprNot(ExprNot* e) {
		auto e2 = new ExprApp(new ExprVar("__not"),e->subj);
		compileC(code, e2, env, depth);
	}
};
struct CompileBVisitor : public ExprVisitor {
	CompileBVisitor(CodeArray& code, Env& env, unsigned args) : code(code), env(env), depth(args) {}
	CodeArray& code;
	Env& env;
	unsigned depth;
	void visitExprNum(ExprNum* eint) {
		Instruction ins(PUSHBASIC,eint->value);
		code.add(ins);
	}
	void visitExprVar(ExprVar* evar) {
		compileE(code, evar, env, depth);
		code.add(Instruction(GET));
	}
	void visitExprApp(ExprApp* eapp) {
		compileE(code, eapp, env, depth);
		code.add(Instruction(GET));
	}
	void visitExprLet(ExprLet*) { throw __PRETTY_FUNCTION__; }
	void visitExprBool(ExprBool* e) {
		code.add(Instruction(PUSHBASIC,e->value));
	}
	void visitExprChar(ExprChar*) { throw __PRETTY_FUNCTION__; }
	void visitExprStr(ExprStr*) { throw __PRETTY_FUNCTION__; }
	void visitExprNil(ExprNil*) {
		code.add(Instruction(PUSHNIL));
	}
	void visitExprIf(ExprIf* e) {
		e->cond->visit(this);
		auto backpatch1 = code.code.size();
		code.add(Instruction(JFALSE));
		e->trueExpr->visit(this);
		auto backpatch2 = code.code.size();
		code.add(Instruction(JMP));
		code.code[backpatch1].dest = code.code.size();
		e->falseExpr->visit(this);
		code.code[backpatch2].dest = code.code.size();
	}
	void visitExprCons(ExprCons* e) {
		compileC(code,e->hd, env, depth);
		compileC(code,e->tl, env, depth+1);
		code.add(Instruction(CONS));
	}
	void visitExprHd(ExprHd*) { throw __PRETTY_FUNCTION__; }
	void visitExprTl(ExprTl*) { throw __PRETTY_FUNCTION__; }
	void visitExprNull(ExprNull* e) {
		compileE(code, e->subject, env, depth);
		code.add(Instruction(NULLinst));
	}
	void visitExprAdd(ExprAdd* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(ADD));
	}
	void visitExprSub(ExprSub* e) {
		cerr << __PRETTY_FUNCTION__ << " " << e->to_string(0) << endl;
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(SUB));
	}
	void visitExprMul(ExprMul* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(MUL));
	}
	void visitExprDiv(ExprDiv* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(DIV));
	}
	void visitExprMod(ExprMod* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(MOD));
	}
	void visitExprEq(ExprEq* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(EQ));
	}
	void visitExprNe(ExprNe* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(NE));
	}
	void visitExprLt(ExprLt* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(LT));
	}
	void visitExprGt(ExprGt* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(GT));
	}
	void visitExprLe(ExprLe* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(LE));
	}
	void visitExprGe(ExprGe* e) {
		compileB(code,e->left, env, depth);
		compileB(code,e->right, env, depth);
		code.add(Instruction(GE));
	}
	void visitExprNeg(ExprNeg* e) {
		compileB(code,e->subj, env, depth);
		code.add(Instruction(NEG));
	}
	void visitExprNot(ExprNot* e) {
		compileB(code, e->subj, env, depth);
		code.add(Instruction(NOT));
	}
};
struct CompileEVisitor : public ExprVisitor {
	CompileEVisitor(CodeArray& code, Env& env, unsigned depth) : code(code), env(env), depth(depth) {}
	CodeArray& code;
	Env& env;
	unsigned depth;
	void visitExprNum(ExprNum* eint) {
		code.add(Instruction(PUSHINT,eint->value));
	}
	void visitExprVar(ExprVar* evar) {
		auto pm = env.find(evar->var);
		if (pm != env.end()) {
			switch (pm->second.mode.mode) {
			case AddressMode::Local: {
				code.add(Instruction(PUSH,depth-pm->second.mode.localIndex+1));
				code.add(Instruction(EVAL)); break;
			}
			case AddressMode::Global: code.add(Instruction(PUSHFUN,pm->second.mode.node)); break;
			}
			return;
		}
		throw __PRETTY_FUNCTION__ + string(" ")+ evar->var + " undefined";
	}
	void visitExprApp(ExprApp* eapp) {
		compileC(code,eapp,env,depth);
		code.add(Instruction(EVAL));
	}
	void visitExprLet(ExprLet* e) {
		auto d=e->bindings;
		//code.add(STOP);
		CompileCVisitor innerc(code,env,depth);
		auto rpdepthp = innerc.compileXr(d, env, depth);
		innerc.compileCletrec(d, rpdepthp.first, depth,rpdepthp.second);
		compileE(code,e->value,rpdepthp.first, rpdepthp.second);
		code.add(SLIDE, rpdepthp.second-depth);
		//throw __PRETTY_FUNCTION__;
	}
	void visitExprBool(ExprBool* e) {
		code.add(Instruction(PUSHBOOL,e->value));
	}
	void visitExprChar(ExprChar*) { throw __PRETTY_FUNCTION__; }
	void visitExprStr(ExprStr*) { throw __PRETTY_FUNCTION__; }
	void visitExprNil(ExprNil*) {
		code.add(Instruction(PUSHNIL));
	}
	void visitExprIf(ExprIf* e) {
		// In the B scheme all the subexpressions are compiled
		// with B. In the E scheme only the condition is compiled
		// with B.
		compileB(code, e->cond, env,depth);
		auto backpatch1 = code.code.size();
		code.add(Instruction(JFALSE));
		e->trueExpr->visit(this);
		auto backpatch2 = code.code.size();
		code.add(Instruction(JMP));
		code.code[backpatch1].dest = code.code.size();
		e->falseExpr->visit(this);
		code.code[backpatch2].dest = code.code.size();
	}
	void visitExprCons(ExprCons* e) {
		compileC(code,e->hd, env, depth);
		compileC(code,e->tl, env, depth+1);
		code.add(Instruction(CONS));
	}
	void visitExprHd(ExprHd* e) {
		compileC(code,e->subject, env, depth);
		code.add(Instruction(HD));
	}
	void visitExprTl(ExprTl* e) {
		compileC(code,e->subject, env, depth);
		code.add(Instruction(TL));
	}
	void visitExprNull(ExprNull* e) {
		compileE(code, e->subject, env, depth);
		code.add(Instruction(NULLinst));
		code.add(Instruction(MKBOOL));
	}
	void visitExprAdd(ExprAdd* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKINT));
	}
	void visitExprSub(ExprSub* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKINT));
	}
	void visitExprMul(ExprMul* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKINT));
	}
	void visitExprDiv(ExprDiv* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKINT));
	}
	void visitExprMod(ExprMod* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKINT));
	}
	void visitExprEq(ExprEq* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKBOOL));
	}
	void visitExprNe(ExprNe* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKBOOL));
	}
	void visitExprLt(ExprLt* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKBOOL));
	}
	void visitExprGt(ExprGt* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKBOOL));
	}
	void visitExprLe(ExprLe* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKBOOL));
	}
	void visitExprGe(ExprGe* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKBOOL));
	}
	void visitExprNeg(ExprNeg* e) {
		compileB(code, e, env, depth);
		code.add(Instruction(MKINT));
	}
	void visitExprNot(ExprNot* e) {
		compileB(code,e->subj, env, depth);
		code.add(Instruction(NOT));
	}
};
void compileC(CodeArray& code, Expr* expr, const Env& env, unsigned depth) {
	CompileCVisitor visitor(code, env, depth);
	expr->visit(&visitor);
}
void compileE(CodeArray& code, Expr* expr, Env& env, unsigned depth) {
	CompileEVisitor visitor(code,env, depth);
	expr->visit(&visitor);
}
void compileB(CodeArray& code, Expr* expr, Env& env, unsigned depth) {
	CompileBVisitor visitor(code,env, depth);
	expr->visit(&visitor);
}
void compileR(CodeArray& code, Expr* expr, size_t depth, Env& env) {
#if 0
	compileC(code,expr,env);
	code.add(UpdateInstruction(depth));
	code.add(UnwindInstruction());
#else
	compileE(code,expr,env,depth);
	code.add(Instruction(UPDATE,depth+1));
	code.add(Instruction(RET,depth));
#endif
}
void compileSc(CodeArray& code, const Definition& def, Env& env) {
	Env env2 = env;
	envAddArgs(env2, def.args);
	compileR(code, def.body, def.args.size(), env2);
}
/*
 Manage the rerouting of a file's input to the std::cin object
 so that the parser can just use cin.
 */
struct InputFixer
{
	InputFixer() {
		orig_cin = nullptr;
	}
	~InputFixer() {
		if (orig_cin)
			cin.rdbuf(orig_cin);
	}
	int open(const char* name, ios_base::openmode mode) {
		input.open(name,mode);// = fopen(argv[1],"rt");
		if (!input)
			return -1;
		orig_cin = cin.rdbuf(input.rdbuf());
		cin.tie(0);
		return 0;
	}
	ifstream input;
	streambuf* orig_cin;
};
int main(int argc, char** argv)
{
	InputFixer inputFixer;
    if (argc>1)
    {
    	if (strcmp(argv[1], "test")==0) {
    	    CodeArray code;
    	    return 0;
    	}
		if (inputFixer.open(argv[1], ios::in))
			return -1;
    }

    nextChar();
	next();
    auto defs = parse_defs();

    if(defs.size()==0) {
        fprintf(stderr, "no parse\n");
        return 1;
    }

    Definition t; t.name = "sub"; t.args = { "left","right" };
    t.body = new ExprSub(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "add"; t.body = new ExprAdd(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "mul"; t.body = new ExprMul(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "div"; t.body = new ExprDiv(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "mod"; t.body = new ExprMod(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "__lt"; t.body = new ExprLt(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "__le"; t.body = new ExprLe(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "__gt"; t.body = new ExprGt(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "__ge"; t.body = new ExprGe(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "__eq"; t.body = new ExprEq(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    t.name = "__ne"; t.body = new ExprNe(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    pprint_defs(0, defs);
    Env env;
    CodeArray code;
    code.add(Instruction(STOP)); // If an instr needs to stop it simply jumps to zero.
    code.add(Instruction(UNWIND)); // If an instr needs to start an unwind (like RET) it jumps to one.

    try {
		for (auto def : defs) {
			EnvItem item;
			item.args = def.args.size();
			item.mode = AddressMode(new NFun(code.code.size(), item.args));
			env[def.name] = item;
			if (def.name=="main") {
				compileE(code,def.body,env,0);
			    code.add(Instruction(PRINT));
			    code.add(Instruction(STOP)); // jumping into main's body and then main falling through to print.
			} else {
				compileSc(code, def, env);
			}
		}
    }
    catch (int e) {
		cerr << " threw " << e << endl;
		return 1;
    }
    catch (const char* e) {
		cerr << " threw " << e << endl;
		return 1;
    }
    catch (const string& e) {
		cerr << " threw " << e << endl;
		return 1;
    }
    AddressMode mode;
    auto m = find_mode(env, "main", &mode);
    if (!m) {
		cerr << "main not found" << endl;
		return 1;
    }
    ptrdiff_t pc = mode.node->address; //code.code.size();
    for (unsigned i=0; i<code.code.size(); ++i) {
    	string id;
    	for (auto e : env) {
    		if (e.second.mode.node->address == i) {
    			id = e.first;
    			break;
    		}
    	}
		if (id.size()) cerr << id << ":" << endl;
		if (i == pc) cerr << "PC:" << endl;
		cerr << i <<": " << instructionToString(code.code[i]) << endl;
    }
    try {
		while (code.code[pc].ins != STOP) {
			step(code, pc);
			string id;
			for (auto e : env) {
				if (e.second.mode.node->address == pc) {
					id = e.first;
					break;
				}
			}
			if (id.size()) cerr << id << ":" << endl;
			cerr << "S:" << nodeStack.size()<<" D:"<< dump.size()<<" PC:"<< pc <<": " << instructionToString(code.code[pc]) << endl;
		}
    }
    catch (int e) {
		cerr << pc << ": " << instructionToString(code.code[pc-1]) << " threw " << e << endl;
    }
    catch (const char* e) {
		cerr << pc-1 << ": " << instructionToString(code.code[pc-1]) << " threw " << e << endl;
    }
    catch (const string& e) {
		cerr << pc-1 << ": " << instructionToString(code.code[pc-1]) << " threw " << e << endl;
    }

}
