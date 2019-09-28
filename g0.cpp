/*
 * g0.cpp
 *
 *  Created on: Sep 9, 2019
 *      Author: Mike
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

//#define MARK1 1
//#define MARK2 1
#define MARK3 1

struct Definition {
    string name;
    list<string> args;
    Expr* body;
};

Expr *parse_expr();

void pprint_expr(int col, const Expr *e);
Expr* parse_let();
Expr* E(int);
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

enum InstructionType {
#if 0
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
#endif
	MKAP,
#if 0
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
#endif
#ifndef MARK1
	POP,
#endif
	PUSH,
#if 0
	PUSHBASIC,
	PUSHBOOL,
	PUSHCHAR,
#endif
	PUSHGLOBAL,
	PUSHINT,
#if 0
	PUSHNIL,
	RETURN,
	SCALL,
	SEVAL,
#endif
#ifdef MARK1
	SLIDE,
#endif
#if 0
	SMKAP,
	SPRINT,
	SQUEEZE,
#endif
	STOP,
#if 0
	SUB,
	SUNWIND,
	TL,
#endif
	UNWIND, // not in SG
#ifndef MARK1
	UPDATE,
#endif
	zzzmaxInstr
};
const char* insNames[] = {
#if 0
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
#endif
		"MKAP",
#if 0
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
#endif
#ifndef MARK1
		"POP",
#endif
		"PUSH",
#if 0
		"PUSHBASIC",
		"PUSHBOOL",
		"PUSHCHAR",
#endif
		"PUSHGLOBAL",
		"PUSHINT",
#if 0
		"PUSHNIL",
		"RETURN",
		"SCALL",
		"SEVAL",
#endif
#ifdef MARK1
		"SLIDE",
#endif
#if 0
		"SMKAP",
		"SPRINT",
		"SQUEEZE",
#endif
		"STOP",
#if 0
		"SUB",
		"SUNWIND",
		"TL",
#endif
		"UNWIND", // not in stg
#ifndef MARK1
		"UPDATE"
#endif
};
const char* insToString(InstructionType ins) {
	if (0<=ins && ins<zzzmaxInstr)
		return insNames[ins];
	return "inxxx";
}
struct NGlobal;
struct NInt;
struct NAp;
#ifndef MARK1
struct NInd;
#endif
struct NodeVisitor {
	virtual ~NodeVisitor() {}
	virtual void visitNGlobal(NGlobal* n) = 0;
	virtual void visitNInt(NInt* n) = 0;
	virtual void visitNAp(NAp* n) = 0;
#ifndef MARK1
	virtual void visitNInd(NInd* n) = 0;
#endif
};
struct Node {
	virtual ~Node() {}
	virtual string to_string() const = 0;
	virtual void visit(NodeVisitor* v) = 0;
};
struct NGlobal : public Node {
	NGlobal(ptrdiff_t address, unsigned args) : address(address), args(args) {}
	string to_string() const {
		return "NG " + ::to_string(address);
	}
	void visit(NodeVisitor* v) { v->visitNGlobal(this); }
	ptrdiff_t address;
	unsigned args;
};
struct NInt : public Node {
	NInt(int i) : i(i) {}
	string to_string() const {
		return "NInt " + ::to_string(i);
	}
	void visit(NodeVisitor* v) { v->visitNInt(this); }
	int i;
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
#ifndef MARK1
struct NInd : public Node {
	NInd(Node* a) : a(a) {}
	string to_string() const {
		return "&"+a->to_string();
	}
	void visit(NodeVisitor* v) { v->visitNInd(this); }
	Node* a;
};
#endif
struct Instruction {
	InstructionType ins;
	union {
		unsigned n;
		int i;
		bool b;
	};
	ptrdiff_t dest;
	NGlobal* node;
};
struct CodeArray {
	vector<Instruction> code;
	void add(const Instruction& i) {
		code.push_back(i);
	}
};
struct AddressMode {
	enum Mode { Local, Global };
	Mode mode;
	union { unsigned localIndex; /*ptrdiff_t address;*/ NGlobal* node; };
	AddressMode(NGlobal* node) : mode(Global), node(node) {}
	AddressMode() : mode(Local) {}
};
InstructionType needed[] = { MKAP, PUSH, PUSHGLOBAL, PUSHINT,
#ifndef MARK1
		POP, UPDATE,
#endif
#ifdef MARK1
		SLIDE,
#endif
		STOP,UNWIND,  };
string instructionToString(Instruction* ins) {
	string rv = insToString(ins->ins);
	switch (ins->ins) {
	case PUSHGLOBAL: rv += " " + ::to_string(ins->node->address); break;
	case PUSHINT: rv += " " + ::to_string(ins->i); break;
	case PUSH:
#ifndef MARK1
	case UPDATE:
#endif
#ifdef MARK1
	case SLIDE:
#endif
#ifndef MARK1
	case POP:
#endif
		rv += " " + ::to_string(ins->n); break;
	case UNWIND:
	case STOP:
		break;
	default:
		break;
	}
	return rv;
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
                //cout << "env "<<i.first <<" entry "<<i.second << flush;/*, env, entry); fflush(stdout);*/
                //printf(" %s args %d mode %s\n", entry->name, entry->args, amToString(entry->mode.mode));
                cout << i.first << " args " << i.second.args << ' ' << amToString(i.second.mode) << endl;
        }
}

typedef list<Node*> GmStack;
GmStack gmStack;
struct GmStats {

};
void showStack(const string& label) {
	cout << label << endl;
	for (auto s : gmStack) {
		cout << /*s << ' ' <<*/ '[' << s->to_string() << "] ";
	}
	cout << endl;
}
void stepPushGlobal(NGlobal* sc) {
	showStack("Stack before pushGlobal");
	gmStack.push_front(sc);
	showStack("Stack after pushGlobal");
}
void stepPushInt(int i) {
	showStack("Stack before pushInt");
	gmStack.push_front(new NInt(i));
	showStack("Stack after pushInt");
}
void stepMkAp() {
	showStack("Stack before mkap");
	Node* a1 = gmStack.front(); gmStack.pop_front();
	Node* a2 = gmStack.front(); gmStack.pop_front();
	gmStack.push_front(new NAp(a1, a2));
	showStack("Stack after mkap");
}
#if MARK1 || MARK2
void stepPush(unsigned n) {
	showStack("Stack before push");
	unsigned i=0;
	for (const auto& se : gmStack) {
		cout << i << ": " << se << ' ' << se->to_string() << endl;
		++i;
	}
	auto p = gmStack.begin();
	advance(p,n+1);
	Node* node = *p;
	auto ap = dynamic_cast<NAp*>(node);
	assert(ap);
	auto arg = ap->a2;
	gmStack.push_front(arg);
	cout << "Stack after push " << endl;
	i=0;
	for (const auto& se : gmStack) {
		cout << i << ": " << se << ' ' << se->to_string() << endl;
		++i;
	}
}
#endif
#if MARK3
void stepPush(unsigned n) {
	showStack("Stack before push");
	auto p = gmStack.begin();
	advance(p,n+1);
	Node* arg = *p;
	gmStack.push_front(arg);
	showStack("Stack after push");
}
#endif
#ifdef MARK1
void stepSlide(int n) {
	showStack("stack before slide "+::to_string(n));
	auto a0 = gmStack.front();
	gmStack.pop_front();
	for (int i=1; i<=n; ++i)
		gmStack.pop_front();
	gmStack.push_front(a0);
	showStack("Stack after slide");
}
#endif
#if defined(MARK2) || defined(MARK3)
void stepPop(unsigned n) {
	showStack("stack before pop "+::to_string(n));
	for (unsigned i=1; i<=n; ++i)
		gmStack.pop_front();
	showStack("Stack after pop");
}
void stepUpdate(unsigned n) {
	//if (gmStack.size()<=(n+1)) {
	//	cout << "Judgment " << gmStack.size() << " vs " << (n+1) << " is " << (gmStack.size()>(n+1)) << " Stack size " << gmStack.size() << " not big enough for update " << n << endl;
	//	showStack("");
	//	throw 1;
	//}
	showStack("stack before update "+::to_string(n));
	Node* tos = gmStack.front(); gmStack.pop_front();
	showStack("stack during update "+::to_string(n));
	auto p = gmStack.begin();
	advance(p, n);
	cout << "distance(gmStack.begin(),p) " << distance(gmStack.begin(),p)
		<< " gmStack.size() " << gmStack.size() << endl;
	//assert((unsigned)distance(gmStack.begin(),p) < gmStack.size());
	*p = new NInd(tos);
	showStack("Stack after update");
}
#endif
#if defined(MARK1) || defined(MARK2) || defined(MARK3)
struct UnwindNodeVisitor : public NodeVisitor {
	UnwindNodeVisitor(ptrdiff_t& pc) : pc(pc),done(false) {}
	ptrdiff_t& pc;
	bool done;
	void visitNInt(NInt*) {
		pc = 0;
		done = true;
	}
#if defined(MARK2) || defined(MARK3)
	void visitNInd(NInd* iitop) {
		Node* replacement = iitop->a;
		gmStack.front() = replacement;
		showStack("Stack during ap unwind");
	}
#endif
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
		list<Node*> r;
		auto px = next(x.begin(),d);
		while (px != x.end()) {
			r.push_back(*px++);
		}

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
	void visitNGlobal(NGlobal* gtop) {
#if defined(MARK1) || defined(MARK2)
		if (gmStack.size() < gtop->args) {
			cout << __PRETTY_FUNCTION__ << ": stack " << gmStack.size() << " not enough for " << gtop->args << " arguments" << endl;
		}
		pc = gtop->address;
		done = true;
#else
		showStack("Stack during ap unwind beginning with a Global node");
		if (gmStack.size() < gtop->args) {
			cout << __PRETTY_FUNCTION__ << ": stack " << gmStack.size() << " not enough for " << gtop->args << " arguments" << endl;
		}
		gmStack.pop_front();
#if 1
		if (gtop->args)
			gmStack = rearrange(gtop->args, gmStack);
#else
		list<Node*> args;
		for (unsigned i=0; i<gtop->args; ++i) {
			auto argNode = dynamic_cast<NAp*>(gmStack.front()); gmStack.pop_front();
			if (!argNode) {
				cout<< __PRETTY_FUNCTION__  << "Argument node from spine is not NAp" << endl;
				throw 1;
			}
			args.push_back(argNode->a2);
		}
		for (auto p = args.rbegin(); p != args.rend(); ++p) {
			gmStack.push_front(*p);
		}
#endif
		pc = gtop->address;
		done = true;
		showStack("Stack during ap unwind");
#endif
	}
	void visitNAp(NAp* aptop) {
		cout << "aptop " << aptop->to_string() << endl;
		cout << "aptop.a1 " << aptop->a1->to_string() << " aptop.a2 " << aptop->a2->to_string() << endl;
		gmStack.push_front(aptop->a1);
		showStack("Stack during ap unwind");
		//pc--; // instead of backing up, we reiterate directly.
	}
};
#endif
// Unwind will cause a jump if the top node is an NGlobal.
void stepUnwind(ptrdiff_t& pc) {
	showStack("Stack before unwind");
	UnwindNodeVisitor visitor(pc);
	while (!visitor.done) {
		Node* top = gmStack.front();
		cout << "Unwind viewing " << top->to_string() << " from top of stack of size " << gmStack.size() << endl;
		top->visit(&visitor);
	}
	showStack("Stack after unwind");
}
bool step(CodeArray& code, ptrdiff_t& pc) {
	Instruction instr = code.code[pc++];
	switch (instr.ins) {
	case PUSHGLOBAL: stepPushGlobal(instr.node); break;
	case PUSHINT: stepPushInt(instr.i); break;
	case MKAP: stepMkAp(); break;
	case PUSH: stepPush(instr.n); break;
#ifdef MARK1
	case SLIDE: stepSlide(instr.n); break;
#endif
#if defined(MARK2) || defined(MARK3)
	case POP: stepPop(instr.n); break;
	case UPDATE: stepUpdate(instr.n); break;
#endif
	case UNWIND: stepUnwind(pc); break;
	case STOP: break;
	case zzzmaxInstr: break; // dummy value
	}
	return instr.ins != STOP;
}
#if 0
// Assignment operator does this
static
Env envDup(const Env& env) {
	Env dup = env;
    //printf("envDup "); pprint_env(dup);
	return dup;
}
#endif
// Shift the locals when there has been a push
// that affects the pop/push distance to an argument.
Env envShift(const Env& env, int sh) {
	Env shift;
	for (auto s: env) {
		EnvItem e = s.second;
		if (e.mode.mode==AddressMode::Local)
			e.mode.localIndex+=sh;
		shift[s.first]=e;
	}
	return shift;
}
static
void envAddArgs(Env& env, const list<string> args) {
	int kArg = 0;
	for (auto arg : args) {
        EnvItem entry;
        //entry.name = arg;
        entry.mode.mode = AddressMode::Local;
        entry.mode.localIndex = kArg;
        kArg++;
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

#ifdef MARK1
Instruction SlideInstruction(unsigned n) {
	Instruction ins;
	ins.ins = SLIDE;
	ins.n = n;
	return ins;
}
#else
Instruction PopInstruction(unsigned n) {
	Instruction ins;
	ins.ins = POP;
	ins.n = n;
	return ins;
}
Instruction UpdateInstruction(unsigned n) {
	Instruction ins;
	ins.ins = UPDATE;
	ins.n = n;
	return ins;
}
#endif
Instruction UnwindInstruction() {
	Instruction ins;
	ins.ins = UNWIND;
	return ins;
}
Instruction PushIntInstruction(ptrdiff_t v) {
	Instruction ins;
	ins.ins = PUSHINT;
	ins.i = v;
	return ins;
}
Instruction PushInstruction(unsigned local) {
	Instruction ins;
	ins.ins = PUSH;
	ins.n = local;
	return ins;
}
Instruction PushGlobalInstruction(NGlobal* node) {
	Instruction ins;
	ins.ins = PUSHGLOBAL;
	ins.node = node;
	return ins;
}
Instruction MkapInstruction() {
	Instruction ins;
	ins.ins = MKAP;
	return ins;
}
void compileC(CodeArray& code, Expr* expr, Env& env);
struct CompileCVisitor : public ExprVisitor {
	CompileCVisitor(CodeArray& code, Env& env) : code(code), env(env) {}
	CodeArray& code;
	Env& env;
	void visitExprNum(ExprNum* eint) {
		code.add(PushIntInstruction(eint->value));
	}
	void visitExprVar(ExprVar* evar) {
		auto pm = env.find(evar->var);
		if (pm != env.end()) {
			switch (pm->second.mode.mode) {
			case AddressMode::Local: code.add(PushInstruction(pm->second.mode.localIndex)); break;
			case AddressMode::Global: code.add(PushGlobalInstruction(pm->second.mode.node)); break;
			}
			return;
		}
	}
	void visitExprApp(ExprApp* eapp) {
#ifdef BIG_LIST_AP
		int shiftCount = 0;
		for (auto pArg = eapp->args.rbegin(); pArg!=eapp->args.rend(); ++pArg) {
			if (shiftCount == 0) {
				compileC(code,*pArg,env);
			} else {
				Env shift = envShift(env, shiftCount);
				compileC(code,*pArg,shift);
			}
			shiftCount++;
		}
		Env shift = envShift(env, shiftCount);
		compileC(code,eapp->fun,shift);
		for (unsigned a=0; a<eapp->args.size(); ++a)
			code.add(MkapInstruction());
#else
		compileC(code,eapp->arg,env);
		Env shift = envShift(env, 1);
		cout << "Providing modified arg environment" << endl; pprint_env(shift);
		compileC(code,eapp->fun,shift);
		code.add(MkapInstruction());
#endif
		return;
	}
	void visitExprLet(ExprLet*) {}
	void visitExprTl(ExprTl*){}
	void visitExprSub(ExprSub*){}
	void visitExprStr(ExprStr*){}
	void visitExprNull(ExprNull*){}
	void visitExprNot(ExprNot*){}
	void visitExprNil(ExprNil*){}
	void visitExprNeg(ExprNeg*){}
	void visitExprNe(ExprNe*){}
	void visitExprMul(ExprMul*){}
	void visitExprMod(ExprMod*){}
	void visitExprLt(ExprLt*){}
	void visitExprLe(ExprLe*){}
	void visitExprIf(ExprIf*){}
	void visitExprHd(ExprHd*){}
	void visitExprGt(ExprGt*){}
	void visitExprGe(ExprGe*){}
	void visitExprEq(ExprEq*){}
	void visitExprDiv(ExprDiv*){}
	void visitExprCons(ExprCons*){}
	void visitExprChar(ExprChar*){}
	void visitExprBool(ExprBool*){}
	void visitExprAdd(ExprAdd*){}
};
void compileC(CodeArray& code, Expr* expr, Env& env) {
	CompileCVisitor visitor(code, env);
	expr->visit(&visitor);
}
void compileR(CodeArray& code, Expr* expr, size_t args, Env& env) {
	compileC(code,expr,env);
#ifdef MARK1
	code.add(SlideInstruction(args+1));
#else
	code.add(UpdateInstruction(args));
	code.add(PopInstruction(args));
#endif
	code.add(UnwindInstruction());
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

    pprint_defs(0, defs);
    Env env;
    CodeArray code;
    Instruction instr; instr.ins = STOP;
    code.add(instr);

    for (auto def : defs) {
    	EnvItem item;
		item.args = def.args.size();
		item.mode = AddressMode(new NGlobal(code.code.size(), item.args));
		env[def.name] = item;
		compileSc(code, def, env);
    }
    AddressMode mode;
    auto m = find_mode(env, "main", &mode);
    if (!m) {
		cout << "main not found" << endl;
		return 1;
    }
    ptrdiff_t pc = code.code.size();
    code.add(PushGlobalInstruction(mode.node));
#if !defined(MARK1)
    //code.add(UpdateInstruction(0));
    //code.add(PopInstruction(0));
    // maybe the above are not good for zero parameters?
#endif
    code.add(UnwindInstruction());
    for (unsigned i=0; i<code.code.size(); ++i) {
    	string id;
    	for (auto e : env) {
    		if (e.second.mode.node->address == i) {
    			id = e.first;
    			break;
    		}
    	}
    	if (id.size()) cout << id << ":" << endl;
    	if (i == pc) cout << "PC:" << endl;
		cout << i <<": " << instructionToString(&code.code[i]) << endl;
    }
    while (code.code[pc].ins != STOP) {
    	step(code, pc);
    	string id;
    	for (auto e : env) {
    		if (e.second.mode.node->address == pc) {
    			id = e.first;
    			break;
    		}
    	}
    	if (id.size()) cout << id << ":" << endl;
		cout << pc <<": " << instructionToString(&code.code[pc]) << endl;
    }
}

