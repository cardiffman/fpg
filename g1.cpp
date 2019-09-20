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

using namespace std;

template <typename T, typename UnaryOp, typename T2=std::string, typename C2=std::list<T2>>
C2 mapf(const T b, const T e, UnaryOp f)
{
	C2 out;
	std::transform(b, e, back_inserter(out), f);
	return out;
}
template <typename C1, typename UnaryOp, typename T2=std::string, typename C2=C1>
C2 mapf(const C1 in, UnaryOp f)
{
	C2 out;
	std::transform(in.begin(), in.end(), back_inserter(out), f);
	return out;
}
template <typename T, typename UnaryOp, typename T2=std::string, typename C2=std::list<T2>>
C2 mapf(const T* b, const T*e, UnaryOp f)
{
	C2 out;
	std::transform(b, e, back_inserter(out), f);
	return out;
}
template <typename T, typename UnaryOp, typename T2=std::string, typename T3=unsigned, typename C2=std::list<T2>>
C2 mapf(const T* a, const T3& n, UnaryOp f)
{
	C2 out;
	std::transform(a, a+n, back_inserter(out), f);
	return out;
}
template <typename T, typename UnaryOp, typename T2=std::string, typename T3=unsigned, typename C2=std::list<T2>>
C2 mapfl(const T* a, const T3& n, UnaryOp f)
{
	C2 out;
	for (T3 i=0; i!=n; ++i) {
		out.push_back(f(a[i],i));
	}
	return out;
}
template <typename BinaryOp>
string fold(list<string>& strings, string base, BinaryOp op)
{
	list<string>::const_iterator ps = strings.begin();
	string out = op(base,*ps++);
	while (ps != strings.end()) {
		out = op(out,*ps++);
	}
	return out;
}
template <typename C1, typename C2, typename BinaryOp>
C2 fold(C1& strings, C2 base, BinaryOp op)
{
	typename C1::const_iterator ps = strings.begin();
	C2 out = op(base,*ps++);
	while (ps != strings.end()) {
		out = op(out,*ps++);
	}
	return out;
}
void f() {
	list<int> ints;
	fold(ints, 0, [](int a,int b){ return a+b; });
}
string join(const list<string>& strs, int joint) {
	string r;
	auto p = strs.begin();
	r = *p++;
	for (; p!=strs.end(); ++p) {
		r+= string(1,joint);
		r+= *p;
	}
	return r;
}

extern string indent(int col);
struct ExprVar;
struct ExprNum;
struct ExprApp;
struct ExprLet;
struct ExprBool;
struct ExprStr;
struct ExprNil;
struct ExprIf;
struct ExprChar;
struct ExprCons;
struct ExprHd;
struct ExprTl;
struct ExprNull;
struct ExprOper;
struct ExprAdd;
struct ExprSub;
struct ExprMul;
struct ExprDiv;
struct ExprMod;
struct ExprEq;
struct ExprNe;
struct ExprLt;
struct ExprGt;
struct ExprLe;
struct ExprGe;
struct ExprNeg;
struct ExprVisitor {
	virtual ~ExprVisitor() {}
	virtual void visitExprVar(ExprVar* e) = 0;
	virtual void visitExprNum(ExprNum* e) = 0;
	virtual void visitExprApp(ExprApp* e) = 0;
	virtual void visitExprLet(ExprLet* e) = 0;
	virtual void visitExprBool(ExprBool* e) = 0;
	virtual void visitExprChar(ExprChar* e) = 0;
	virtual void visitExprStr(ExprStr* e) = 0;
	virtual void visitExprNil(ExprNil* e) = 0;
	virtual void visitExprIf(ExprIf* e) = 0;
	virtual void visitExprCons(ExprCons* e) = 0;
	virtual void visitExprHd(ExprHd* e) = 0;
	virtual void visitExprTl(ExprTl* e) = 0;
	virtual void visitExprNull(ExprNull* e) = 0;
	virtual void visitExprOper(ExprOper* e) = 0;
	virtual void visitExprAdd(ExprAdd* e) = 0;
	virtual void visitExprSub(ExprSub* e) = 0;
	virtual void visitExprMul(ExprMul* e) = 0;
	virtual void visitExprDiv(ExprDiv* e) = 0;
	virtual void visitExprMod(ExprMod* e) = 0;
	virtual void visitExprEq(ExprEq* e) = 0;
	virtual void visitExprNe(ExprNe* e) = 0;
	virtual void visitExprLt(ExprLt* e) = 0;
	virtual void visitExprGt(ExprGt* e) = 0;
	virtual void visitExprLe(ExprLe* e) = 0;
	virtual void visitExprGe(ExprGe* e) = 0;
	virtual void visitExprNeg(ExprNeg* e) = 0;
};

struct Expr {
	Expr() {}
	virtual ~Expr() {}
	virtual string to_string(int) const = 0;
	virtual void visit(ExprVisitor*) = 0;
};
struct ExprVar : public Expr {
	ExprVar(const string& v) : var(v) {}
	string to_string(int) const { return "VAR " + var; }
	void visit(ExprVisitor* v) { v->visitExprVar(this); }
	string var;
};
#define BIG_LIST_AP
/* BIG_LIST_AP is for the alternative of representing an application of a series of atoms
 * using an NAp node with one function and a list of arguments instead of as a chain of
 * NAp nodes with only one argument.
 */
struct ExprApp : public Expr {
#ifdef BIG_LIST_AP
	ExprApp(Expr* fun, Expr* arg) : fun(fun) { args.push_back(arg); }
#else
	ExprApp(Expr* fun, Expr* arg) : fun(fun),arg(arg) { }
#endif
	Expr* fun;
#ifdef BIG_LIST_AP
	list<Expr*> args;
#else
	Expr* arg;
#endif
	string to_string(int) const;
	void visit(ExprVisitor* v) { v->visitExprApp(this); }
};
string ExprApp::to_string(int col) const {
	string rv = "APP " + fun->to_string(col);
	rv += " ARGS ";
#ifdef BIG_LIST_AP
	auto argstrs = mapf(args.begin(), args.end(), [col](const Expr* ex) { return ex->to_string(col); });
	if (argstrs.size()) {
		rv += '\n';
		rv += string(col,' ');
	}
	rv += join(argstrs, ' ');
#else
	rv += arg->to_string(col);
#endif
	return rv;
}
struct ExprNum : public Expr {
	ExprNum(ptrdiff_t value) : value(value) {}
	ptrdiff_t value;
	string to_string(int) const { return "NUM " + ::to_string((int)value); }
	void visit(ExprVisitor* v) { v->visitExprNum(this); }
};
#if !defined(MARK1) && !defined(MARK2) && !defined(MARK3)
struct ExprBool : public Expr {
	ExprBool(bool v) : value(v) {}
	string to_string(int) const { return value?"true":"false"; }
	void visit(ExprVisitor* v) { v->visitExprBool(this); }
	bool value;
};
struct ExprStr : public Expr {
	ExprStr(const std::string str): str(str) {}
	string str;
	string to_string(int) const { return "STR \"" + str + '"'; }
	void visit(ExprVisitor* v) { v->visitExprStr(this); }
};
struct ExprNull : public Expr {
	ExprNull(Expr* subject) : subject(subject) {}
	string to_string(int c) const { return "NULLP "+subject->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprNull(this); }
	Expr* subject;
};
struct ExprHd : public Expr {
	ExprHd(Expr* subject) : subject(subject) {}
	string to_string(int c) const { return "HD "+subject->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprHd(this); }
	Expr* subject;
};
struct ExprTl : public Expr {
	ExprTl(Expr* subject) : subject(subject) {}
	string to_string(int c) const { return "Tl "+subject->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprTl(this); }
	Expr* subject;
};
struct ExprAdd : public Expr {
	ExprAdd(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "ADD "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprAdd(this); }
	Expr* left;
	Expr* right;
};
struct ExprSub : public Expr {
	ExprSub(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "SUB "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprSub(this); }
	Expr* left;
	Expr* right;
};
struct ExprMul : public Expr {
	ExprMul(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "MUL "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprMul(this); }
	Expr* left;
	Expr* right;
};
struct ExprDiv : public Expr {
	ExprDiv(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "DIV "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprDiv(this); }
	Expr* left;
	Expr* right;
};
struct ExprMod : public Expr {
	ExprMod(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "MOD "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprMod(this); }
	Expr* left;
	Expr* right;
};
struct ExprLt : public Expr {
	ExprLt(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "LT "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprLt(this); }
	Expr* left;
	Expr* right;
};
struct ExprLe : public Expr {
	ExprLe(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "LE "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprLe(this); }
	Expr* left;
	Expr* right;
};
struct ExprEq : public Expr {
	ExprEq(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "EQ "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprEq(this); }
	Expr* left;
	Expr* right;
};
struct ExprNe : public Expr {
	ExprNe(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "NE "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprNe(this); }
	Expr* left;
	Expr* right;
};
struct ExprGt : public Expr {
	ExprGt(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "GT "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprGt(this); }
	Expr* left;
	Expr* right;
};
struct ExprGe : public Expr {
	ExprGe(Expr* left, Expr* right) : left(left), right(right) {}
	string to_string(int c) const { return "GE "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprGe(this); }
	Expr* left;
	Expr* right;
};
struct ExprNeg : public Expr {
	ExprNeg(Expr* subj) : subj(subj) {}
	string to_string(int c) const { return "NEG "+subj->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprNeg(this); }
	Expr* subj;
};
struct ExprIf : public Expr {
	ExprIf(Expr* cond, Expr* trueExpr, Expr* falseExpr)
	: cond(cond), trueExpr(trueExpr), falseExpr(falseExpr)
	{}
	string to_string(int c) const;
	void visit(ExprVisitor* v) { v->visitExprIf(this); }
	Expr* cond;
	Expr* trueExpr;
	Expr* falseExpr;
};
string ExprIf::to_string(int c) const {
	return "IF " + cond->to_string(c) + " " + trueExpr->to_string(c) + " "+ falseExpr->to_string(c);
}
struct ExprNil : public Expr {
	ExprNil()
	{}
	string to_string(int) const { return "nil"; }
	void visit(ExprVisitor* v) { v->visitExprNil(this); }
};
struct ExprChar : public Expr {
	ExprChar(int ch) : ch(ch)
	{}
	string to_string(int) const { return "ch("+::to_string(ch)+")"; }
	void visit(ExprVisitor* v) { v->visitExprChar(this); }
	int ch;
};
struct ExprCons : public Expr {
	ExprCons(Expr* hd, Expr* tl) : hd(hd), tl(tl) {}
	Expr* hd;
	Expr* tl;
	string to_string(int) const { return "cons("+hd->to_string(0)+","+tl->to_string(0)+")"; }
	void visit(ExprVisitor* v) { v->visitExprCons(this); }
};
#endif
struct ExprLet : public Expr {
	struct Binding {
		string name;
		Expr* value;
	};
	string to_string(int col) const {
		return "LET "
				+ join(mapf(bindings.begin(), bindings.end(), [col](const Binding& x){ return x.name+"="+x.value->to_string(col);}), ' ')
				+ " IN "
				+ value->to_string(col);
	}
	void visit(ExprVisitor* v) { v->visitExprLet(this); }
	list<Binding> bindings;
	Expr* value;
};
struct Definition {
    string name;
    list<string> args;
    Expr* body;
};

enum Token {
	T_NAME,
	T_NUM,
	T_CHAR,
	T_STR,
	T_PUNCT,
	T_SEMI,
	T_EQUALS,
	T_LPAREN,
	T_RPAREN,
	T_LT,
	T_GT,
	T_LE,
	T_GE,
	T_EQ,
	T_NE,
	T_OR,
	T_AND,
	T_ADD,
	T_SUB,
	T_MUL,
	T_DIV,
	T_MOD,
	T_LET,
	T_IN,
	T_IF,
	T_NULL,
	T_NIL,
	T_CONS,
	T_HD,
	T_TL,
	T_CHR,
	T_TRUE,
	T_FALSE,
	T_EOF
};
struct tkn {
	Token type;
	string s;
	int value;
};
tkn token;
int ch;
void nextChar()
{
	ch = cin.get();
}
void next() {
	while (true){
		while (ch == ' ' || ch=='\t' || ch=='\n' || ch == '\r')
			nextChar();
		if (ch=='#') {
			while (ch != '\n' && ch != EOF)
				nextChar();
			if (ch == '\n') {
				continue;
			} else {
				break;
			}
		}
		break;
	}
	if (ch==EOF) {
		token.type = T_EOF;
		return;
	}
	switch (ch) {
	case ';': token.type = T_SEMI; nextChar(); return;
	case '(': token.type = T_LPAREN; nextChar(); return;
	case ')': token.type = T_RPAREN; nextChar(); return;
	case '\'': token.type = T_CHAR; nextChar(); token.value = ch; nextChar(); assert(ch=='\''); nextChar(); return;
	}
	token = tkn();
	if (ch=='"') {
		nextChar();
		while (ch != '"') {
			token.s.append(1,ch);
			nextChar();
		}
		nextChar();
		token.type = T_STR;
		return;
	}
	if (isalpha(ch)) {
		while (isalnum(ch) || ch=='_' || ch=='\'')
		{
			token.s.append(1,ch);
			nextChar();
		}
		token.type = T_NAME;
		if (token.s == "let")
			token.type = T_LET;
		else if (token.s == "in")
			token.type = T_IN;
		else if (token.s == "if")
			token.type = T_IF;
		else if (token.s == "cons")
			token.type = T_CONS;
		else if (token.s == "hd")
			token.type = T_HD;
		else if (token.s == "tl")
			token.type = T_TL;
		else if (token.s == "nil")
			token.type = T_NIL;
		else if (token.s == "null")
			token.type = T_NULL;
		else if (token.s == "chr")
			token.type = T_CHR;
		else if (token.s == "true")
			token.type = T_TRUE;
		else if (token.s == "false")
			token.type = T_FALSE;
		return;
	}
	if (isdigit(ch)) {
		token.value = ch - '0';
		nextChar();
		while (isdigit(ch)) {
			token.value = token.value*10 + ch-'0';
			nextChar();
		}
		token.type = T_NUM;
		return;
	}

	while (ch && !isalnum(ch) && ch!=';' &&ch !=')' && ch!='(' && ch!=' ' && ch!='\t' && ch!='\n' && ch!='#') {
		token.s.append(1,ch);
		nextChar();
	}
	token.type = T_PUNCT;
	switch (token.s.length()) {
	case 1: // 1-character sequence
		switch (token.s[0]){
		case '&': token.type = T_AND; break;
		case '|': token.type = T_OR; break;
		case '<': token.type = T_LT; break;
		case '>': token.type = T_GT; break;
		case '=': token.type = T_EQUALS; break;
		case '+': token.type = T_ADD; break;
		case '-': token.type = T_SUB; break;
		case '*': token.type = T_MUL; break;
		case '/': token.type = T_DIV; break;
		case '%': token.type = T_MOD; break;
		}
		break;
	case 2: // 2-character sequence
		if (token.s=="<=")
			token.type = T_LE;
		else if (token.s==">=")
			token.type = T_GE;
		else if (token.s=="!=")
			token.type = T_NE;
		else if (token.s=="==")
			token.type = T_EQ;
		break;
	}
	return;
}
string token_to_string(const tkn& token) {
	string rv;
	switch (token.type) {
	case T_NAME: rv = "T_NAME: "+ token.s; break;
	case T_PUNCT: rv = "T_PUNCT: "+ token.s; break;
	case T_NUM: rv = "T_NUM: " + to_string(token.value); break;
	case T_SEMI: rv = "T_SEMI"; break;
	case T_LPAREN: rv = "T_LPAREN"; break;
	case T_RPAREN: rv = "T_RPAREN"; break;
	case T_LT: rv = "T_LT"; break;
	case T_MUL: rv = "T_MUL"; break;
	case T_SUB: rv = "T_SUB"; break;
	case T_ADD: rv = "T_ADD"; break;
	case T_DIV: rv = "T_DIV"; break;
	case T_MOD: rv = "T_MOD"; break;
	case T_IN: rv = "T_IN"; break;
	case T_LET: rv = "T_LET"; break;
	default: rv = to_string((int)token.type); break;
	}
	return rv;
}
Expr *parse_expr();

void pprint_expr(int col, const Expr *e);
Expr* mkleaf(tkn t) {
	switch (t.type) {
	case T_STR: return new ExprStr(t.s);
	case T_CHAR: return new ExprChar(t.value);
	case T_NIL: return new ExprNil();
	case T_TRUE: return new ExprBool(true);
	case T_FALSE: return new ExprBool(false);
	case T_NAME: return new ExprVar(t.s);
	case T_NUM: return new ExprNum(t.value);
	default: return NULL;
	}
	return NULL;
}
Expr* mkop(Token t, Expr* left, Expr* right) {
	switch (t) {
	case T_ADD: return new ExprAdd(left,right);
	case T_SUB: return new ExprSub(left,right);
	case T_MUL: return new ExprMul(left,right);
	case T_DIV: return new ExprDiv(left,right);
	case T_MOD: return new ExprMod(left,right);
	case T_LT: return new ExprLt(left,right);
	default:
		throw "unexpected binary operator token";
	}
	//ExprOper* r = new ExprOper(t);
	//return r;
}
Expr* mkop(Token t, Expr* subj) {
	switch (t) {
	case T_SUB: return new ExprNeg(subj);
	default:
		throw "unexpected unary operator token";
	}
	//ExprOper* r = new ExprOper(t);
	//return r;
}
Expr* mkapp(Expr* fun_, Expr* arg) {
#ifdef BIG_LIST_AP
	ExprApp* fun = dynamic_cast<ExprApp*>(fun_);
	if (fun) {
		fun->args.push_back(arg);
		return fun;
	}
#endif
	ExprApp* r = new ExprApp(fun_, arg);
	return r;
}
bool atom(tkn t) {
	switch (t.type) {
	case T_NAME:
	case T_NUM:
	case T_CHAR:
	case T_HD:
	case T_TL:
	case T_CONS:
	case T_NIL:
	case T_NULL:
	case T_STR:
	case T_TRUE:
	case T_FALSE:
	case T_IF: // a bit of a hack
		return true;
	default: break;
	}
	return false;
}
bool binop(Token t) {
	switch (t) {
	case T_OR:
	case T_AND:
	case T_ADD:
	case T_SUB:
	case T_MUL:
	case T_DIV:
	case T_MOD:
	case T_LT:
		return true;
	default: break;
	}
	return false;
}
bool postfix(Token t) {
	(void)t;
	return false; // don't have these yet.
}
int precedence(Token t) {
	switch (t){
	case T_OR:
		return 2;
	case T_AND:
		return 3;
	case T_LT:
		return 4;
	case T_ADD: case T_SUB:
		return 5;
	case T_DIV: case T_MOD:
		return 6;
	case T_MUL:
		return 7;
	default: break;
	}
	return 1;
}
int rightPrec(Token t) {
	switch (t){
	case T_OR:
		return 3;
	case T_AND:
		return 4;
	case T_LT:
		return 5;
	case T_ADD: case T_SUB:
		return 6;
	case T_DIV:
		return 7;
	case T_MUL:
		return 8;
	default: break;
	}
	return 1;
}
int nextPrec(Token t) {
	switch (t){
	case T_OR:
		return 1;
	case T_AND:
		return 2;
	case T_LT:
		return 3;
	case T_ADD: case T_SUB:
		return 5;
	case T_DIV:
		return 6;
	case T_MUL:
		return 7;
	default: break;
	}
	return -1;
}
Expr* P(void);
Expr* parse_conditional();
Expr* E(int p) {
	Expr* t = P();
	while (atom(token) || token.type == T_LPAREN) {
		Expr* arg = P();
		//cout << __PRETTY_FUNCTION__ << " argument: " << arg->to_string(0) << endl;
		t = mkapp(t, arg);
		//cout << __PRETTY_FUNCTION__ << " application: " << t->to_string(0) << endl;
	}


	int r = 8;
	//printf("E(%d): %s binop %d postfix %d precedence() %d, p <=precedence() %d precedence() <= r %d r=%d\n", p, token_to_string(&token), binop(token.type), postfix(token), precedence(token.type), p <= precedence(token.type), precedence(token.type)<=r, r);
	while ((binop(token.type) || postfix(token.type)) && ((p <= precedence(token.type)) && (precedence(token.type) <= r))) {
		Token b = token.type;
		next();
		if (binop(b)) {
			Expr* t1 = E(rightPrec(b));
			//printf("RHS expr: "); pprint_expr(0, t1);
			//t = mkop(b, t, t1);
			//pprint_expr(0, t);
			switch(b) {
			case T_ADD: t = new ExprAdd(t,t1); break;
			case T_SUB: t = new ExprSub(t,t1); break;
			case T_MUL: t = new ExprMul(t,t1); break;
			case T_DIV: t = new ExprDiv(t,t1); break;
			case T_MOD: t = new ExprMod(t,t1); break;
			case T_GE: t = new ExprGe(t,t1); break;
			case T_GT: t = new ExprGt(t,t1); break;
			case T_LE: t = new ExprLe(t,t1); break;
			case T_LT: t = new ExprLt(t,t1); break;
			case T_NE: t = new ExprNe(t,t1); break;
			case T_EQ: t = new ExprEq(t,t1); break;
			default:
				throw "Unknown binary operator";
			}
		} else {
			switch (b){
			case T_SUB: t=new ExprNeg(t); break;
			default:
				throw "Unknown unary operator";
			}
		}
		r = nextPrec(b);
		//printf("E(%d): %s binop %d postfix %d precedence() %d, p <=precedence() %d precedence() <= r %d r=%d\n", p, token_to_string(&token), binop(token.type), postfix(token), precedence(token.type), p <= precedence(token.type), precedence(token.type)<=r, r);
	}
	//pprint_expr(0, t);
	return t;
}
Expr* P(void) {
	if (token.type == T_SUB) { next(); Expr* t = E(2); return mkop(T_SUB, t); }
	else if (token.type == T_NULL) { next(); Expr* t = E(2); return new ExprNull(t); }
	else if (token.type == T_HD) { next(); Expr* t = E(2); return new ExprHd(t); }
	else if (token.type == T_TL) { next(); Expr* t = E(2); return new ExprTl(t); }
	else if (token.type == T_NIL) { next(); return new ExprNil(); }
		if (token.type == T_LPAREN) { next(); Expr* t = E(0); assert(token.type==T_RPAREN);
	//cout << __PRETTY_FUNCTION__ << " parenthesized: " << t->to_string(0) << endl;
	next(); return t; }
	else if (atom(token)) {
		if (token.type == T_IF) {
			return parse_conditional();
		}
		Expr* t = mkleaf(token); next(); return t;
	}
	else { cout << "Unexpected token " << token_to_string(token); exit(1); }
}
Expr* parse_conditional(){
	next(); // advance past 'if'
	auto condition = P();
	auto trueExpr = P();
	auto falseExpr = P();
	//if (token.type == T_EOF || token.type == T_SEMI)
	//	next();
	return new ExprIf(condition, trueExpr, falseExpr);
}
Expr* parse_let();
Expr* parse_expr() {
	if (token.type == T_LET) {
		return parse_let();
	}
	 return E(0);
}
list<ExprLet::Binding> parse_let_exprs()
{
	list<ExprLet::Binding> bindings;
	next();
	while (token.type == T_NAME)
	{
		auto id = token.s;
		next();
		assert(token.type == T_EQUALS);
		next();
		Expr* val = parse_expr();
		assert(token.type == T_IN || token.type == T_SEMI);
		if (token.type == T_SEMI)
			next();
		ExprLet::Binding def;
		def.name = id;
		def.value = val;
		bindings.push_back(def);
	}
	assert(token.type == T_IN);
	next();
	return bindings;
}
Expr* parse_let() {
	auto letx = new ExprLet();
	letx->bindings = parse_let_exprs();
	letx->value = parse_expr();
	return letx;
}
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

string indent(int col)
{
	return string(col,' ');
}

void pprint_expr(int col, const Expr *e)
{
	cout << e->to_string(col) << endl;
}

template <typename C> string join(const C& ner, int joint) {
	string rv;
	auto i = ner.begin();
	rv += *i++;
	while (i != ner.end()) {
		rv += joint;
		rv += *i++;
	}
	return rv;
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
13.<o,PUSHB00L b.c, s, v, G, E, D> => <o, c, n'.s, v, G[n'=B00L b], E, D>
14.<o,PUSHNIL.c, s, v, G, E, D> => <o, c, n'.s, v, GIn'=NIL], E, D>
15.<o,PUSHFUN f.c, s, v, G, E, D> => <o, c, n'.s, v, G[n'=FUN f], E, D>
16.<o,PUSH m.c, n O ..... nm.S, v, G, E, D> => <o, c, nm.n 0 .... nm.S, v, G, E, D>
17.<o,MKINT.c, s, i.v, G, E, D> => <o, c, n'.s, v, G[n'=INT i], E, D>
18.<o,MKB00L.c, s, b.v, G, E, D> => <o, c, n'.s, v, G[n'=B00L b], E, D>
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
	"MUL",
	"NE",
	"NEG",
	"NOT",
	"NULL",
	"PRINT",
	"PUSH",
	"PUSHBASIC",
	"PUSHB00L",
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
struct NInd;
struct NCons;
struct NNil;
struct NHole;
struct NodeVisitor {
	virtual ~NodeVisitor() {}
	virtual void visitNFun(NFun* n) = 0;
	virtual void visitNInt(NInt* n) = 0;
	virtual void visitNBool(NBool* n) = 0;
	virtual void visitNAp(NAp* n) = 0;
	virtual void visitNInd(NInd* n) = 0;
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
struct NInd : public Node {
	NInd(Node* a) : a(a) {}
	string to_string() const {
		return "&"+a->to_string();
	}
	void visit(NodeVisitor* v) { v->visitNInd(this); }
	Node* a;
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
                //cout << "env "<<i.first <<" entry "<<i.second << flush;/*, env, entry); fflush(stdout);*/
                //printf(" %s args %d mode %s\n", entry->name, entry->args, amToString(entry->mode.mode));
                cout << i.first << " args " << i.second.args << ' ' << amToString(i.second.mode) << endl;
        }
}

typedef list<Node*> GmStack;
GmStack nodeStack;
typedef list<int> ValueStack;
ValueStack valueStack;
struct GmStats {

};
void showStack(const string& label) {
	cout << label << endl;
	for (auto s : nodeStack) {
		cout << /*s << ' ' <<*/ '[' << s->to_string() << "] ";
	}
	cout << endl;
}
void showValues(const string& label) {
	cout << label << endl;
	for (auto s : valueStack) {
		cout << /*s << ' ' <<*/ '[' << ::to_string(s) << "] ";
	}
	cout << endl;
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
void stepMkAp() {
	showStack("Stack before mkap");
	Node* a1 = nodeStack.front(); nodeStack.pop_front();
	Node* a2 = nodeStack.front(); nodeStack.pop_front();
	nodeStack.push_front(new NAp(a1, a2));
	showStack("Stack after mkap");
}
void stepPush(unsigned n) {
	showStack("Stack before push");
	unsigned i=0;
	for (const auto& se : nodeStack) {
		cout << i << ": " << se << ' ' << se->to_string() << endl;
		++i;
	}
	auto p = nodeStack.begin();
#if 0
	advance(p,n+1);
	Node* node = *p;
	//auto ap = dynamic_cast<NAp*>(node);
	//assert(ap);
	//auto arg = ap->a2;
	//nodeStack.push_front(arg);
	nodeStack.push_front(node);
#else
	advance(p,n+1);
	Node* node = *p;
	cout << "Push: About to pull " << node->to_string() << " up to top " << endl;
	auto ap = dynamic_cast<NAp*>(node);
	assert(ap);
	auto arg = ap->a2;
	nodeStack.push_front(arg);
#endif
	cout << "Stack after push " << endl;
	i=0;
	for (const auto& se : nodeStack) {
		cout << i << ": " << se << ' ' << se->to_string() << endl;
		++i;
	}
}
void stepSlide(int n) {
	showStack("stack before slide "+::to_string(n));
	auto a0 = nodeStack.front();
	nodeStack.pop_front();
	for (int i=1; i<=n; ++i)
		nodeStack.pop_front();
	nodeStack.push_front(a0);
	showStack("Stack after slide");
}
void stepUpdate(unsigned n) {
	//if (gmStack.size()<=(n+1)) {
	//	cout << "Judgment " << gmStack.size() << " vs " << (n+1) << " is " << (gmStack.size()>(n+1)) << " Stack size " << gmStack.size() << " not big enough for update " << n << endl;
	//	showStack("");
	//	throw "Bad Update";
	//}
	showStack("stack before update "+::to_string(n));
	Node* tos = nodeStack.front(); nodeStack.pop_front();
	showStack("stack during update "+::to_string(n));
	auto p = nodeStack.begin();
	advance(p, n-1);
	cout << "distance(gmStack.begin(),p) " << distance(nodeStack.begin(),p)
		<< " gmStack.size() " << nodeStack.size() << endl;
	//assert((unsigned)distance(gmStack.begin(),p) < gmStack.size());
	*p = new NInd(tos);
	showStack("Stack after update");
}
struct UnwindNodeVisitor : public NodeVisitor {
	UnwindNodeVisitor(ptrdiff_t& pc) : pc(pc),done(false) {}
	ptrdiff_t& pc;
	bool done;
	void visitNInt(NInt*) {
		pc = 0;
		done = true;
	}
	void visitNBool(NBool*) {
		pc = 0;
		done = true;
	}
	void visitNInd(NInd* iitop) {
		Node* replacement = iitop->a;
		nodeStack.front() = replacement;
		showStack("Stack during ap unwind");
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
	void visitNFun(NFun* gtop) {
		if (nodeStack.size() < gtop->args) {
			cout << __PRETTY_FUNCTION__ << ": stack " << nodeStack.size() << " not enough for " << gtop->args << " arguments" << endl;
		}
		GmStack spine;
		GmStack::const_iterator p=nodeStack.begin(); // where NFun is.
		++p; // first argument;
		for (unsigned i=1; i<gtop->args; ++i) {
			Node* a = *p++;
			NAp* c = dynamic_cast<NAp*>(a);
			if (c == nullptr) {
				cout << "argument should be NAp but isn't" << endl;
				throw "Bad arg";
			}
			spine.push_back(c->a2);
		}
		cout << __PRETTY_FUNCTION__<< " Spine ";
		for (const auto& se : spine) {
			cout << ' ' << se->to_string() << endl;
		}
		nodeStack = concat(take(gtop->args, nodeStack), drop(gtop->args+1,nodeStack));
		showStack("Stack during ap unwind");

		pc = gtop->address;
		done = true;
	}
	void visitNAp(NAp* aptop) {
		cout << "aptop " << aptop->to_string() << endl;
		cout << "aptop.a1 " << aptop->a1->to_string() << " aptop.a2 " << aptop->a2->to_string() << endl;
		nodeStack.push_front(aptop->a1);
		showStack("Stack during ap unwind");
		//pc--; // instead of backing up, we reiterate directly.
	}
	void visitNCons(NCons*) { throw "don't unwind Cons"; }
	void visitNNil(NNil*) { throw "don't unwind Nil"; }
	void visitNHole(NHole*) { throw "Don't unwind a hole";}
};
struct PrintNodeVisitor : public NodeVisitor {
	PrintNodeVisitor() : done(false) {}
	bool done;
	void visitNInt(NInt* n) {
		cout << n->to_string();
		done = true;
	}
	void visitNBool(NBool* n) {
		cout << n->to_string();
		done = true;
	}
	void visitNCons(NCons*) {
#if 0
		nodeStack.push_front(n->tl);
		nodeStack.push_front(n->hd);
		stepEval();
		stepPrint();
		stepEval();
		stepPrint();
		done = true;
#endif
		throw "don't print Cons yet";
	}
	void visitNNil(NNil*) { /*Nil doesn't print anything*/ done=true;}
	void visitNInd(NInd* iitop) {
		Node* replacement = iitop->a;
		nodeStack.front() = replacement;
		showStack("Stack NInd print");
	}
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
		cout << "Unwind viewing " << top->to_string() << " from top of stack of size " << nodeStack.size() << endl;
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
	case LT: valueStack.front() = (left<right); break;
	default: throw "Unimplemented math in stepBinop";
	}
	showValues("Stack after "+instructionToString(ins));
}
void stepMkInt() {
	showValues("Stack before MKINT");
	nodeStack.push_front(new NInt(valueStack.front()));
	valueStack.pop_front();
	showStack("Stack after MKINT");
}
void stepRet(const Instruction& ins, ptrdiff_t& pc) {
	showStack("Before RET "+::to_string(ins.n));
	for (unsigned n=0; n<ins.n; n++)
		nodeStack.pop_front();
	showStack("RET after popping "+::to_string(ins.n));
	while (true) {
		auto ap = dynamic_cast<NAp*>(nodeStack.front());
		if (ap) {
			pc = 1;
			break;
		}
		auto ii = dynamic_cast<NInd*>(nodeStack.front());
		if (ii) {
			nodeStack.front() = ii->a;
			continue;
		}
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
			pc = 1;
			break;
		}
		auto ii = dynamic_cast<NInd*>(nodeStack.front());
		if (ii) {
			nodeStack.front() = ii->a;
			continue;
		}
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
bool step(CodeArray& code, ptrdiff_t& pc) {
	Instruction instr = code.code[pc++];
	switch (instr.ins) {
	case ALLOC:
	case CONS:
	case DIV:
	case EQ:
	case GE:
	case GT:
	case HD:
	case LABEL:
	case MKBOOL:
	case MUL:
	case NE:
	case NEG:
	case NOT:
	case NULLinst:
	case PUSHBOOL:
	case PUSHNIL:
	case TL: throw "Unknown instruction in step";
	case ADD: stepBinop(instr); break;
	case EVAL: stepEval(/*instr,*/ pc); break;
	case GET: stepGet(/*instr*/); break;
	case JFALSE: stepJFalse(instr, pc); break;
	case JMP: stepJmp(instr, pc); break;
	case LE:
	case LT: stepBinop(instr); break;
	case MKAP: stepMkAp(); break;
	case MKINT: stepMkInt(); break;
	case PRINT: stepPrint(instr,pc); break;
	case PUSH: stepPush(instr.n); break;
	case PUSHBASIC: stepPushBasic(instr); break;
	case PUSHFUN: stepPushFun(instr.node); break;
	case PUSHINT: stepPushInt(instr.dest); break;
	case RET: stepRet(instr, pc); break;
	case SLIDE: stepSlide(instr.n); break;
	case SUB: stepBinop(instr); break;
	case UPDATE: stepUpdate(instr.n); break;
	case UNWIND: stepUnwind(pc); break;
	case STOP: break;
	case zzzmaxInstr: break; // dummy value
	}
	return instr.ins != STOP;
}
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

void compileC(CodeArray& code, Expr* expr, Env& env);
void compileE(CodeArray& code, Expr* expr, Env& env);
void compileB(CodeArray& code, Expr* expr, Env& env);
struct CompileCVisitor : public ExprVisitor {
	CompileCVisitor(CodeArray& code, Env& env) : code(code), env(env) {}
	CodeArray& code;
	Env& env;
	void visitExprNum(ExprNum* eint) {
		code.add(Instruction(PUSHINT,eint->value));
	}
	void visitExprVar(ExprVar* evar) {
		auto pm = env.find(evar->var);
		if (pm != env.end()) {
			switch (pm->second.mode.mode) {
			case AddressMode::Local: code.add(Instruction(PUSH,pm->second.mode.localIndex)); break;
			case AddressMode::Global: code.add(Instruction(PUSHFUN,pm->second.mode.node)); break;
			}
			cout << __PRETTY_FUNCTION__ << " compileC VAR ended at " << code.code.size() << endl;
			return;
		}
		cout << __PRETTY_FUNCTION__ << " Can't find " << evar->var << " in "; pprint_env(env);
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
			code.add(Instruction(MKAP));
		cout << __PRETTY_FUNCTION__ << "Mkaps for " << eapp->to_string(0) << ", " << eapp->args.size() << "args, ended at " << code.code.size() << endl;
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
	void visitExprBool(ExprBool* e) {
		code.add(Instruction(PUSHBOOL,e->value));
	}
	void visitExprChar(ExprChar*) {}
	void visitExprStr(ExprStr*) {}
	void visitExprNil(ExprNil*) {
		code.add(Instruction(PUSHNIL));
	}
	void visitExprIf(ExprIf* e) {
		cout << __PRETTY_FUNCTION__ << " Not to be handled: " << e->to_string(0) << endl;
	}
	void visitExprCons(ExprCons* e) {
		compileC(code,e->hd, env);
		Env shift = envShift(env, 1);
		compileC(code,e->tl, shift);
		code.add(Instruction(CONS));
	}
	void visitExprHd(ExprHd*) {}
	void visitExprTl(ExprTl*) {}
	void visitExprNull(ExprNull*) {}
	void visitExprOper(ExprOper*) {}
	void visitExprAdd(ExprAdd*) {}
	void visitExprSub(ExprSub* e) {
		// desugar in this case:
		auto e2 = new ExprApp(new ExprVar("sub"),e->left);
		e2->args.push_back(e->right);
		//e2->visit(this);
		compileC(code, e2, env);
		cout << __PRETTY_FUNCTION__ << " Desugar: " << e->to_string(0) << " to " << e2->to_string(3) << endl;
		cout << __PRETTY_FUNCTION__ << " Desugar: compileC code ended at " << code.code.size() << endl;
	}
	void visitExprMul(ExprMul* e) {
		auto e2 = new ExprApp(new ExprVar("mul"),e->left);
		e2->args.push_back(e->right);
		//e2->visit(this);
		compileC(code, e2, env);
		cout << __PRETTY_FUNCTION__ << " Desugar: " << e->to_string(0) << " to " << e2->to_string(3) << endl;
		cout << __PRETTY_FUNCTION__ << " Desugar: compileC code ended at " << code.code.size() << endl;
	}
	void visitExprDiv(ExprDiv*) {}
	void visitExprMod(ExprMod*) {}
	void visitExprEq(ExprEq*) {}
	void visitExprNe(ExprNe*) {}
	void visitExprLt(ExprLt* e) {
		auto e2 = new ExprApp(new ExprVar("__lt"),e->left);
		e2->args.push_back(e->right);
		e2->visit(this);
		cout << __PRETTY_FUNCTION__ << " Desugar: " << e->to_string(0) << " to " << e2->to_string(3) << endl;
	}
	void visitExprGt(ExprGt*) {}
	void visitExprLe(ExprLe*) {}
	void visitExprGe(ExprGe*) {}
	void visitExprNeg(ExprNeg*) {}
};
struct CompileBVisitor : public ExprVisitor {
	CompileBVisitor(CodeArray& code, Env& env) : code(code), env(env) {}
	CodeArray& code;
	Env& env;
	void visitExprNum(ExprNum* eint) {
		cout << __PRETTY_FUNCTION__ << " " << eint->to_string(0) << endl;
		Instruction ins(PUSHBASIC,eint->value);
		cout << __PRETTY_FUNCTION__ << " resulting instruction " << instructionToString(&ins) << endl;
		code.add(ins);
	}
	void visitExprVar(ExprVar* evar) {
		compileE(code, evar, env);
		code.add(Instruction(GET));
	}
	void visitExprApp(ExprApp* eapp) {
		compileE(code, eapp, env);
		code.add(Instruction(GET));
	}
	void visitExprLet(ExprLet*) {}
	void visitExprBool(ExprBool* e) {
		code.add(Instruction(PUSHBASIC,e->value));
	}
	void visitExprChar(ExprChar*) {}
	void visitExprStr(ExprStr*) {}
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
		compileC(code,e->hd, env);
		Env shift = envShift(env, 1);
		compileC(code,e->tl, shift);
		code.add(Instruction(CONS));
	}
	void visitExprHd(ExprHd*) {}
	void visitExprTl(ExprTl*) {}
	void visitExprNull(ExprNull* e) {
		compileE(code, e->subject, env);
		code.add(Instruction(NULLinst));
	}
	void visitExprOper(ExprOper*) {}
	void visitExprAdd(ExprAdd* e) {
		cout << __PRETTY_FUNCTION__ << " " << e->to_string(0) << endl;
		cout << __PRETTY_FUNCTION__ << " code begins at " << code.code.size() << endl;
		compileB(code,e->left, env);
		Env shift = envShift(env, 1);
		compileB(code,e->right, shift);
		code.add(Instruction(ADD));
		cout << __PRETTY_FUNCTION__ << " code begins at " << code.code.size() << endl;
	}
	void visitExprSub(ExprSub* e) {
		cout << __PRETTY_FUNCTION__ << " " << e->to_string(0) << endl;
		compileB(code,e->left, env);
		Env shift = envShift(env, 1);
		compileB(code,e->right, shift);
		code.add(Instruction(SUB));
	}
	void visitExprMul(ExprMul* e) {
		cout << __PRETTY_FUNCTION__ << " " << e->to_string(0) << endl;
		cout << __PRETTY_FUNCTION__ << " code begins at " << code.code.size() << endl;
		//e->left->visit(this); // open coded for economy
		compileB(code,e->left, env);
		Env shift = envShift(env, 1);
		compileB(code,e->right, shift); // can't open code with different env parameter
		code.add(Instruction(MUL));
		cout << __PRETTY_FUNCTION__ << " code begins at " << code.code.size() << endl;
	}
	void visitExprDiv(ExprDiv*) {}
	void visitExprMod(ExprMod*) {}
	void visitExprEq(ExprEq*) {}
	void visitExprNe(ExprNe*) {}
	void visitExprLt(ExprLt* e) {
		cout << __PRETTY_FUNCTION__ << " " << e->to_string(0) << endl;
		compileB(code,e->left, env);
		Env shift = envShift(env, 1);
		compileB(code,e->right, shift);
		code.add(Instruction(LT));
	}
	void visitExprGt(ExprGt*) {}
	void visitExprLe(ExprLe*) {}
	void visitExprGe(ExprGe*) {}
	void visitExprNeg(ExprNeg*) {}
};
struct CompileEVisitor : public ExprVisitor {
	CompileEVisitor(CodeArray& code, Env& env) : code(code), env(env) {}
	CodeArray& code;
	Env& env;
	void visitExprNum(ExprNum* eint) {
		code.add(Instruction(PUSHINT,eint->value));
	}
	void visitExprVar(ExprVar* evar) {
		auto pm = env.find(evar->var);
		if (pm != env.end()) {
			switch (pm->second.mode.mode) {
			case AddressMode::Local: code.add(Instruction(PUSH,pm->second.mode.localIndex)); code.add(Instruction(EVAL)); break;
			case AddressMode::Global: code.add(Instruction(PUSHFUN,pm->second.mode.node)); break;
			}
			return;
		}
	}
	void visitExprApp(ExprApp* eapp) {
		compileC(code,eapp,env);
		code.add(Instruction(EVAL));
	}
	void visitExprLet(ExprLet*) {}
	void visitExprBool(ExprBool* e) {
		code.add(Instruction(PUSHBOOL,e->value));
	}
	void visitExprChar(ExprChar*) {}
	void visitExprStr(ExprStr*) {}
	void visitExprNil(ExprNil*) {
		code.add(Instruction(PUSHNIL));
	}
	void visitExprIf(ExprIf* e) {
		// In the B scheme all the subexpressions are compiled
		// with B. In the E scheme only the condition is compiled
		// with B.
		compileB(code, e->cond, env);
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
		compileC(code,e->hd, env);
		Env shift = envShift(env, 1);
		compileC(code,e->tl, shift);
		code.add(Instruction(CONS));
	}
	void visitExprHd(ExprHd*) {}
	void visitExprTl(ExprTl*) {}
	void visitExprNull(ExprNull* e) {
		compileE(code, e->subject, env);
		code.add(Instruction(NULLinst));
		code.add(Instruction(MKBOOL));
	}
	void visitExprOper(ExprOper*) {}
	void visitExprAdd(ExprAdd* e) {
		compileB(code, e, env);
		code.add(Instruction(MKINT));
	}
	void visitExprSub(ExprSub* e) {
		compileB(code, e, env);
		code.add(Instruction(MKINT));
	}
	void visitExprMul(ExprMul* e) {
		compileB(code, e, env);
		code.add(Instruction(MKINT));
	}
	void visitExprDiv(ExprDiv*) {}
	void visitExprMod(ExprMod*) {}
	void visitExprEq(ExprEq*) {}
	void visitExprNe(ExprNe*) {}
	void visitExprLt(ExprLt* e) {
		compileB(code, e, env);
		code.add(Instruction(MKBOOL));
	}
	void visitExprGt(ExprGt*) {}
	void visitExprLe(ExprLe*) {}
	void visitExprGe(ExprGe*) {}
	void visitExprNeg(ExprNeg*) {}
};
void compileC(CodeArray& code, Expr* expr, Env& env) {
	CompileCVisitor visitor(code, env);
	expr->visit(&visitor);
}
void compileE(CodeArray& code, Expr* expr, Env& env) {
	CompileEVisitor visitor(code,env);
	expr->visit(&visitor);
}
void compileB(CodeArray& code, Expr* expr, Env& env) {
	CompileBVisitor visitor(code,env);
	expr->visit(&visitor);
}
void compileR(CodeArray& code, Expr* expr, size_t args, Env& env) {
#if 0
	compileC(code,expr,env);
	code.add(UpdateInstruction(args));
	code.add(UnwindInstruction());
#else
	compileE(code,expr,env);
	code.add(Instruction(UPDATE,args+1));
	code.add(Instruction(RET,args));
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
    t.name = "__lt"; t.body = new ExprLt(new ExprVar("left"), new ExprVar("right"));
    defs.push_front(t);
    pprint_defs(0, defs);
    Env env;
    CodeArray code;
    code.add(Instruction(STOP));
    code.add(Instruction(UNWIND));

    for (auto def : defs) {
    	EnvItem item;
		item.args = def.args.size();
		item.mode = AddressMode(new NFun(code.code.size(), item.args));
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
    code.add(Instruction(PUSHFUN,mode.node));
    //code.add(UpdateInstruction(0));
    //code.add(PopInstruction(0));
    // maybe the above are not good for zero parameters?
    code.add(Instruction(UNWIND));
    code.add(Instruction(PRINT));
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
		cout << i <<": " << instructionToString(code.code[i]) << endl;
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
			if (id.size()) cout << id << ":" << endl;
			cout << pc <<": " << instructionToString(code.code[pc]) << endl;
		}
    }
    catch (int e) {
		cout << pc << ": " << instructionToString(code.code[pc-1]) << " threw " << e << endl;
    }
    catch (const char* e) {
		cout << pc-1 << ": " << instructionToString(code.code[pc-1]) << " threw " << e << endl;
    }
    catch (const string& e) {
		cout << pc-1 << ": " << instructionToString(code.code[pc-1]) << " threw " << e << endl;
    }

}
