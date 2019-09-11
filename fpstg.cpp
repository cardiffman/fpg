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
using namespace std;

template <typename T, typename UnaryOp, typename T2=std::string, typename C2=std::list<T2>>
C2 mapf(const T b, const T e, UnaryOp f)
{
	C2 out;
	std::transform(b, e, back_inserter(out), f);
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
#ifdef EXPR_TAGS
enum ExprType {
    EXPR_VAR,
    EXPR_APP,
    EXPR_STR,
    EXPR_NUM,
	EXPR_NIL,
	EXPR_IF,
	EXPR_CHR,
	EXPR_CONS,
	EXPR_HD,
	EXPR_NULL,
	EXPR_OPER,
	EXPR_LET,
	EXPR_ADD,
	EXPR_SUB,
	EXPR_MUL,
	EXPR_DIV,
	EXPR_MOD,
	EXPR_EQ,
	EXPR_NE,
	EXPR_LT,
	EXPR_GT,
	EXPR_LE,
	EXPR_GE,
	EXPR_NEG,
};
#endif

extern string indent(int col);
struct ExprVar;
struct ExprNum;
struct ExprBool;
struct ExprApp;
struct ExprStr;
struct ExprNil;
struct ExprIf;
struct ExprChar;
struct ExprCons;
struct ExprHd;
struct ExprTl;
struct ExprNull;
struct ExprOper;
struct ExprLet;
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
	virtual void visitExprBool(ExprBool* e) = 0;
	virtual void visitExprChar(ExprChar* e) = 0;
	virtual void visitExprApp(ExprApp* e) = 0;
	virtual void visitExprStr(ExprStr* e) = 0;
	virtual void visitExprNil(ExprNil* e) = 0;
	virtual void visitExprIf(ExprIf* e) = 0;
	virtual void visitExprCons(ExprCons* e) = 0;
	virtual void visitExprHd(ExprHd* e) = 0;
	virtual void visitExprTl(ExprTl* e) = 0;
	virtual void visitExprNull(ExprNull* e) = 0;
	virtual void visitExprOper(ExprOper* e) = 0;
	virtual void visitExprLet(ExprLet* e) = 0;
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
#ifdef EXPR_TAGS
	Expr() :type() {}
	Expr(ExprType t) : type(t) {}
#else
	Expr() {}
#endif
	virtual ~Expr() {}
	virtual string to_string(int) const = 0;
	virtual void visit(ExprVisitor*) = 0;
#ifdef EXPR_TAGS
	ExprType type;
#endif
};
struct ExprVar : public Expr {
#ifdef EXPR_TAGS
	ExprVar(const string& v) : Expr(EXPR_VAR), var(v) {}
#else
	ExprVar(const string& v) : var(v) {}
#endif
	string to_string(int) const { return "VAR " + var; }
	void visit(ExprVisitor* v) { v->visitExprVar(this); }
	string var;
};
struct ExprBool : public Expr {
	ExprBool(bool v) : value(v) {}
	string to_string(int) const { return value?"true":"false"; }
	void visit(ExprVisitor* v) { v->visitExprBool(this); }
	bool value;
};
struct ExprApp : public Expr {
#ifdef EXPR_TAGS
	ExprApp(Expr* fun, Expr* arg) : Expr(EXPR_APP), fun(fun) { args.push_back(arg); }
#else
	ExprApp(Expr* fun, Expr* arg) : fun(fun) { args.push_back(arg); }
#endif
	Expr* fun;
	list<Expr*> args;
	string to_string(int) const;
	void visit(ExprVisitor* v) { v->visitExprApp(this); }
};
string ExprApp::to_string(int col) const {
	string rv = "APP " + fun->to_string(col);
	rv += " ARGS ";
	auto argstrs = mapf(args.begin(), args.end(), [col](const Expr* ex) { return ex->to_string(col); });
	if (argstrs.size()) {
		rv += '\n';
		rv += string(col,' ');
	}
	rv += join(argstrs, ' ');
	return rv;
}
struct ExprStr : public Expr {
#ifdef EXPR_TAGS
	ExprStr(const std::string str): Expr(EXPR_STR), str(str) {}
#else
	ExprStr(const std::string str): str(str) {}
#endif
	string str;
	string to_string(int) const { return "STR \"" + str + '"'; }
	void visit(ExprVisitor* v) { v->visitExprStr(this); }
};
struct ExprNum : public Expr {
#ifdef EXPR_TAGS
	ExprNum(ptrdiff_t value) : Expr(EXPR_NUM), value(value) {}
#else
	ExprNum(ptrdiff_t value) : value(value) {}
#endif
	ptrdiff_t value;
	string to_string(int) const { return "NUM " + ::to_string((int)value); }
	void visit(ExprVisitor* v) { v->visitExprNum(this); }
};
struct ExprOper : public Expr {
#ifdef EXPR_TAGS
	ExprOper(int op) : Expr(EXPR_OPER), op(op) {}
#else
	ExprOper(int op) : op(op) {}
#endif
	int op;
	string to_string(int col) const;
	void visit(ExprVisitor* v) { v->visitExprOper(this); }
};
string ExprOper::to_string(int) const {
	switch (op) {
	case 9: return "LT";
	case 15: return "ADD";
	case 16: return "SUB";
	case 17: return "MUL";
	}
	return ::to_string(op);
}
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
	case T_NAME: return new ExprVar(t.s);
	case T_NUM: return new ExprNum(t.value);
	case T_NIL: return new ExprNil();
	case T_TRUE: return new ExprBool(true);
	case T_FALSE: return new ExprBool(false);
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
	ExprOper* r = new ExprOper(t);
	return r;
}
Expr* mkop(Token t, Expr* subj) {
	switch (t) {
	case T_SUB: return new ExprNeg(subj);
	default:
		throw "unexpected unary operator token";
	}
	ExprOper* r = new ExprOper(t);
	return r;
}
Expr* mkapp(Expr* fun_, Expr* arg) {
	ExprApp* fun = dynamic_cast<ExprApp*>(fun_);
	if (fun) {
		fun->args.push_back(arg);
		return fun;
	}
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
		cout << __PRETTY_FUNCTION__ << " argument: " << arg->to_string(0) << endl;
		t = mkapp(t, arg);
		cout << __PRETTY_FUNCTION__ << " application: " << t->to_string(0) << endl;
	}

	int r = 8;
	//printf("E(%d): %s binop %d postfix %d precedence() %d, p <=precedence() %d precedence() <= r %d r=%d\n", p, token_to_string(&token), binop(token.type), postfix(token), precedence(token.type), p <= precedence(token.type), precedence(token.type)<=r, r);
	while ((binop(token.type) || postfix(token.type)) && ((p <= precedence(token.type)) && (precedence(token.type) <= r))) {
		Token b = token.type;
		next();
		if (binop(b)) {
			Expr* t1 = E(rightPrec(b));
			//printf("RHS expr: "); pprint_expr(0, t1);
			t = mkop(b, t, t1);
			//pprint_expr(0, t);
		} else {
			t = mkop(b, t);
		}
		r = nextPrec(b);
		//printf("E(%d): %s binop %d postfix %d precedence() %d, p <=precedence() %d precedence() <= r %d r=%d\n", p, token_to_string(&token), binop(token.type), postfix(token), precedence(token.type), p <= precedence(token.type), precedence(token.type)<=r, r);
	}
	//pprint_expr(0, t);
	return t;
}
Expr* P(void) {
	if (token.type == T_SUB) { next(); Expr* t = E(2); return mkop(T_SUB, t); }
	else if (token.type == T_LPAREN) { next(); Expr* t = E(0); assert(token.type==T_RPAREN);
	cout << __PRETTY_FUNCTION__ << " parenthesized: " << t->to_string(0) << endl;
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
		CodeArray* code;
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
	auto oper = dynamic_cast<const ExprOper*>(exp);
	if (oper)
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
	virtual void visitExprOper(ExprOper*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
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
	virtual void visitExprOper(ExprOper*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
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
	virtual void visitExprOper(ExprOper*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
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
	virtual void visitExprOper(ExprOper*) { cout << __PRETTY_FUNCTION__ << " to come" << endl; }
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
	void visitExprOper(ExprOper* e) { (void)e; r=false; }
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
	void visitExprOper(ExprOper* e) {(void)e;}
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
