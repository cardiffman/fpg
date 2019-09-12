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
struct ExprBool : public Expr {
	ExprBool(bool v) : value(v) {}
	string to_string(int) const { return value?"true":"false"; }
	void visit(ExprVisitor* v) { v->visitExprBool(this); }
	bool value;
};
struct ExprApp : public Expr {
	ExprApp(Expr* fun, Expr* arg) : fun(fun) { args.push_back(arg); }
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
	ExprStr(const std::string str): str(str) {}
	string str;
	string to_string(int) const { return "STR \"" + str + '"'; }
	void visit(ExprVisitor* v) { v->visitExprStr(this); }
};
struct ExprNum : public Expr {
	ExprNum(ptrdiff_t value) : value(value) {}
	ptrdiff_t value;
	string to_string(int) const { return "NUM " + ::to_string((int)value); }
	void visit(ExprVisitor* v) { v->visitExprNum(this); }
};
struct ExprOper : public Expr {
	ExprOper(int op) : op(op) {}
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
	SLIDE,
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
#if 0
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
		"SLIDE",
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
#if 0
		"UPDATE"
#endif
};
const char* insToString(InstructionType ins) {
	if (0<=ins && ins<zzzmaxInstr)
		return insNames[ins];
	return "inxxx";
}
struct Node {
	virtual ~Node() {}
	virtual string to_string() const = 0;
};
struct NGlobal : public Node {
	NGlobal(ptrdiff_t address, unsigned args) : address(address), args(args) {}
	string to_string() const {
		return "NG " + ::to_string(address);
	}
	ptrdiff_t address;
	unsigned args;
};
struct NInt : public Node {
	NInt(int i) : i(i) {}
	string to_string() const {
		return "NInt " + ::to_string(i);
	}
	int i;
};
struct NAp : public Node {
	NAp(Node* a1, Node* a2) : a1(a1), a2(a2) {}
	string to_string() const {
		std::ostringstream os;
		os << "NAp " << a1 << " " << a2;
		return os.str();
	}
	Node* a1;
	Node* a2;
};
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
InstructionType needed[] = { MKAP, PUSH, PUSHGLOBAL, PUSHINT, SLIDE, STOP,UNWIND,  };
string instructionToString(Instruction* ins) {
	string rv = insToString(ins->ins);
	switch (ins->ins) {
	case PUSHGLOBAL: rv += " " + ::to_string(ins->node->address); break;
	case PUSHINT: rv += " " + ::to_string(ins->i); break;
	case PUSH:
	case SLIDE: rv += " " + ::to_string(ins->n); break;
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

typedef list<Node*> GmStack;
GmStack gmStack;
struct GmStats {

};
#if 0
void pushGlobal(ptrdiff_t dest, unsigned args) {
	gmStack.push_back(new NGlobal(dest, args));
}
#endif
void pushGlobal(NGlobal* sc) {
	gmStack.push_back(sc);
}
void pushInt(int i) {
	gmStack.push_back(new NInt(i));
}
void mkAp() {
	cout << "Stack before mkap" << endl;
	for (auto s : gmStack) {
		cout << s << ' ' << s->to_string() << ' ';
	}
	cout << endl;
	Node* a1 = gmStack.back(); gmStack.pop_back();
	Node* a2 = gmStack.back(); gmStack.pop_back();
	gmStack.push_back(new NAp(a1, a2));
	cout << "Stack after mkap" << endl;
	for (auto s : gmStack) {
		cout << s << ' ' << s->to_string() << ' ';
	}
	cout << endl;
}
void push(int n) {
	cout << "Stack before push " << endl;
	for (auto s : gmStack) {
		cout << s << ' ' << s->to_string() << ' ';
	}
	cout << endl;
	for (unsigned i=0; i<1 && i<gmStack.size(); ++i) {
		auto p = gmStack.end();
		prev(p,i+1);
		cout << i << ": " << *p << ' ' << (*p)->to_string() << endl;
	}
	auto p = gmStack.end();
	p = prev(p,n);
	Node* node = *p;
	auto ap = dynamic_cast<NAp*>(node);
	assert(ap);
	auto arg = ap->a2;
	gmStack.push_back(arg);
	cout << "Stack after push " << endl;
	for (unsigned i=0; i<=n+1 && i<gmStack.size(); ++i) {
		auto p = gmStack.begin();
		advance(p,i);
		cout << i << ": " << (*p)->to_string() << endl;
	}
}
void slide(int n) {
	auto a0 = gmStack.back();
	gmStack.pop_back();
	for (int i=1; i<=n; ++i)
		gmStack.pop_back();
	gmStack.push_back(a0);
	cout << "Stack after slide " << n << endl;
	for (auto s : gmStack) {
		cout << s << ' ' << s->to_string() << ' ';
	}
	cout << endl;
}
// Unwind will cause a jump if the top node is an NGlobal.
void unwind(ptrdiff_t& pc) {
	cout << "Stack before unwind " << endl;
	for (auto s : gmStack) {
		cout << s << ' ' << s->to_string() << ' ';
	}
	cout << endl;
	while (true) {
		Node* top = gmStack.back();
		auto itop = dynamic_cast<NInt*>(top);
		if (itop)
			break;
		auto aptop = dynamic_cast<NAp*>(top);
		if (aptop) {
			cout << "aptop " << aptop->to_string() << endl;
			cout << "aptop.a1 " << aptop->a1->to_string() << " aptop.a2 " << aptop->a2->to_string() << endl;
			gmStack.push_back(aptop->a1);
			cout << "Stack during ap unwind " << endl;
			for (auto s : gmStack) {
				cout << s << ' ' << s->to_string() << ' ';
			}
			cout << endl;
			//pc--; // instead of backing up, we reiterate directly.
			continue;
		}
		auto gtop = dynamic_cast<NGlobal*>(top);
		if (gtop) {
			if (gmStack.size() < gtop->args) {
				cout << __PRETTY_FUNCTION__ << ": stack " << gmStack.size() << " not enough for " << gtop->args << " arguments" << endl;
			}
			pc = gtop->address;
			break;
		}
	}
	cout << "Stack after unwind " << endl;
	for (auto s : gmStack) {
		cout << s << ' ' << s->to_string() << ' ';
	}
	cout << endl;
}
bool step(CodeArray& code, ptrdiff_t& pc) {
	Instruction instr = code.code[pc++];
	switch (instr.ins) {
	case PUSHGLOBAL: pushGlobal(instr.node); break;
	case PUSHINT: pushInt(instr.i); break;
	case MKAP: mkAp(); break;
	case PUSH: push(instr.n); break;
	case SLIDE: slide(instr.n); break;
	case UNWIND: unwind(pc); break;
	case STOP: break;
	case zzzmaxInstr: break; // dummy value
	}
	return instr.ins != STOP;
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

Instruction SlideInstruction(unsigned n) {
	Instruction ins;
	ins.ins = SLIDE;
	ins.n = n;
	return ins;
}
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
void compileC(CodeArray& code, Expr* expr, Env& env) {
	auto eint = dynamic_cast<ExprNum*>(expr);
	if (eint) {
		code.add(PushIntInstruction(eint->value));
		return;
	}
	auto evar = dynamic_cast<ExprVar*>(expr);
	if (evar) {
		auto pm = env.find(evar->var);
		if (pm != env.end()) {
			switch (pm->second.mode.mode) {
			case AddressMode::Local: code.add(PushInstruction(pm->second.mode.localIndex)); break;
			case AddressMode::Global: code.add(PushGlobalInstruction(pm->second.mode.node)); break;
			}
			return;
		}
		return;
	}
	auto eapp = dynamic_cast<ExprApp*>(expr);
	if (eapp) {
		auto pArg = eapp->args.rbegin();
		compileC(code,*pArg++,env);
		while (pArg != eapp->args.rend()) {
			compileC(code,*pArg++,env);
			//if (pArg != eapp->args.rend())
				code.add(MkapInstruction());
		}
		compileC(code,eapp->fun,env);
		code.add(MkapInstruction());
		return;
	}
}
void compileR(CodeArray& code, Expr* expr, size_t args, Env& env) {
	compileC(code,expr,env);
	code.add(SlideInstruction(args+1));
	code.add(UnwindInstruction());
}
void compileSc(CodeArray& code, const Definition& def, Env& env) {
	Env env2 = envDup(env);
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
    for (auto def : defs) {
    	EnvItem item;
		item.args = def.args.size();
		item.mode = AddressMode(new NGlobal(code.code.size(), item.args));
		// If there are no arguments, the entry point should be
		// the first instruction we will emit.
		// If there are arguments, the entry point should be
		// the check instruction, which is after n-1 push-labels.
		//if (item.args == 0)
		//	item.mode.address = code.code.size();
		//else
		//	item.mode.params.address = code.code.size() + (item.args-1);
		env[def.name] = item;
		compileSc(code, def, env);
    }
    AddressMode mode;
    auto m = find_mode(env, "main", &mode);
    ptrdiff_t pc = code.code.size();
    code.add(PushGlobalInstruction(mode.node));
    code.add(UnwindInstruction());
    Instruction instr; instr.ins = STOP;
    code.add(instr);
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

