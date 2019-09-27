/*
 * Expr.cpp
 *
 *  Created on: Sep 27, 2019
 *      Author: menright
 */
#include "Expr.h"

#include <string>
#include <list>
#include <iostream>
#include <cassert>
#include "util.h"

using namespace std;
extern string indent(int col);
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
string ExprIf::to_string(int c) const {
	return "IF " + cond->to_string(c) + " " + trueExpr->to_string(c) + " "+ falseExpr->to_string(c);
}

std::string ExprLet::to_string(int col) const {
	return "LET "
			+ join(mapf(bindings.begin(), bindings.end(), [col](const Binding& x){ return x.name+"="+x.value->to_string(col);}), ' ')
			+ " IN "
			+ value->to_string(col);
}

#include "Token.h"
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
}
Expr* mkop(Token t, Expr* subj) {
	switch (t) {
	case T_SUB: return new ExprNeg(subj);
	default:
		throw "unexpected unary operator token";
	}
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
	case T_LE:
	case T_GT:
	case T_GE:
	case T_EQ:
	case T_NE:
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
		//cerr << __PRETTY_FUNCTION__ << " argument: " << arg->to_string(0) << endl;
		t = mkapp(t, arg);
		//cerr << __PRETTY_FUNCTION__ << " application: " << t->to_string(0) << endl;
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
			case T_NOT: t=new ExprNot(t); break;
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
#if 1
	Expr* t; Expr* hd; Expr* tl;
	switch (token.type) {
	// Prefix operators
	case T_SUB: next(); t = E(2); return new ExprNeg(t);
	case T_NOT: next(); t = E(2); return new ExprNot(t);
	case T_NULL: next(); t = E(2); return new ExprNull(t);
	case T_HD: next(); t=E(2); return new ExprHd(t);
	case T_TL: next(); t=E(2); return new ExprTl(t);
	// Combinator-like forms
	case T_CONS: next(); hd=P(); tl=P(); return new ExprCons(hd,tl);
	case T_IF: return parse_conditional();
	// Parenthesized expr
	case T_LPAREN: next(); t=E(0); assert(token.type==T_RPAREN); next(); return t;
	// Everything else
	default:
		if (atom(token)) {
			t = mkleaf(token); next(); return t;
		} else {
			cerr << "Unexpected token " << token_to_string(token); exit(1);
		}
		break;
	}
#else
	if (token.type == T_SUB) { next(); Expr* t = E(2); return mkop(T_SUB, t); }
	else if (token.type == T_NULL) { next(); Expr* t = E(2); return new ExprNull(t); }
	else if (token.type == T_HD) { next(); Expr* t = E(2); return new ExprHd(t); }
	else if (token.type == T_TL) { next(); Expr* t = E(2); return new ExprTl(t); }
	else if (token.type == T_NIL) { next(); return new ExprNil(); }
		if (token.type == T_LPAREN) { next(); Expr* t = E(0); assert(token.type==T_RPAREN);
	//cerr << __PRETTY_FUNCTION__ << " parenthesized: " << t->to_string(0) << endl;
	next(); return t; }
	else if (atom(token)) {
		if (token.type == T_IF) {
			return parse_conditional();
		}
		if (token.type == T_CONS) {
			next();
			Expr* hd = P();
			Expr* tl = P();
			return new ExprCons(hd,tl);
		}
		Expr* t = mkleaf(token); next(); return t;
	}
	else { cerr << "Unexpected token " << token_to_string(token); exit(1); }
#endif
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
string indent(int col)
{
	return string(col,' ');
}

void pprint_expr(int col, const Expr *e)
{
	cerr << e->to_string(col) << endl;
}




