/*
 * util.h
 *
 *  Created on: Sep 27, 2019
 *      Author: menright
 */

#ifndef UTIL_H_
#define UTIL_H_

#include <string>
#include <list>
#include <algorithm>

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
std::string fold(std::list<std::string>& strings, std::string base, BinaryOp op)
{
	std::list<std::string>::const_iterator ps = strings.begin();
	std::string out = op(base,*ps++);
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

inline std::string join(const std::list<std::string>& strs, int joint) {
	std::string r;
	auto p = strs.begin();
	r = *p++;
	for (; p!=strs.end(); ++p) {
		r+= std::string(1,joint);
		r+= *p;
	}
	return r;
}

template <typename C> std::string join(const C& ner, int joint) {
	std::string rv;
	auto i = ner.begin();
	rv += *i++;
	while (i != ner.end()) {
		rv += joint;
		rv += *i++;
	}
	return rv;
}



#endif /* UTIL_H_ */
