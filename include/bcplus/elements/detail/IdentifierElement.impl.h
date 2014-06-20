#pragma once

#include "babb/utils/memory.h"

#include "bcplus/Location.h"
#include "bcplus/elements/Element.h"
#include "bcplus/elements/detail/IdentifierElement.h"
#include "bcplus/symbols/ConstantSymbol.h"


namespace u = babb::utils;
namespace sy = bcplus::symbols;

namespace bcplus {
namespace elements {
namespace detail {

template <typename BaseType, int type, typename SymbolType>
IdentifierElement_bare<BaseType, type, SymbolType>::IdentifierElement_bare(SymbolType const* symbol, Location const& begin, Location const& end, bool parens) 
	: BaseType(
		(typename BaseType::Type::type)type, 
		BaseType::newConstSet(symbol), 
		BaseType::newVarSet(symbol),
		(symbol->type() == sy::Symbol::Type::CONSTANT ? ((sy::ConstantSymbol const*)symbol)->constType() : 0),
		begin, end, parens), _sym(symbol) {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType>
IdentifierElement_bare<BaseType, type, SymbolType>::~IdentifierElement_bare() {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType>
Element* IdentifierElement_bare<BaseType, type, SymbolType>::copy() const {
	BaseType const* bt = (BaseType const*)this;
	return new IdentifierElement_bare<BaseType, type, SymbolType>(symbol(), bt->beginLoc(), bt->endLoc(), bt->parens());
}

template <typename BaseType, int type, typename SymbolType>
void IdentifierElement_bare<BaseType, type, SymbolType>::output(std::ostream& out) const {
	out << symbol()->base();
}

template <typename BaseType, int type, typename SymbolType>
DomainType::type IdentifierElement_bare<BaseType, type, SymbolType>::domainType() const {
	return symbol()->domainType();
}

template <typename BaseType, int type, typename SymbolType, typename ArgType>
IdentifierElement<BaseType, type, SymbolType, ArgType>::IdentifierElement(SymbolType const* symbol, ArgumentList const* args, Location const& begin, Location const& end, bool parens) 
	: IdentifierElement_bare<BaseType, type, SymbolType>(symbol, begin, end, parens) {
	if (args) _args = args;
	else _args = new ArgumentList();
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType, typename ArgType>
IdentifierElement<BaseType, type, SymbolType, ArgType>::~IdentifierElement() {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType, typename ArgType>
Element* IdentifierElement<BaseType, type, SymbolType, ArgType>::copy() const {

	// Create a new list
	u::ref_ptr<ArgumentList> l = new ArgumentList();
	for (const_iterator it = begin(); it != end(); it++) {
		l->push_back((ArgType*)(*it)->copy());
	}

	return new IdentifierElement<BaseType, type, SymbolType, ArgType>(IdentifierElement_bare<BaseType, type, SymbolType>::symbol(), l, ((BaseType const*)this)->beginLoc(), ((BaseType const*)this)->endLoc(), ((BaseType const*)this)->parens());
}

template <typename BaseType, int type, typename SymbolType, typename ArgType>
void IdentifierElement<BaseType, type, SymbolType, ArgType>::output(std::ostream& out) const {
	IdentifierElement_bare<BaseType, type, SymbolType>::output(out);
	if (arity() > 0) {
		out << "(";
		for (const_iterator it = begin(); it != end(); ) {
			(*it)->output(out);
			it++;
			if (it != end()) {
				out << ", ";
			}
		}
		out << ")";
	}
}	



}}}


