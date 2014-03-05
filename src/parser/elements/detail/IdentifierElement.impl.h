#pragma once

#include "parser/Location.h"
#include "parser/elements/Element.h"
#include "parser/elements/detail/IdentifierElement.h"

namespace bcplus {
namespace parser {
namespace elements {
namespace detail {

template <typename BaseType, int type, typename SymbolType>
IdentifierElement_bare<BaseType, type, SymbolType>::IdentifierElement_bare(SymbolType const* symbol, Location const& begin, Location const& end, bool parens) : BaseType(type, begin, end, parens), _sym(symbol) {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType>
IdentifierElement_bare<BaseType, type, SymbolType>::~IdentifierElement_bare() {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType>
Element* IdentifierElement_bare<BaseType, type, SymbolType>::copy() const {
	return new IdentifierElement_bare<BaseType, type, SymbolType>(symbol(), ((BaseType const*)this)->begin(), ((BaseType const*)this)->end(), ((BaseType const*)this)->parens());
}

template <typename BaseType, int type, typename SymbolType>
void IdentifierElement_bare<BaseType, type, SymbolType>::output(std::ostream& out) const {
	out << symbol()->base();
}


template <typename BaseType, int type, typename SymbolType, typename ArgType>
IdentifierElement<BaseType, type, SymbolType, ArgType>::IdentifierElement(SymbolType const* symbol, ArgumentList* args, Location const& begin, Location const& end, bool parens) 
	: IdentifierElement_bare<BaseType, type, SymbolType>(symbol, type, begin, end, parens), _args(args) {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType, typename ArgType>
IdentifierElement<BaseType, type, SymbolType, ArgType>::~IdentifierElement() {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename SymbolType, typename ArgType>
Element* IdentifierElement<BaseType, type, SymbolType, ArgType>::copy() const {

	// Create a new list
	ref_ptr<ArgumentList> l = new ArgumentList();
	for (iterator it = begin(); it != end(); it++) {
		l->push_back((*it)->copy());
	}

	return new IdentifierElement<BaseType, type, SymbolType, ArgType>(IdentifierElement_bare<BaseType, type, SymbolType>::symbol(), l, ((BaseType const*)this)->begin(), ((BaseType const*)this)->end(), ((BaseType const*)this)->parens());
}

template <typename BaseType, int type, typename SymbolType, typename ArgType>
void IdentifierElement<BaseType, type, SymbolType, ArgType>::output(std::ostream& out) const {
	IdentifierElement_bare<BaseType, type, SymbolType>::output(out);
	if (arity() > 0) {
		out << "(";
		for (iterator it = begin(); it != end(); ) {
			(*it)->output(out);
			it++;
			if (it != end()) {
				out << ", ";
			}
		}
		out << ")";
	}
}	

}}}}


