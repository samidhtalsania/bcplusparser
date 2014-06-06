
#include <boost/foreach.hpp>

#include "babb/utils/memory.h"

#include "bcplus/Location.h"
#include "bcplus/elements/Element.h"
#include "bcplus/symbols/ConstantSymbol.h"


namespace bcplus {
namespace elements  {

namespace u = babb::utils;
namespace sy = bcplus::symbols;

Element::Element(Type::type type, ConstantSet const* constants, int cmask, Location const& begin, Location const& end, bool parens)
	: _parens(parens), _type(type), _begin(begin), _end(end), _constants(constants), _cmask(cmask) {
	/* Intentionally left blank */
}

Element::~Element() {
	/* Intentionally left blank */
}


Element::ConstantSet* Element::newConstSet(Element const* a, Element const* b) {
	u::ref_ptr<ConstantSet> set = new ConstantSet();
	
	if (a) {
		for (ConstantSet::const_iterator it = a->beginConstants(); it != a->endConstants(); it++) {
			set->insert(*it);
		}
	}

	if (b) {
		for (ConstantSet::const_iterator it = b->beginConstants(); it != b->endConstants(); it++) {
			set->insert(*it);
		}
	}

	return set.release();
}

Element::ConstantSet* Element::newConstSet(sy::Symbol const* sym) {
	u::ref_ptr<ConstantSet> set = new ConstantSet();

	if (sym->type() == sy::Symbol::Type::CONSTANT) {
		set->insert((sy::ConstantSymbol const*)sym);
	}
	return set;
}

Element::ConstantSet* Element::insertConstants(ConstantSet* dest, Element const* src) {
	if (src) {
		for (ConstantSet::const_iterator it = src->beginConstants(); it != src->endConstants(); it++) {
			dest->insert(*it);
		}
	}
	return dest;
}

}}


