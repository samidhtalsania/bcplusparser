#include <ostream>
#include <boost/foreach.hpp>

#include "bcplus/Location.h"
#include "bcplus/DomainType.h"
#include "bcplus/elements/Element.h"
#include "bcplus/elements/formulas.h"
#include "bcplus/elements/terms.h"
#include "bcplus/elements/CardinalityFormula.h"

namespace bcplus {
namespace elements {


CardinalityFormula::CardinalityFormula(AtomicFormula* af, BindingList* binds, Term* min, Term* max, Location const& begin, Location const& end, bool parens)
	: Formula(Formula::Type::CARDINALITY, begin, end, parens), _min(min), _max(max), _binds(binds), _af(af) {
	/* Intentionally left blank */
}

CardinalityFormula::~CardinalityFormula() {
	/* Intentionally left blank */
}

Element* CardinalityFormula::copy() const {
	// copy the binding list
	BindingList* l = new BindingList();
	BOOST_FOREACH(Binding const& bind, *this) {
		l->push_back(Binding(bind.first, (LocalVariable*)bind.second->copy()));
	}

	return new CardinalityFormula((AtomicFormula*)af()->copy(), l, (Term*)min()->copy(), (Term*)max()->copy(), beginLoc(), endLoc(), parens());
}

void CardinalityFormula::output(std::ostream& out) const {
	if (min()) out << *min();
	out << "{" << *af();
	BOOST_FOREACH(Binding const& bind, *this) {
		out << " : " << *bind.first->base() << "(" << *bind.second << ")";
	}
	out << "}";
	if (max()) out << *max();
}

DomainType::type CardinalityFormula::domainType() const {
	return DomainType::NO_DOMAIN;
}


}}




