
#include "bcplus/DomainType.h"
#include "bcplus/elements/formulas.h"
#include "bcplus/elements/terms.h"
#include "bcplus/elements/BindingFormula.h"

namespace bcplus {
namespace elements {

BindingFormula::BindingFormula(Term* step, Formula* formula, Location const& begin, Location const& end, bool p)
	: Formula(Formula::Type::ATOMIC, begin, end, p), _step(step), _formula(formula) {
	/* Intentionally left blank */
}

BindingFormula::~BindingFormula() {
	/* Intentionally left blank */
}

Element* BindingFormula::copy() const {
	return new BindingFormula((elements::Term*)step()->copy(), (elements::Formula*)formula()->copy(), beginLoc(), endLoc(), parens());
}

void BindingFormula::output(std::ostream& out) const {
	if (parens()) out << "(";
	step()->output(out);
	out << "=";
	formula()->output(out);
	if (parens()) out << ")";
}

DomainType::type BindingFormula::domainType() const {
	return formula()->domainType();
}

}}


