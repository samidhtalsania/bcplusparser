
#include "bcplus/elements/QuantifierFormula.h"

namespace bcplus {
namespace elements {

QuantifierFormula::QuantifierFormula(
	QuantifierList* quants,
	Formula* subformula,
	Location const& begin,
	Location const& end,
	bool parens) 
	: Formula(Formula::Type::QUANTIFIER, begin, end, parens), _quants(quants), _sub(subformula)
{
	/* Intentionally left blank */
}

QuantifierFormula::~QuantifierFormula() {
	/* Intentionally left blank */
}

Element* QuantifierFormula::copy() const {

	// Make a new quantifier list
	QuantifierList* l = new QuantifierList();
	for (const_iterator it = begin(); it != end(); it++) {
		l->push_back(Quantifier(it->first, (Variable*)it->second->copy()));
	}

	return new QuantifierFormula(l, (Formula*)subformula()->copy(), beginLoc(), endLoc(), parens());
}

void QuantifierFormula::output(std::ostream& out) const {
	if (parens()) out << "(";

	out << "[";
	for (const_iterator it = begin(); it != end(); it++) {
		outputQuantifier(*it, out);
	}
	out << "| ";
	subformula()->output(out);
	out << " ]";
	if (parens()) out << ")";
}

void QuantifierFormula::outputQuantifier(Quantifier const& q, std::ostream& out) const {
	switch (q.first) {
	case Operator::CONJ:				out << "/\\";	break;
	case Operator::DISJ:				out << "\\/";	break;
	}
	q.second->output(out);

}
}}


