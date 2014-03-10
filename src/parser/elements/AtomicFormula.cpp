
#include "parser/elements/formulas.h"
#include "parser/elements/terms.h"
#include "parser/elements/AtomicFormula.h"

namespace bcplus {
namespace parser {
namespace elements {

AtomicFormula::AtomicFormula(Constant* c, Term* v, Location const& begin, Location const& end, bool p)
	: Formula(Formula::Type::ATOMIC, begin, end, p), _c(c), _v(v) {
	/* Intentionally left blank */
}

AtomicFormula::~AtomicFormula() {
	/* Intentionally left blank */
}

Element* AtomicFormula::copy() const {
	return new AtomicFormula((Constant*)c()->copy(), (Term*)v()->copy(), beginLoc(), endLoc(), parens());
}

void AtomicFormula::output(std::ostream& out) const {
	if (parens()) out << "(";
	c()->output(out);
	out << "=";
	v()->output(out);
	if (parens()) out << ")";
}

}}}


