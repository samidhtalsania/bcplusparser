
#include "parser/elements/AtomicFormula.h"

namespace bcplus {
namespace parser {
namespace elements {

AtomicFormula::AtomicFormula(Constant* c, Term* v, Location const& begin, Location const& end, bool parens)
	: Formula(Formula::Type::UNARY, begin, end, parens), _c(c), _v(v) {
	/* Intentionally left blank */
}

AtomicFormula::~AtomicFormula() {
	/* Intentionally left blank */
}

Element* AtomicFormula::copy() const {
	return new AtomicFormula(c()->copy(), v()->copy(), begin(), end(), parens());
}

void AtomicFormula::output(std::ostream& out) const {
	if (parens) out << "(";
	c()->output(out);
	out << "=";
	v()->output(out);
	if (parens) out << ")";
}

}}}


