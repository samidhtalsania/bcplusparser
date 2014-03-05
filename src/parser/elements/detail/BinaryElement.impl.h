
#include "parser/elements/detail/BinaryElement.h"

namespace bcplus {
namespace parser {
namespace elements {
namespace detail {

template <typename BaseType, int type, typename Op, typename opString>
BinaryElement<BaseType, type, Op, opString>::BinaryElement(Op const& op, BaseType* left, BaseType* right, Location const& begin, Location const& end, bool parens)
	: BaseType(type, begin, end, parens), _left(left), _op(op), _right(right) {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename Op, typename opString>
BinaryElement<BaseType, type, Op, opString>::~BinaryElement() {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename Op, typename opString>
Element* BinaryElement<BaseType, type, Op, opString>::copy() const {
	return new BinaryElement(op(), left()->copy(), right()->copy(), ((BaseType*)this)->begin(), ((BaseType*)this)->bend(), ((BaseType*)this)->bparens());
}

template <typename BaseType, int type, typename Op, typename opString>
void BinaryElement<BaseType, type, Op, opString>::output(std::ostream& out) const {
	if (((BaseType*)this)->parens()) out << "(";
	left()->output(out);
	opString cstr;
	out << cstr(op());
	right()->output(out);
	if (((BaseType*)this)->parens()) out << ")";
}
}}}}


