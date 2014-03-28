
#include "bcplus/elements/detail/BinaryElement.h"

namespace bcplus {
namespace elements {
namespace detail {

template <typename BaseType, int type, typename Op, typename LHS, typename RHS, typename opString, typename dt>
BinaryElement<BaseType, type, Op, LHS, RHS, opString, dt>::BinaryElement(typename Op::type const& op, LHS* left, RHS* right, Location const& begin, Location const& end, bool parens)
	: BaseType((typename BaseType::Type::type)type, begin, end, parens), _op(op), _left(left), _right(right) {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename Op, typename LHS, typename RHS, typename opString, typename dt>
BinaryElement<BaseType, type, Op, LHS, RHS, opString, dt>::~BinaryElement() {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename Op, typename LHS, typename RHS, typename opString, typename dt>
Element* BinaryElement<BaseType, type, Op, LHS, RHS, opString, dt>::copy() const {
	return new BinaryElement(op(), (LHS*)left()->copy(), (RHS*)right()->copy(), ((BaseType*)this)->beginLoc(), ((BaseType*)this)->endLoc(), ((BaseType*)this)->parens());
}

template <typename BaseType, int type, typename Op, typename LHS, typename RHS, typename opString, typename dt>
void BinaryElement<BaseType, type, Op, LHS, RHS, opString, dt>::output(std::ostream& out) const {
	if (((BaseType*)this)->parens()) out << "(";
	left()->output(out);
	opString cstr;
	out << cstr(op());
	right()->output(out);
	if (((BaseType*)this)->parens()) out << ")";
}

template <typename BaseType, int type, typename Op, typename LHS, typename RHS, typename opString, typename dt>
DomainType::type BinaryElement<BaseType, type, Op, LHS, RHS, opString, dt>::domainType() const {
	dt dtype;
	return dtype(op());
}

}}}


