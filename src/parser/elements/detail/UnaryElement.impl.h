
#include "parser/elements/detail/UnaryElement.h"

namespace bcplus {
namespace parser {
namespace elements {
namespace detail {

template <typename BaseType, int type, typename Op, typename preOpString, typename postOpString>
UnaryElement<BaseType,type,Op,preOpString,postOpString>::UnaryElement(Op const& op, BaseType* subformula, Location const& begin, Location const& end, bool parens)
	: BaseType(type, begin, end, parens), _op(op), _sub(subformula) {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename Op, typename preOpString, typename postOpString>
UnaryElement<BaseType,type,Op,preOpString,postOpString>::~UnaryElement<BaseType,type,Op,preOpString,postOpString>() {
	/* Intentionally left blank */
}

template <typename BaseType, int type, typename Op, typename preOpString, typename postOpString>
Element* UnaryElement<BaseType,type,Op,preOpString,postOpString>::copy() const {
	return new UnaryElement<BaseType,type,Op,preOpString,postOpString>(op(), sub()->copy(), ((BaseType*)this)->begin(), ((BaseType*)this)->end(), ((BaseType*)this)->parens());
}

template <typename BaseType, int type, typename Op, typename preOpString, typename postOpString>
void UnaryElement<BaseType,type,Op,preOpString,postOpString>::output(std::ostream& out) const {
	preOpString pre;
	postOpString post;

	if (((BaseType*)this)->parens()) out << "(";	
	out << pre(op());
	sub()->output(out);
	out << post(op());
	if (((BaseType*)this)->parens()) out << ")";
}
}}}}


