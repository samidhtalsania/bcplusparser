
#include "parser/Location.h"
#include "parser/elements/detail/ElementClass.h"

namespace bcplus {
namespace parser {
namespace elements  {
namespace detail {

template<int t, typename Subtypes>
ElementClass<t, Subtypes>::ElementClass(typename Type::Value subtype, Location const& begin, Location const& end, bool parens)
	: Element((Element::Type::Value)t, begin, end, parens), _subtype(subtype) {
	/* Intentionally left blank */
}

template<int type, typename Subtypes>
ElementClass<type, Subtypes>::~ElementClass() {
	/* Intentionally left blank */
}

}}}}


