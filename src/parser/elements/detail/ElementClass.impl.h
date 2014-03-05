
#include "parser/Location.h"
#include "parser/elements/detail/ElementClass.h"

namespace bcplus {
namespace parser {
namespace elements  {
namespace detail {

template<int type, typename Subtypes>
ElementClass<type, Subtypes>::ElementClass(Subtype type, Location const& begin, Location const& end, bool parens)
	: Element(type, begin, end, parens) {
	/* Intentionally left blank */
}

template<int type, typename Subtypes>
ElementClass<type, Subtypes>::~ElementClass() {
	/* Intentionally left blank */
}

}}}}


