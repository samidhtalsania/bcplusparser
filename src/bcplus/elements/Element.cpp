
#include "bcplus/Location.h"
#include "bcplus/elements/Element.h"

namespace bcplus {
namespace elements  {


Element::Element(Type::type type, Location const& begin, Location const& end, bool parens)
	: _parens(parens), _type(type), _begin(begin), _end(end) {
	/* Intentionally left blank */
}

Element::~Element() {
	/* Intentionally left blank */
}

}}


