
#include "parser/Location.h"
#include "parser/elements/Element.h"

namespace bcplus {
namespace parser {
namespace elements  {


Element::Element(Type::Value type, Location const& begin, Location const& end, bool parens)
	: _parens(parens), _type(type), _begin(begin), _end(end) {
	/* Intentionally left blank */
}

Element::~Element() {
	/* Intentionally left blank */
}

}}}


