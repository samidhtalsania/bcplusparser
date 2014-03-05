#pragma once

#include "parser/symbols/MacroSymbol.h"

#include "parser/elements/Element.h"
#include "parser/elements/detail/IdentifierElement.h"
#include "parser/elements/terms.h"

namespace bcplus {
namespace parser {
namespace elements {
/**
 * @brief An arbitrary macro instance 'm(...)'.
 */
typedef detail::IdentifierElement<Element, Element::Type::MACRO, symbols::MacroSymbol, Element> Macro;

}}}
