#pragma once

#include <string>
#include <ostream>

#include <boost/property_tree/ptree.hpp>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

#include "parser/symbols/Symbol.h"
#include "parser/symbols/SortSymbol.h"

namespace bcplus {
namespace parser {
namespace symbols {

class Resolver;

class MacroSymbol : public Symbol {
	// TODO
	inline virtual bool integral() const { return false; }
};

}}}
