#pragma once

#include <string>

namespace bcplus {
namespace parser {
namespace symbols {

class SortSymbol;
class VariableSymbol;
class ObjectSymbol;
class ConstantSymbol;
class MacroSymbol;

/// A basic interface that resolves symbols based on their type, name, and arity
class Resolver {
public:

	/// Find a specific type of symbol matching the provided name/arity
	/// @param typemask A mask of types that we should look for
	/// @param base The base name of the symbol
	/// @param arity The arity of the symbol
	/// @return The symbol matching the parameters or NULL if it doesn't exist.
	virtual Symbol const* resolve(size_t typemask, std::string const& name, size_t arity = 0) const = 0;
	virtual Symbol* resolve(size_t typemask, std::string const& name, size_t arity = 0) = 0;

	/// Attempts to resolve a symbol matching the provided object and attempts to create it if not found.
	/// @param symbol The symbol to match.
	/// @param err An error stream to write to.
	/// @return The symbol within the symbol table matching 'symbol' if one exists, 'symbol' if it was successfully added to the symbol table, and NULL if an error occurred adding the symbol.
	virtual Symbol* resolveOrCreate(Symbol* symbol, std::ostream* err = NULL) = 0;

};


}}}


