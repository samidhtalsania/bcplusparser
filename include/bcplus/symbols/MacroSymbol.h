#pragma once

#include <string>
#include <ostream>

#include <boost/property_tree/ptree.hpp>

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/elements/Element.h"
#include "bcplus/symbols/Symbol.h"

namespace bcplus {
namespace symbols {

class MacroSymbol : public Symbol {
public:
	/****************************************************************************/
	/* Public Types */
	/****************************************************************************/
	typedef ReferencedList<babb::utils::ref_ptr<const ReferencedString> >::type ArgList;
	typedef ArgList::iterator iterator;
	typedef ArgList::const_iterator const_iterator;

	typedef ReferencedList<babb::utils::ref_ptr<const elements::Element> >::type ElementList;

private:
	/****************************************************************************/
	/* Private Members */
	/****************************************************************************/
	/// List of expansion arguments
	babb::utils::ref_ptr<const ArgList> _args;
	
	/// The text to expand
	babb::utils::ref_ptr<const ReferencedString> _text;

public:
	/****************************************************************************/
	/* Constructors / Destructors */
	/****************************************************************************/
	/// Initializes the macro symbol
	/// @param base The base name for the macro.
	/// @param text The text that will be used as an expansion template.
	/// @param args formal arguments for the macro to be expanded (or NULL for no arguments).
	MacroSymbol(ReferencedString const* base, ReferencedString const* text, ArgList const* args = NULL);

	/// Destructor
	virtual ~MacroSymbol();

	/****************************************************************************/
	/* Public Functions */
	/****************************************************************************/


	/// Iterate over the formal arguments
	inline const_iterator begin() const							{ return _args->begin(); }
	inline const_iterator end() const							{ return _args->end(); }


	/// Get the macro definition text
	inline ReferencedString const* text() const					{ return _text; }

	/// Expand the macro given the provided arguments list
	std::string expand(ArgList const* args = NULL) const;
	std::string expand(ElementList const* args) const;

	virtual bool integral() const;
};

}}
