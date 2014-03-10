#pragma once

#include <string>
#include <ostream>

#include <boost/algorithm/string.hpp>
#include <boost/property_tree/ptree.hpp>

#include "Referenced.h"
#include "pointers.h"
#include "referencedwrappers.h"

namespace bcplus {
namespace parser {
namespace symbols {

/**
 * @brief A base symbol class for building symbol tables.
 */
class Symbol : public Referenced
{
public:
	/************************************************************************/
	/* Public Types */
	/************************************************************************/
	/// Container for the types of symbols
	struct Type {
		/// Enumeration of possible values
		enum Value {
			SORT		= 0x0001,
			CONSTANT	= 0x0002,
			VARIABLE	= 0x0004,
			OBJECT		= 0x0008,
			MACRO		= 0x0010,
			_LARGEST_	= 0x0010,			///< placeholder for the largest type value	
			ERR_INVALID_SYMBOL = 0x1000		///< placeholder for invalid value
		};


		/// Interesting masks of multiple symbol types
		enum Mask {
			M_ANY		= ~0,
			M_TERM		= VARIABLE | OBJECT | MACRO | CONSTANT,
			M_FORMULA	= CONSTANT | MACRO
		};

		/// Conversion to a cstring
		static char const* cstr(Value v);

		/// Conversion from a cstring
		static Value val(char const* c);

	};



private:
	/************************************************************************/
	/* Private Members */
	/************************************************************************/
	Type::Value _type;
	size_t _arity;
	ref_ptr<const ReferencedString> _base;
	ref_ptr<const ReferencedString> _name;
	bool _good;


public:
	/************************************************************************/
	/* Static Functions */
	/************************************************************************/
	/// Generate the symbol's name from its base and arity
	static std::string genName(std::string const& base, size_t arity = 0);

	/************************************************************************/
	/* Constructor / Destructor */
	/************************************************************************/
	/// Basic Constructor
	/// @param type The type of symbol being instantiated
	/// @param base The base name of the symbol
	/// @param arity The arity of symbol
	Symbol(Type::Value type, ReferencedString const* base, size_t arity);
	
	/// Attempts to load the symbol from the property tree node.
	/// @param type The type of symbol being instantiated
	/// @param node The node to load the symbol from
	/// @param err An error stream to write to or NULL.
	/// Sets the symbol's good flag if it was successful.
	Symbol(Type::Value type, boost::property_tree::ptree const& node, std::ostream* err = NULL);

	/// Destructor stub
	virtual ~Symbol();
	
	/************************************************************************/
	/* Public Functions */
	/************************************************************************/



	/// Get the symbol type
	inline Type::Value type() const					{ return _type; }

	/// Get the symbol type as a cstring
	inline char const* typeString() const			{ return Type::cstr(type()); }

	/// Get the arity of the symbol
	inline size_t arity() const						{ return _arity; }

	/// Get the base name of the symbol
	inline ReferencedString const* base() const		{ return _base; }
	
	/// Get the full name of the symbol
	inline ReferencedString const* name() const		{ return _name; }

	/// Determines if the symbol has been successfully loaded.
	inline bool good() const						{ return _good; }

	/// Determine if this symbol matches the provided details
	/// @param b The base name to match against.
	/// @param a The arity to match against
	inline bool match(std::string const& b, size_t a) const
													{return (a == arity()) && (b == *base()); }
	/// Determine if this symbol matches the provided details
	/// @param n The full name of the symbol to match against (base/arity)
	inline bool match(std::string const&n) const	{ return n == *name(); }
	

	/// Check to see if two symbols are equal
	/// @param other The other symbol to compare this one to
	/// @return True if they are equal, false otherwise.
	virtual bool operator==(Symbol const& other) const;

	/// Outputs the symbol to the provided property tree node
	/// @param node The node to write to
 	virtual void save(boost::property_tree::ptree& node) const;

	/// Whether this symbol ranges over integers or is an integer
	virtual bool integral() const = 0;

protected:

	/// Set the good flag
	inline void good(bool l)						{ _good = l; }

};

}}}
