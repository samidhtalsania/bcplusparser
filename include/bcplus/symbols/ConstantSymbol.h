#pragma once

#include <string>
#include <ostream>

#include <boost/property_tree/ptree.hpp>

#include "babb/utils/memory.h"

#include "bcplus/DomainType.h"
#include "bcplus/symbols/Symbol.h"
#include "bcplus/symbols/SortSymbol.h"
#include "bcplus/symbols/detail/BaseSymbol.h"

namespace bcplus {
namespace symbols {

class Resolver;

class ConstantSymbol : public detail::BaseSymbol {
public:
	/*************************************************************************************/
	/* Public Types */
	/*************************************************************************************/
	/// The different types a constant can be
	struct Type {
		enum type {
			ABACTION,
			ACTION,
			ADDITIVEFLUENT,
			ADDITIVEACTION,
			ATTRIBUTE,
			EXTERNALACTION,
			EXTERNALFLUENT,
			EXOGENOUSACTION,
			INERTIALFLUENT,
			RIGID,
			SDFLUENT,
			SIMPLEFLUENT
		};
	};

private:
	/*************************************************************************************/
	/* Private Members */
	/*************************************************************************************/
	/// The sort this constant ranges over
	babb::utils::ref_ptr<const SortSymbol> _sort;

	/// The constant type
	Type::type _type;

public:
	/// Basic constructor
	/// @param type The type that this constant is
	/// @param base The name of this object
	/// @param sort The sort that this constant ranges over
	/// @param args The sorts for each of the arguments for this object.
	ConstantSymbol(Type::type type, ReferencedString const* base, SortSymbol const* sort, SortList const* args = NULL);

	/// Loads the object from the property tree node
	/// @param node The node to load the symbol from
	/// @param err An error strem to write to (or NULL)
	/// Sets the symbol's good flag if it was successful
	ConstantSymbol(boost::property_tree::ptree const& node, Resolver const* resolver, std::ostream* err = NULL);

	/// Destructor stub
	virtual ~ConstantSymbol();

	/// Get the sort this symbol ranges over
	SortSymbol const* sort() const						{ return _sort; }

	/// Get the type that this constant is
	Type::type constType() const						{ return _type; }

	// inherited
	virtual bool operator==(Symbol const& other) const;
	virtual DomainType::type domainType() const;
	virtual void save(boost::property_tree::ptree& node) const;

};

}}
