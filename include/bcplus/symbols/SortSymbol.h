#pragma once

#include <string>
#include <ostream>
#include <set>

#include <boost/property_tree/ptree.hpp>

#include "babb/utils/memory.h"
#include "memwrappers.h"

#include "bcplus/DomainType.h"
#include "bcplus/symbols/Symbol.h"

namespace bcplus {
namespace symbols {

class ObjectSymbol;
class Resolver;

class SortSymbol : public Symbol {
public:
	/*************************************************************************************/
	/* Public Types */
	/*************************************************************************************/

	/// List of object symbols in this sort
	typedef ReferencedSet<babb::utils::ref_ptr<const ObjectSymbol> >::type ObjectList;

	/// Object list iterators
	typedef ObjectList::iterator iterator;
	typedef ObjectList::const_iterator const_iterator;

	/// List of sorts
	typedef ReferencedSet<babb::utils::weak_ptr<SortSymbol> >::type SortList;

private:
	/*************************************************************************************/
	/* Private Members */
	/*************************************************************************************/
	/// The sort's objects
	babb::utils::ref_ptr<ObjectList> _objects;

	/// The sort's supersorts
	babb::utils::ref_ptr<SortList> _supersorts;

	/// The sort's subsorts
	babb::utils::ref_ptr<SortList> _subsorts;

	/// Information about what this sort is composed of
	DomainType::type _dt;


public:
	/*************************************************************************************/
	/* Constructors / Destructor */
	/*************************************************************************************/

	/// Basic constructor
	/// @param base The name of the sort
	/// @param objects The objects within the sort
	SortSymbol(ReferencedString const* name, ObjectList* objects = NULL, SortList* subsorts = NULL);

	/// Loads the sort from the property tree node
	/// @param node The node to load the symbol from
	/// @param err An error stream to write to (or NULL)
	/// Sets the symbol's good flag if it was successful
	/// NOTE: This just initializes the sort, it does not load any of the objects or subsorts, see {@ref loadDefinition}.
	SortSymbol(boost::property_tree::ptree const& node, std::ostream* err = NULL);

	/// Destructor stub
	virtual ~SortSymbol();
	
	/*************************************************************************************/
	/* Public Functions */
	/*************************************************************************************/

	/// Gets an iterator for the beginning of the object list
	inline const_iterator begin() const							{ return _objects->begin(); }

	/// Gets an iterator for the end of the object list
	inline const_iterator end() const							{ return _objects->end(); }

	/// Gets the number of elements in the sort
	inline size_t size() const									{ return _objects->size(); }

	/// Gets an iterator for the super sorts
	inline SortList::const_iterator beginSuperSorts() const		{ return _supersorts->begin(); }
	inline SortList::const_iterator endSuperSorts() const		{ return _supersorts->end(); }
	inline SortList::iterator beginSuperSorts() 				{ return _supersorts->begin(); }
	inline SortList::iterator endSuperSorts() 					{ return _supersorts->end(); }

	/// Get the number of registered super sorts
	inline size_t numSuperSorts() const							{ return _supersorts->size(); }

	/// Gets an iterator for the sub sorts
	inline SortList::const_iterator beginSubSorts() const		{ return _subsorts->begin(); }
	inline SortList::const_iterator endSubSorts() const			{ return _subsorts->end(); }
	inline SortList::iterator beginSubSorts() 					{ return _subsorts->begin(); }
	inline SortList::iterator endSubSorts() 					{ return _subsorts->end(); }

	/// Get the number of registered subsorts
	inline size_t numSubSorts() const							{ return _subsorts->size(); }

	/// Add an object symbol to the list of objects
	/// @return True if successful, false if the object was already in the sort.
	bool add(ObjectSymbol const* obj);

	/// Adds a sort symbol as a superset to this one.
	/// @param super The sort to add as a superset.
	/// @return True if successful, false if the sort was already a supersort of this one
	bool addSuperSort(SortSymbol* super);

	/// Adds a sort symbol as a subset to this one.
	/// @param sub The sort to add as a subset.
	/// @return True if successful, false if the sort was already a subsort of this one
	bool addSubSort(SortSymbol* sub);

	/// Completely loads the sort including initializing subsorts and member objects
	/// NOTE: This may fail if member objects refer to a sort not yet instantiated.
	/// @param node The property_tree node load the sort from.
	/// @param resolver A resolver used to resolve external references.
	/// @param err An error stream to write to.
	/// @return True if successful, false otherwise.
	bool loadDefinition(boost::property_tree::ptree const& node, Resolver* resolver, std::ostream* err = NULL);
	

	// inherited
	virtual bool operator==(Symbol const& other) const;
	virtual void save(boost::property_tree::ptree& node) const;
	virtual void outputDefinition(std::ostream& out) const;
	virtual DomainType::type domainType() const;

};

}}
