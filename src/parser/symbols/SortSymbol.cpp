
#include <boost/algorithm/string.hpp>
#include <boost/property_tree/exceptions.hpp>
#include <boost/foreach.hpp>

#include "parser/symbols/Resolver.h"
#include "parser/symbols/Symbol.h"
#include "parser/symbols/ObjectSymbol.h"
#include "parser/symbols/SortSymbol.h"


namespace bcplus {
namespace parser {
namespace symbols{


SortSymbol::SortSymbol(ReferencedString const* base, ObjectList* objects, SortList* subsorts)
	: Symbol(Symbol::Type::SORT, base, 0) {

	if (!objects) _objects = new ObjectList();
	else _objects = objects;

	_supersorts = new SortList();
	if (!subsorts) _subsorts = new SortList();
	else { 
		_subsorts = subsorts;
		BOOST_FOREACH(SortSymbol* sort, *_subsorts) {
			sort->addSupersort(this);
		}
	}

	_integral = true;
	BOOST_FOREACH(ObjectSymbol const* obj, *_objects) {
		if (!obj->integral()) {
			_integral = false;
			break;
		}
	}
}

SortSymbol::SortSymbol(boost::property_tree::ptree const& node, std::ostream* err)
	: Symbol(Symbol::Type::SORT, node, err) {

	_objects = new ObjectList();
	_supersorts = new SortList();
	_subsorts = new SortList();	

	// Ensure arity = 0 for sorts
	if (good() && arity()) {
		good(false);
		if (err) *err << "ERROR: Sort \"" << *base() << "\" cannot be declared with non-zero arity." << std::endl;
	}

}

SortSymbol::~SortSymbol() {
	// Intentionally left blank
}

bool SortSymbol::add(ObjectSymbol const* obj) {
	if (_objects->insert(obj).second) {
		// update any supersorts
		BOOST_FOREACH(SortSymbol* sort, *_supersorts) {
			sort->add(obj);
		}
		return true;
	} else return false;
}

bool SortSymbol::addSupersort(SortSymbol* super) {
	if (_supersorts->insert(super).second) {
		super->addSubsort(this);
		// update the supersort's objects
		BOOST_FOREACH(ObjectSymbol const* obj, *_objects) {
			super->add(obj);
		}
		return true;
	} else return false;
}

bool SortSymbol::addSubsort(SortSymbol* sub) {
	if (_subsorts->insert(sub).second) {
		sub->addSupersort(this);
		return true;
	} else return false;
}

bool SortSymbol::loadDefinition(boost::property_tree::ptree const& node, Resolver* resolver, std::ostream* err) {
	// verify symbol name
	std::string node_name = node.get("<xmlattr>.name", "");
	int node_arity = node.get("<xmlattr>.arity", 0);

	if (node_arity || node_name != *base()) {
		good(false);
		if (err) *err << "INTERNAL ERROR: Cannot load definition for sort \"" << *base() << "\". Identifier \"" << node_name << "/" << node_arity << "\" does not match this sort." << std::endl;
	} else {
		BOOST_FOREACH(boost::property_tree::ptree::value_type const& n, node) {
			if (boost::iequals(n.first, "subsort")) {
				// subsort
				ref_ptr<SortSymbol> subsort = new SortSymbol(n.second, err);
				if (subsort) {
					ref_ptr<SortSymbol> subsort_resolved = (SortSymbol*)resolver->resolveOrCreate(subsort);
					if (!subsort_resolved) {
						good(false);
						if (err) *err << "ERROR: An error occurred while scanning the definition of sort \"" << *base() << "\".  Subsort \"" << *(subsort->base()) << "\" is not a declared sort." << std::endl;
					} else addSubsort(subsort_resolved);
				} else {
					good(false);
					if (err) *err << "ERROR: An error occurred while scanning the definition of sort \"" << *base() << "\". Expected a 'name' attribute which was not provided in a subset declaration." << std::endl;
				}
			} else if (boost::iequals(n.first, "object")) {
				// object
				ref_ptr<ObjectSymbol> obj = new ObjectSymbol(n.second, resolver, err);
				if (!obj || !obj->good()) {
					good(false);
					if (err) *err << "ERROR: An error occurred while scanning the definition of sort \"" << *base() << "\". Encountered a malformed object declaration." << std::endl;
				} else {
					// resolve it with previous definitions
					ref_ptr<ObjectSymbol> obj_resolved = (ObjectSymbol*)resolver->resolveOrCreate(obj);
					if (!obj_resolved) {
						good(false);
						if (err) *err << "ERROR: An error occurred while scanning the definition of sort \"" << *base() << "\". Encountered a conflicting definition of symbol \"" << *(obj->name()) << "\"." << std::endl;
					} else {
						// Good to go. Add the object
						add(obj_resolved);
					}
				}
			} else if (n.first == "<xmlattr>") {
				// ignore xml attributes.
			} else {
				// who knows
				good(false);
				if (err) *err << "ERROR: Encountered an unexpected element \"" << n.first << "\" while scanning the definition of sort \"" << *base() << "\". Expected either \"subsort\" or \"object\"." << std::endl;
			}
		}
	}

	return good();
}

bool SortSymbol::operator==(Symbol const& other) const {
	// sort symbols are special as they have complex interaction with each other
	// a sort symbol is only equal to another if it's the same object
//	return this == &other;


	if (!Symbol::operator==(other)) return false;
/*
	// check objects
	{
		const_iterator oit = o.begin();
		for (const_iterator it = begin() ; it != end(); it++) {
			// Everything draws from the same set of symbols, so we can use pointer comparison here
			if (oit == o.end()) return false;
			if (*it != *oit) return false;
			oit++;
		}
		if (oit != o.end()) return false;
	}

	// check superset
	{
		SortList::const_iterator oit = o.beginSuperSorts();
		for (SortList::const_iterator it = beginSuperSorts() ; it != endSuperSorts(); it++) {
			// Everything draws from the same set of symbols, so we can use pointer comparison here
			if (oit == o.endSuperSorts()) return false;
			if (*it != *oit) return false;
			oit++;
		}
		if (oit != o.endSuperSorts()) return false;
	}

	// check subsets
	{
		SortList::const_iterator oit = o.beginSubSorts();
		for (SortList::const_iterator it = beginSubSorts() ; it != endSubSorts(); it++) {
			// Everything draws from the same set of symbols, so we can use pointer comparison here
			if (oit == o.endSubSorts()) return false;
			if (*it != *oit) return false;
			oit++;
		}
		if (oit != o.endSubSorts()) return false;
	} */
	return true;

}

void SortSymbol::save(boost::property_tree::ptree& node) const {
	Symbol::save(node);

	BOOST_FOREACH(ObjectSymbol const* obj, *_objects) {
		boost::property_tree::ptree& tmp = node.add("object", "");
		obj->save(tmp);
	}

	BOOST_FOREACH(SortSymbol const* sort, *_subsorts) {
		boost::property_tree::ptree& tmp = node.add("subsort", "");
		tmp.put("<xmlattr>.name", *(sort->base()));
	}
}

}}}
