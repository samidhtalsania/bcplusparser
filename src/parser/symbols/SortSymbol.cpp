
#include <boost/algorithm/string.hpp>
#include <boost/property_tree/exceptions.hpp>

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
		for (SortSymbol* sort : _subsorts) {
			sort->addSuperset(this);
		}
	}

	_integral = true;
	for(ObjectSymbol const* obj : objects) {
		if (!obj->integral()) {
			_integral = false;
			break;
		}
	}
}

SortSymbol::SortSymbol(boost::property_tree::ptree const& node, std::ostream* err)
	: Symbol(node, err) {

	

	// Ensure arity = 0 for sorts
	if (good() && arity()) {
		good(false);
		if (err) *err << "ERROR: Sort \"" << *base() << "\" cannot be declared with non-zero arity." << std::endl;
	}

}

SortSymbol::~SortSymbol() {
	// Intentionally left blank
}

bool SortSymbol::add(ObjectSymbol const* obj) const {
	if (_objects->insert(obj).second) {
		// update any supersets
		for (SortSymbol* sort : _supersets) {
			sort->add(obj);
		}
		return true;
	} else return false;
}

bool SortSymbol::addSuperSort(SortSymbol* super) {
	if (_supersorts->insert(super).second) {
		super->addSubSort(this);
		// update the supersort's objects
		for (ObjectSymbol* obj : _objects) {
			super->add(obj);
		}
		return true;
	} else return false;
}

bool SortSymbol::addSubSort(SortSymbol* sub) {
	if (_subsorts->insert(sub).second) {
		sub->addSuperSort(this);
		return true;
	} else return false;
}

bool SortSymbol::loadDefinition(boost::property_tree::ptree const& node, Resolver* resolver, std::ostream* err) {
	// verify symbol name
	std::string node_name = node.get("<xmlattr>.name", "");
	int node_arity = node.get("<xmlattr>.arity", 0)'

	if (node_arity || node_name != *base()) {
		good(false);
		if (err) *err << "INTERNAL ERROR: Cannot load definition for sort \"" << *base() << "\". Identifier \"" << node_name << "/" << node_arity << "\" does not match this sort." << std::endl;
	} else {

		for (boost::property_tree::value_type const& n : node) {
			if (boost::iequals(n.first, "sort")) {
				// subsort
				ref_ptr<SortSymbol> subsort = new SortSymbol(node, err);
				if (subsort) {
					ref_ptr<SortSymbol> subsort_resolved = resolver->resolveOrCreate(subsort, err);
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
				ref_ptr<ObjectSymbol> obj = new ObjectSymbol(node, resolver, err);
				if (!obj || !obj->good()) {
					good(false);
					if (err) *err << "ERROR: An error occurred while scanning the definition of sort \"" << *base() << "\". Encountered a malformed object declaration." << std::endl;
				} else {
					// resolve it with previous definitions
					ref_ptr<ObjectSymbol> obj_resolved = resolver->resolveOrCreate(obj, err);
					if (!obj_resolved) {
						good(false);
						if (err) *err << "ERROR: An error occurred while scanning the definition of sort \"" << *base() << \". Encountered a conflicting definition of symbol \"" << *(obj->name()) << "\"." << std::endl;
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
				if (err) << "ERROR: Encountered an unexpected element \"" << n.first << "\" while scanning the definition of sort \"" << *base() << "\". Expected either \"sort\" or \"object\"." << std::endl;
			}
		}
	}

	return good();
}


void SortSymbol::save(boost::property_tree::ptree& node) const {
	Symbol::save(node);

	for (ObjectSymbol const* obj : *this) {
		boot::property_tree::ptree tmp;
		tmp.put("<xmlattr>.name",*(obj->base()));
		tmp.put("<xmlattr>.arity",obj->arity());
		node.add_child("object", tmp);
	}

	for (SortSymbol::const_iterator it = beginSubSorts(); it != endSubSorts(); it++) {
		boost::property_tree::ptree tmp;
		tmp.put("<xmlattr>.name", *(sort->base()));
		node.add_child("sort", tmp);
	}
}


}}}

