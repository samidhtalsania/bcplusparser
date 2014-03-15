#pragma once

#include <string>
#include <map>
#include <ostream>

#include <boost/filesystem/path.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/type_traits/remove_const.hpp>

#include "babb/utils/memory.h"

#include "bcplus/Configuration.h"
#include "bcplus/symbols/Symbol.h"
#include "bcplus/symbols/Resolver.h"

namespace bcplus  {
namespace symbols {

/// The bcplus's symbol table
class SymbolTable : public Resolver, public babb::utils::Referenced {
public:

	/// Simple functor to get the second element in a pair.
	struct get_second {
		/// this defines the return type of this functor
		template<typename> struct result;
		template < typename F, typename T >
		struct result<F(T)> {
			typedef typename boost::remove_reference<T>::type::second_type type;
		};

		template <typename K, typename V>
		V& operator() ( std::pair<K,V>& pr ) {
			return pr.second;
		}

		template <typename K, typename V>
		V const& operator() ( std::pair<K,V> const& pr ) const {
			return pr.second;
		}
	};



	/*******************************************************************************/
	/* Public Types */
	/*******************************************************************************/
	typedef std::map<std::string,babb::utils::ref_ptr<Symbol> > SymbolMap;
	typedef std::map<Symbol::Type::Value, SymbolMap> TypeMap;

	typedef boost::transform_iterator<get_second, SymbolMap::iterator> iterator;
	typedef boost::transform_iterator<get_second, SymbolMap::const_iterator> const_iterator;

private:
	/*******************************************************************************/
	/* Private Members */
	/*******************************************************************************/

	/// Master symbol table mapping.
	TypeMap _symbols;

	/// The system configuration information
	babb::utils::ref_ptr<Configuration> _config;

	/// Whether the symbol table has been successfully loaded.
	bool _good;

public:

	/*******************************************************************************/
	/* Constructors / Destructor */
	/*******************************************************************************/
	/// Creates a symbol table based on the provided configuration information.
	/// @param config The system-wide configuration information to read from.
	SymbolTable(Configuration* config);

	/// Destructor for the symbol table. 
	/// Saves the table state according to the configuration information.
	virtual ~SymbolTable();
	
	/*******************************************************************************/
	/* Public Functions */
	/*******************************************************************************/


	/// Iterate over the list of symbols matching the provided type
	inline iterator begin(Symbol::Type::Value type) 							{ return iterator(_symbols[type].begin()); }
	inline const_iterator begin(Symbol::Type::Value type) const					{ return const_iterator(_symbols.find(type)->second.begin()); }

	inline iterator end(Symbol::Type::Value type)								{ return iterator(_symbols[type].end()); }
	inline const_iterator end(Symbol::Type::Value type) const					{ return const_iterator(_symbols.find(type)->second.end()); }
	
	/// Determines if the symbol table was successfully loaded
	inline bool good() const													{ return _good; }


	// Inherited from Resolver
	virtual Symbol const* resolve(size_t typemask, std::string const& name, size_t arity = 0) const;
	virtual Symbol* resolve(size_t typemask, std::string const& name, size_t arity = 0);
	virtual bool create(Symbol* symbol);
	virtual Symbol* resolveOrCreate(Symbol* symbol);

private:
	/*******************************************************************************/
	/* Private Functions */
	/*******************************************************************************/


	/// Attempts to load the symbol table from the provided file
	/// @param path The file to load from.
	/// @return True if successful, false otherwise.
	bool load(boost::filesystem::path const& path);

	/// Attempts to save the symbol table to the provided file.
	/// @param path The file to save to.
	/// @return True if successful, false otherwise.
	bool save(boost::filesystem::path const& path) const;


};

}}


