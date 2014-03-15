	// Intentionally left blank
#pragma once
#include <list>
#include <set>
#include <map>
#include <iostream>

#include <boost/filesystem.hpp>

#include "babb/utils/memory.h"



#include "bcplus/Configuration.h"
#include "bcplus/languages/Language.h"
#include "bcplus/parser/detail/Scanner.h"
#include "bcplus/statements/Statement.h"
#include "bcplus/symbols/SymbolTable.h"
#include "bcplus/elements/Element.h"

namespace bcplus {
namespace parser {

/// A parser which allows us to scan a BC+ program
class BCParser : public babb::utils::Referenced {
public:
	/***********************************************************************************/
	/* Types */
	/***********************************************************************************/
	/// Container for status codes
	struct Status {
		enum Value {
			OK,				///< Everything is fine and dandy
			SYNTAX_ERR,		///< A syntax error has occurred
			IO_ERR,
			SOFT_EOF,		///< The end of input has been reached by the scanner, the parser may still accept additional input
			HARD_EOF		///< The end of input has been reached by the parser. No more input may be accepted.
		};
	};

	/// The type returned by the parse function
	typedef std::pair<Status::Value, babb::utils::ref_ptr<const statements::Statement> > ParseType;

private:
	/***********************************************************************************/
	/* Members */
	/***********************************************************************************/

	/// System wide configuration information
	babb::utils::ref_ptr<Configuration> _config;

	/// The scanner object we're reading from
	babb::utils::ref_ptr<detail::Scanner> _scanner;

	/// The symbol table 
	babb::utils::ref_ptr<symbols::SymbolTable> _symtab;

	/// Information about the language we're parsing
	babb::utils::ref_ptr<const languages::Language> _lang;

	/// The lemon internal parser object
	void* _parser;

	/// Temporary storage for a statement that's been parsed
	babb::utils::ref_ptr<const statements::Statement> _stmt;

	/// Our current parse status
	Status::Value _stat;

	/// Whether we've just encountered a soft end of file
	bool _soft_eof;

	/// The last token we passed to lemon (or NULL)
	babb::utils::weak_ptr<const Token> _last_token;

public:
	/***********************************************************************************/
	/* Constructors */
	/***********************************************************************************/
	/// @param config The system-wide configuration information 
	/// @param lang The language specification that we will be enforcing
	/// @param symtab The symbol table to use, or NULL to create one using the configuration options.
	BCParser(Configuration* config, languages::Language const* lang, symbols::SymbolTable* symtab = NULL);

	/// Destructor
	virtual ~BCParser();

	/***********************************************************************************/
	/* Public Functions */
	/***********************************************************************************/

	/// Attempts to parse the next rule from the scanner.
	/// @return A status code and the parsed rule if the status is OK, or NULL otherwise.
	ParseType parse();

	/// Resets the parser clearing all previous input
	void reset();

    /// Attempts to open the provided file and inject it into the front of the stream being read by the scanner.
    /// @param file The file to read from.
    /// @param squelch Whether to silence all errors resulting from this call.
    /// @return True if successful, false otherwise.
    bool push_front(boost::filesystem::path const& file, bool squelch = false);

    /// Attempts to open the provided file and append it to the end of the stream being read by the scanner.
    /// @param file The file to read from.
    /// @param squelch Whether to silence all errors resulting from this call.
    /// @return True if successful, false otherwise.
    bool push_back(boost::filesystem::path const& file, bool squelch = false);

    /// Injects the provided null-terminated buffer into the front of the stream being read by the scanner.
    /// @param buffer The null-terminated buffer to read from.
    /// @param loc The location to display in messages partaining to this buffer.
    void push_front(char const* buffer, Location const& loc = Location(NULL, 0, 0));

    /// Appends the provided null-terminated buffer to the end of the stream being read by the scanner.
    /// @param buffer The null-terminated buffer to read from.
    /// @param loc The location to display in messages partaining to this buffer.
    void push_back(char const* buffer, Location const& loc = Location(NULL, 0, 0));

	/// Gets the symbol table for the parser
	inline symbols::SymbolTable* symtab()						{ return _symtab; }
	inline symbols::SymbolTable const* symtab() const			{ return _symtab; }

	/// Get the language specification being enforced.
	inline languages::Language const* lang() const				{ return _lang; }
 
	/// Get the configuration information
	inline Configuration const* config() const					{ return _config; }

	/***********************************************************************************/
	/* Stuff Thou Shall not Call */
	/***********************************************************************************/

	/// INTERNAL FUNCTION
	/// Reports the use of an unsupported feature.
	/// @param feature The unsupported feature that was used.
	void _feature_error(languages::Language::Feature::Value feature);

	/// INTERNAL FUNCTION
	/// Registers a parse error.
	/// @param err The error to register
	void _parse_error(std::string const& err);

	elements::Element const* _resolve(ReferencedString const* name, size_t arity);

	void _handle_stmt(statements::Statement const* stmt);	

private:

	/// Prepares the parser for an input to be injected at its current location
	/// @param popState whether the top of the parser's state stack should be popped in the process.
	void preInjectPrep(bool popState = false);

};


}}


