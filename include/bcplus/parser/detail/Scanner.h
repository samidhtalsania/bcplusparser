#pragma once

#include <string>
#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "babb/utils/memory.h"

#include "bcplus/Location.h"
#include "bcplus/parser/Token.h"
#include "bcplus/parser/detail/ScannerSource.h"
#include "bcplus/parser/detail/ScannerTokenSource.h"
#include "bcplus/parser/detail/ScannerRawSource.h"

namespace bcplus  {
namespace parser  {
namespace detail {


class Scanner : public babb::utils::Referenced {
public:
	/***********************************************************/
	/* Public Types */
	/***********************************************************/
	/// A list of tokens.
	typedef ScannerTokenSource::TokenList TokenList;


private:
	/***********************************************************/
	/* Private Types */
	/***********************************************************/
	/// Condition type for the scanner
	enum YYCONDTYPE {
		yycNORMAL,
		yycSGL_STRING,
		yycDBL_STRING,
		yycCOMMENT,
		yycBLK_COMMENT,
		yycASP_GR,
		yycASP_CP,
		yycLUA_GR,
		yycLUA_CP,
		yycF2LP_GR,
		yycF2LP_CP
	};

	/***********************************************************/
	/* Private Members */
	/***********************************************************/

	/// Input sources
	std::list<babb::utils::ref_ptr<ScannerSource> > _sources;

	/// Scanner State
	YYCONDTYPE _cond;

	/// System-wide configuration information
	babb::utils::ref_ptr<Configuration> _config;
public:
	/***********************************************************/
	/* Constructors */
	/***********************************************************/
	/// @param config System wide configuration information
	Scanner(Configuration* config);

	/// Destructor
	virtual ~Scanner();

	/***********************************************************/
	/* Public Functions */
	/***********************************************************/

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

	/// Injects the provided token into the from of the stream being read by the scanner
	/// @param token The token to inject
	void push_front(Token const* token);
	
	/// Appends the provided token to the end of the stream being read by the scanner
	/// @param token The token to inject
	void push_back(Token const* token);

	/// Injects the provided tokens into the from of the stream being read by the scanner
	/// @param tokens The tokens to inject
	void push_front(TokenList* tokens);

	/// Appends the provided token to the end of the stream being read by the scanner
	/// @param tokens The tokens to inject
	void push_back(TokenList* tokens);

	/// Gets the number of input streams associated with this scanner
	inline size_t size() const									{ return _sources.size(); }

	/// Reads a token from the currently open file.
	/// @return The token that was read in.
	Token const* readToken();

	/// Gets the current location of the scanner
	inline Location loc() const									{ return (_sources.size() ? _sources.front()->loc() : Location(NULL, 0, 0)); }

	/// Closes all streams open by the scanner and resets its state
	void close();

private:

	/// Tokenizes the input from the provided raw source
	/// @param source The source input to tokenize
	Token const* tokenize(ScannerRawSource* source);

};

}}}

