#pragma once
#include <string>
#include <iostream>

#ifndef BOOST_FILESYSTEM_VERSION
#define BOOST_FILESYSTEM_VERSION 3
#endif
#include <boost/filesystem.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/null.hpp>

#include "Referenced.h"

/// Container for option enumeration information
struct Option {
	enum Value {
		MOD_FILE,			///< Specify the XML file providing the module definitions.
		VERIFY,				///< Sets whether we should verify the module definitions.
		STATE_DIR,			///< Specify the directory to write the state files in.
		INPUT_FILE,			///< Specify the file containing the input events.
		VERBOSITY,			///< Specify the application verbosity level.
		HELP,				///< Show help dialog.
		VERSION,			///< Show the application version
		BAD					///< Unknown option.
	};

	enum Status {
		STAT_OK,		///< Everything is a-ok.
		STAT_BAD_ARG,	///< The user provided an option we don't recognize.
		STAT_HELP,		///< The user needs help.
		STAT_VERSION	///< The user wants the application version.
	};

};

/// Container for verbosity levels
struct Verb {

	enum Level {
		ERROR = 0,			/// Error Messages
		STD = 1,			/// Standard Messages
		OP = 2,				/// Operation-level messages
		DETAIL = 3			/// Detailed debug Messages
	};
};


namespace fs = boost::filesystem;
namespace ios = boost::iostreams;

class Configuration : public Referenced
{
public:
	/***************************************************************************/
	/* Constants */
	/***************************************************************************/
	/// Default state directory value
	static const fs::path DEFAULT_STATE_DIR;

	/// Default module verification value
	static const bool DEFAULT_VERIFY_MODULES;

	/// Default verbosity level
	static const size_t DEFAULT_VERB_LEVEL;

	/// Application version
	static const std::string VERSION;

private:

	/***************************************************************************/
	/* Private Members */
	/***************************************************************************/
	std::string _version;					///< The version of this application.
	std::string _bin_name;					///< The name of the executable.
	fs::path _mod_file;						///< The file that defines the modules
	bool _verify;							///< Whether we should attempt to verify module integrity.
	fs::path _state_dir;					///< The directory used to store the state files in.
	fs::path _input;						///< The file containing the event inputs for this run.
	size_t _verb;							///< The user defined verbosity level.	


	ios::stream<ios::null_sink>	_nullstream;///< Output stream to a null device in the event the verbosity is squelched.


public:
	/***************************************************************************/
	/* Constructors */
	/***************************************************************************/
	/// Default constructor
	/// Initializes all parameters to defaults.
	/// @param version The version of this application
	Configuration(std::string const& version);

	/// Destructor
	virtual ~Configuration();


	/***************************************************************************/
	/* Public Methods */
	/***************************************************************************/
	/// Loads configuration parameters from the provided arguments list.
	/// @param argc The number of arguments.
	/// @param argv An array of these arguments.
	/// @return The status of the load
	Option::Status load(int argc, char const** argv);

	/// Verifies the integrity of the configuration parameters.
	/// @return True if the parameters appear to be correct. False otherwise.
	bool verify();

	// -------------------------------------------------------------------------

	/// Get the application version
	inline std::string const& version() const					{ return _version; }

	/// Get the name of this executable file
	inline std::string const& binaryName() const				{ return _bin_name; }
	inline void binaryName(std::string const& name)				{ _bin_name = name; }	

	inline fs::path const& moduleFile() const					{ return _mod_file; }
	inline void moduleFile(fs::path const& p) 					{ _mod_file = p; }

	inline bool verifyModules() const							{ return _verify; }
	inline void verifyModule(bool verify) 						{ _verify = verify; }

	inline fs::path const& stateDirectory() const				{ return _state_dir; }
	inline void stateDirectory(fs::path const& p)				{ _state_dir = p; }

	inline fs::path const& inputFile() const					{ return _input; }
	inline void inputFile(fs::path const& p)					{ _input = p; }


	/// Get/set the application verbosity level
	inline size_t verbosity() const								{ return _verb; }
	inline void verbosity(size_t verb)							{ _verb = verb; }

	/// Get the output stream for the provided verbosity level
	/// This will return a null-sink, std::cerr, or std::cout.
	std::ostream& ostream(Verb::Level v);

	// --------------------------------------------------------------------------
	/// Outputs the current configuration to a stream.
	/// @param out The output stream.
	/// @return The output stream.
	std::ostream& output(std::ostream& out) const;

	/// Outputs the application version information.
	/// @param out The output stream to write to.
	/// @return out
	std::ostream& outputVersion(std::ostream& out) const;

	/// Outputs a help dialog to the provided output stream.
	/// @param out The output stream to write the help dialog to.
	/// @return out
	std::ostream& outputHelp(std::ostream& out) const;


private:
	/***************************************************************************/
	/* Private Methods */
	/***************************************************************************/
	/// Parses the string and determines the command line option it contains.
	/// @param opt The option string to parse.
	/// @param[out] val The value the option is set to, where applicable.
	/// @return The option contained within the string or BAD.
	Option::Value parseOption(char const* opt, char const*& val);

};


