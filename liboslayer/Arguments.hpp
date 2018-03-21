#ifndef __ARGUMENTS_HPP__
#define __ARGUMENTS_HPP__

#include <string>
#include <vector>

namespace OS {

	
	class ArgumentDeclaration {
	private:
		std::string _longname;
		std::string _shortname;
		bool _must;
		bool _optional;
		std::string _description;
	public:
		ArgumentDeclaration();
		virtual ~ArgumentDeclaration();
		std::string & longname();
		std::string & shortname();
		bool & must();
		bool & optional();
		std::string & description();

		static ArgumentDeclaration mustFull(const std::string & longname,
											const std::string & shortname);
		static ArgumentDeclaration mustFull(const std::string & longname,
											const std::string & shortname,
											const std::string & description);
		static ArgumentDeclaration mustLong(const std::string & longname);
		static ArgumentDeclaration mustLong(const std::string & longname,
											const std::string & description);
		static ArgumentDeclaration mustShort(const std::string & shortname);
		static ArgumentDeclaration mustShort(const std::string & shortname,
											 const std::string & description);

		static ArgumentDeclaration optionalFull(const std::string & longname,
												const std::string & shortname);
		static ArgumentDeclaration optionalFull(const std::string & longname,
												const std::string & shortname,
												const std::string & description);
		static ArgumentDeclaration optionalLong(const std::string & longname);
		static ArgumentDeclaration optionalLong(const std::string & longname,
												const std::string & description);
		static ArgumentDeclaration optionalShort(const std::string & shortname);
		static ArgumentDeclaration optionalShort(const std::string & shortname,
												 const std::string & description);
	};


	class Argument {
	private:
		std::string _name;
		std::string _str;
	public:
		Argument();
		Argument(const std::string & name);
		Argument(const std::string & name, const std::string & str);
		virtual ~Argument();
		std::string & name();
		std::string & str();
		int to_int();
		long to_long();
	};



	class Arguments {
	private:
		std::string _program;
		std::vector<Argument> _vec_named;
		std::vector<std::string> _texts;
	public:
		Arguments();
		virtual ~Arguments();
		std::string & program();
		std::vector<Argument> & vec_named();
		Argument named(const std::string & name);
		bool is_set(const std::string & name);
		void set(const std::string & name);
		void set(const std::string & name, const std::string & str);
		std::vector<std::string> & texts();
	};


	class ArgumentParseError {
	private:
	public:
		ArgumentParseError();
		virtual ~ArgumentParseError();
	};


	class ArgumentValidation {
	private:
	public:
		class ErrorHandler {
		private:
		public:
			ErrorHandler();
			virtual ~ErrorHandler();
			virtual bool onError(const ArgumentParseError & error) = 0;
		};

	public:
		ArgumentValidation();
		virtual ~ArgumentValidation();
		void optional(const ArgumentDeclaration & decl);
	};


	class ArgumentParser {
	private:
	public:
		ArgumentParser();
		virtual ~ArgumentParser();
		static Arguments parse(int argc, char * argv[]);
		static Arguments parse(int argc, char * argv[], const ArgumentValidation & validation);
	private:
		static bool is_inline(const std::string & str);
		static bool is_longname(const std::string & str);
		static bool is_shortname(const std::string & str);
		static std::string get_longname(const std::string & str);
		static std::string get_shortname(const std::string & str);
	};

}

#endif
