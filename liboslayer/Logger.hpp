#ifndef __LOGGER_HPP__
#define __LOGGER_HPP__

#include <string>
#include <map>
#include <vector>
#include "AutoRef.hpp"
#include "Observer.hpp"

namespace osl {

	class LoggerFactory;
	class LogSession;
	class Logger;

	/**
	 * log level
	 */
	class LogLevel {
	public:
		static const int LEVEL_FATAL;
		static const int LEVEL_ERROR;
		static const int LEVEL_WARNING;
        static const int LEVEL_INFO;
        static const int LEVEL_DEBUG;
		static const int LEVEL_TRACE;
		static const int LEVEL_VERBOSE;
		
	private:
		int _level;
		
	public:
		LogLevel(int level);
		virtual ~LogLevel();
		int level() const;
		void level(int level);
		std::string shortName();
		std::string name();
		static std::string shortName(int level);
		static std::string name(int level);
		static void validate(int level);
		static std::vector<int> levels();
	};

	/**
	 * 
	 */
	class Log {
	private:
		LogLevel _level;
		std::string _name;
		std::string _msg;
	public:
		Log(const LogLevel & level, const std::string & name, const std::string & msg);
		Log(int level, const std::string & name, const std::string & msg);
		virtual ~Log();
		LogLevel & level();
		LogLevel level() const;
		std::string & name();
		std::string name() const;
		std::string & msg();
		std::string msg() const;
	};


	/**
	 * log formatter
	 */
	class LogFormatter {
	public:
		LogFormatter();
		virtual ~LogFormatter();
		virtual std::string format(const Log & log) = 0;
	};
	

	/**
	 * log writer
	 */
	class LogWriter {
	public:
		LogWriter();
		virtual ~LogWriter();
		virtual void write(const std::string & str) = 0;
	};
	

	/**
	 * log session
	 */
	class LogSession {
	private:
		osl::AutoRef<LogFormatter> _formatter;
		osl::AutoRef<LogWriter> _writer;
		
	public:
		LogSession();
		virtual ~LogSession();
		void log(const Log & log) const;
		std::string name() const;
		osl::AutoRef<LogFormatter> & formatter();
		osl::AutoRef<LogWriter> & writer();
		void level(const LogLevel & level);
		LogLevel level() const;
	};
	

	/**
	 * logger
	 */
	class Logger : public Observer {
	private:
		std::string _name;
		osl::AutoRef<LogSession> _fatal;
		osl::AutoRef<LogSession> _error;
		osl::AutoRef<LogSession> _warning;
		osl::AutoRef<LogSession> _info;
		osl::AutoRef<LogSession> _debug;
		osl::AutoRef<LogSession> _trace;
		osl::AutoRef<LogSession> _verbose;
		
	public:
		Logger(const std::string & name);
		virtual ~Logger();
		
	private:
		void _init();
		
	public:
		std::string & name();
		std::string name() const;
		virtual void fatal(const std::string & msg) const;
		virtual void error(const std::string & msg) const;
		virtual void warning(const std::string & msg) const;
		virtual void info(const std::string & msg) const;
		virtual void debug(const std::string & msg) const;
		virtual void trace(const std::string & msg) const;
		virtual void verbose(const std::string & msg) const;
		osl::AutoRef<LogSession> & session(int level);
		virtual void onUpdate(Observable * target);
		void updateLogger(osl::AutoRef<Logger> logger);
	};

	/**
	 * logger profile
	 */
	class LoggerProfile {
	private:
		std::string _pattern;
		std::map<int, std::string> _formatters;
		std::map<int, std::string> _writers;
		
	public:
		LoggerProfile();
		LoggerProfile(const std::string & pattern);
		virtual ~LoggerProfile();
		bool match(const std::string & keyword) const;
		osl::AutoRef<Logger> makeLogger(LoggerFactory & factory, const std::string & name);
		void setSession(LoggerFactory & factory, int level, osl::AutoRef<LogSession> session);
		void formatter(int level, const std::string & name);
		void writer(int level, const std::string & name);
		void allFormatters(const std::string & name);
		void allWriters(const std::string & name);
	};

	/**
	 * logger factory
	 */
	class LoggerFactory : public Observable {
	private:
		std::map<std::string, osl::AutoRef<LogFormatter> > _formatters;
		std::map<std::string, osl::AutoRef<LogWriter> > _writers;
		std::vector<LoggerProfile> _profiles;
		
	private:
		LoggerFactory();
		
	public:
		virtual ~LoggerFactory();
		static LoggerFactory & instance();
		osl::AutoRef<Logger> getObservingLogger(const char * name);
		osl::AutoRef<Logger> getObservingLogger(const std::string & name);
		osl::AutoRef<Logger> getLogger(const char * name);
		osl::AutoRef<Logger> getLogger(const std::string & name);
		void setProfile(const std::string & pattern,
						const std::string & formatterName,
						const std::string & writerName);
		void setProfile(const LoggerProfile & descriptor);
		void registerFormatter(const std::string & name, osl::AutoRef<LogFormatter> formatter);
		void registerWriter(const std::string & name, osl::AutoRef<LogWriter> writer);
		osl::AutoRef<LogFormatter> formatter(const std::string & name);
		osl::AutoRef<LogWriter> writer(const std::string & name);
	};
}

#endif
