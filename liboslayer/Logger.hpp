#ifndef __LOGGER_HPP__
#define __LOGGER_HPP__

#include <string>
#include <map>
#include <vector>
#include "AutoRef.hpp"
#include "Observer.hpp"

namespace UTIL {

	class LoggerFactory;
	class LogSession;

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
	 * log formatter
	 */
	class LogFormatter {
	public:
		LogFormatter();
		virtual ~LogFormatter();
		virtual std::string format(const LogSession & session, const std::string & msg) = 0;
	};
	

	/**
	 * log writer
	 */
	class LogWriter {
	public:
		LogWriter();
		virtual ~LogWriter();
		virtual void write(const LogSession & session, const std::string & msg) = 0;
	};
	

	/**
	 * log session
	 */
	class LogSession {
	private:
		bool _enable;
		LogLevel _level;
		OS::AutoRef<LogFormatter> _formatter;
		OS::AutoRef<LogWriter> _writer;
		
	public:
		LogSession(const LogLevel & level);
		LogSession(int level);
		virtual ~LogSession();
		void log(const std::string & msg) const;
		void enable(bool enable);
		bool enable() const;
		OS::AutoRef<LogFormatter> & formatter();
		OS::AutoRef<LogWriter> & writer();
		void formatter(OS::AutoRef<LogFormatter> formatter);
		void writer(OS::AutoRef<LogWriter> writer);
		void level(const LogLevel & level);
		LogLevel level() const;
	};
	

	/**
	 * logger
	 */
	class Logger : public Observer {
	private:
		std::string name;
		OS::AutoRef<LogSession> _fatal;
		OS::AutoRef<LogSession> _error;
		OS::AutoRef<LogSession> _warning;
		OS::AutoRef<LogSession> _info;
		OS::AutoRef<LogSession> _debug;
		OS::AutoRef<LogSession> _trace;
		OS::AutoRef<LogSession> _verbose;
		
	public:
		Logger(const std::string & name);
		virtual ~Logger();
		
	private:
		void _init();
		
	public:
		virtual void fatal(const std::string & msg) const;
		virtual void error(const std::string & msg) const;
		virtual void warning(const std::string & msg) const;
		virtual void info(const std::string & msg) const;
		virtual void debug(const std::string & msg) const;
		virtual void trace(const std::string & msg) const;
		virtual void verbose(const std::string & msg) const;
		OS::AutoRef<LogSession> & session(int level);
		virtual void onUpdate(Observable * target);
		void updateLogger(OS::AutoRef<Logger> logger);
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
		OS::AutoRef<Logger> makeLogger(LoggerFactory & factory, const std::string & name);
		OS::AutoRef<LogSession> makeSession(LoggerFactory & factory, int level);
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
		std::map<std::string, OS::AutoRef<LogFormatter> > _formatters;
		std::map<std::string, OS::AutoRef<LogWriter> > _writers;
		std::vector<LoggerProfile> _profiles;
		
	private:
		LoggerFactory();
		
	public:
		virtual ~LoggerFactory();
		static LoggerFactory & inst();
		OS::AutoRef<Logger> getObservingLogger(const std::string & name);
		OS::AutoRef<Logger> getLogger(const std::string & name);
		void setProfile(const std::string & pattern,
						const std::string & formatterName,
						const std::string & writerName);
		void setProfile(const LoggerProfile & descriptor);
		void registerFormatter(const std::string & name, OS::AutoRef<LogFormatter> formatter);
		void registerWriter(const std::string & name, OS::AutoRef<LogWriter> writer);
		OS::AutoRef<LogFormatter> formatter(const std::string & name);
		OS::AutoRef<LogWriter> writer(const std::string & name);
	};
}

#endif
