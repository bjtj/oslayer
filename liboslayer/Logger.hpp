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
		static const int LEVEL_WARN;
        static const int LEVEL_INFO;
        static const int LEVEL_DEBUG;
		static const int LEVEL_TRACE;
		static const int LEVEL_VERBOSE;
	private:
		int level;
	public:
		LogLevel(int level);
		virtual ~LogLevel();
		int getLevel() const;
		void setLevel(int level);
		std::string getShortName();
		std::string getName();
		static std::string getShortName(int level);
		static std::string getName(int level);
		static void testLevel(int level);
		static std::vector<int> getLevels();
	};

	/**
	 * log formatter
	 */
	class LogFormatter {
	public:
		LogFormatter();
		virtual ~LogFormatter();
		virtual std::string format(const LogSession & session, const std::string & msg);
	};

	/**
	 * log printer
	 */
	class LogPrinter {
	public:
		LogPrinter();
		virtual ~LogPrinter();
		virtual void print(const LogSession & session, const std::string & msg);
	};

	/**
	 * log session
	 */
	class LogSession {
	private:
		bool enabled;
		LogLevel level;
		OS::AutoRef<LogFormatter> formatter;
		OS::AutoRef<LogPrinter> printer;
	public:
		LogSession(const LogLevel & level);
		LogSession(int level);
		virtual ~LogSession();
		void log(const std::string & msg) const;
		void setEnable(bool enable);
		bool isEnable();
		OS::AutoRef<LogFormatter> getFormatter();
		OS::AutoRef<LogPrinter> getPrinter();
		void setFormatter(OS::AutoRef<LogFormatter> formatter);
		void setPrinter(OS::AutoRef<LogPrinter> printer);
		LogLevel getLevel() const;
	};

	/**
	 * logger
	 */
	class Logger : public Observer {
	private:
		std::string name;
		OS::AutoRef<LogSession> _fatal;
		OS::AutoRef<LogSession> _error;
		OS::AutoRef<LogSession> _warn;
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
		virtual void logf(const std::string & msg) const;
		virtual void loge(const std::string & msg) const;
		virtual void logw(const std::string & msg) const;
		virtual void logi(const std::string & msg) const;
		virtual void logd(const std::string & msg) const;
		virtual void logt(const std::string & msg) const;
		virtual void logv(const std::string & msg) const;
		OS::AutoRef<LogSession> & session(int level);
		OS::AutoRef<LogSession> & fatal();
		OS::AutoRef<LogSession> & error();
		OS::AutoRef<LogSession> & warn();
		OS::AutoRef<LogSession> & info();
		OS::AutoRef<LogSession> & debug();
		OS::AutoRef<LogSession> & trace();
		OS::AutoRef<LogSession> & verbose();
		virtual void onUpdate(Observable * target);
		void updateLogger(OS::AutoRef<Logger> logger);
	};

	/**
	 * logger descriptor
	 */
	class LoggerDescriptor {
	private:
		std::string pattern;
		std::map<int, std::string> formatters;
		std::map<int, std::string> printers;
	public:
		LoggerDescriptor();
		LoggerDescriptor(const std::string & pattern);
		virtual ~LoggerDescriptor();
		bool match(const std::string & keyword) const;
		OS::AutoRef<Logger> makeLogger(LoggerFactory & factory, const std::string & name);
		OS::AutoRef<LogSession> makeSession(LoggerFactory & factory, int level);
		void setFormatter(int level, const std::string & name);
		void setPrinter(int level, const std::string & name);
		void setAllFormatter(const std::string & name);
		void setAllPrinter(const std::string & name);
	};

	/**
	 * logger factory
	 */
	class LoggerFactory : public Observable {
	private:
		std::map<std::string, OS::AutoRef<LogFormatter> > formatters;
		std::map<std::string, OS::AutoRef<LogPrinter> > printers;
		std::vector<LoggerDescriptor> descriptors;
	private:
		LoggerFactory();
	public:
		virtual ~LoggerFactory();
		static LoggerFactory & getInstance();
		OS::AutoRef<Logger> getObservingLogger(const std::string & name);
		OS::AutoRef<Logger> getLogger(const std::string & name);
		void setLoggerDescriptorSimple(const std::string & pattern, const std::string & formatterName, const std::string & printerName);
		void setLoggerDescriptor(const LoggerDescriptor & descriptor);
		void setFormatter(const std::string & name, OS::AutoRef<LogFormatter> formatter);
		void setPrinter(const std::string & name, OS::AutoRef<LogPrinter> writer);
		OS::AutoRef<LogFormatter> getFormatter(const std::string & name);
		OS::AutoRef<LogPrinter> getPrinter(const std::string & name);
	};
}

#endif
