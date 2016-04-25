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

	class LogLevel {
	public:
		static const int FATAL_LEVEL;
		static const int ERROR_LEVEL;
		static const int WARN_LEVEL;
        static const int INFO_LEVEL;
        static const int DEBUG_LEVEL;
		static const int TRACE_LEVEL;
		static const int VERBOSE_LEVEL;
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

	class LogFormatter {
	public:
		LogFormatter();
		virtual ~LogFormatter();
		virtual std::string format(const LogSession & session, const std::string & msg);
	};

	class LogPrinter {
	public:
		LogPrinter();
		virtual ~LogPrinter();
		virtual void print(const LogSession & session, const std::string & msg);
	};


	class LogSession {
	private:
		bool enabled;
		LogLevel level;
		AutoRef<LogFormatter> formatter;
		AutoRef<LogPrinter> printer;
	public:
		LogSession(const LogLevel & level);
		LogSession(int level);
		virtual ~LogSession();
		void log(const std::string & msg) const;
		void setEnable(bool enable);
		bool isEnable();
		AutoRef<LogFormatter> getFormatter();
		AutoRef<LogPrinter> getPrinter();
		void setFormatter(AutoRef<LogFormatter> formatter);
		void setPrinter(AutoRef<LogPrinter> printer);
		LogLevel getLevel() const;
	};



	class Logger : public Observer {
	private:
		LoggerFactory * factory;
		std::string name;
		AutoRef<LogSession> _fatal;
		AutoRef<LogSession> _error;
		AutoRef<LogSession> _warn;
		AutoRef<LogSession> _info;
		AutoRef<LogSession> _debug;
		AutoRef<LogSession> _trace;
		AutoRef<LogSession> _verbose;
	public:
		Logger(LoggerFactory * factory, const std::string & name);
		Logger(const std::string & name);
		virtual ~Logger();
		virtual void logf(const std::string & msg) const;
		virtual void loge(const std::string & msg) const;
		virtual void logw(const std::string & msg) const;
		virtual void logi(const std::string & msg) const;
		virtual void logd(const std::string & msg) const;
		virtual void logt(const std::string & msg) const;
		virtual void logv(const std::string & msg) const;
		AutoRef<LogSession> & session(int level);
		void observe(LoggerFactory * factory);
		virtual void update(Observable * target);
	};

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
		AutoRef<Logger> makeLogger(LoggerFactory & factory, const std::string & name);
		AutoRef<LogSession> makeSession(LoggerFactory & factory, int level);
		void setFormatter(int level, const std::string & name);
		void setPrinter(int level, const std::string & name);
		void setAllFormatter(const std::string & name);
		void setAllPrinter(const std::string & name);
	};


	class LoggerFactory : public Observable {
	private:
		std::map<std::string, AutoRef<LogFormatter> > formatters;
		std::map<std::string, AutoRef<LogPrinter> > printers;
		std::vector<LoggerDescriptor> descriptors;
	private:
		LoggerFactory();
	public:
		virtual ~LoggerFactory();
		static LoggerFactory & getInstance();
		AutoRef<Logger> getLogger(const std::string & name);
		AutoRef<Logger> getLoggerWithoutObserve(const std::string & name);
		void setLoggerDescriptorSimple(const std::string & pattern, const std::string & formatterName, const std::string & printerName);
		void setLoggerDescriptor(const LoggerDescriptor & descriptor);
		void setFormatter(const std::string & name, AutoRef<LogFormatter> formatter);
		void setPrinter(const std::string & name, AutoRef<LogPrinter> writer);
		AutoRef<LogFormatter> getFormatter(const std::string & name);
		AutoRef<LogPrinter> getPrinter(const std::string & name);
	};
}

#endif
