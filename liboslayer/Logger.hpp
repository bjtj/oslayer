#ifndef __LOGGER_HPP__
#define __LOGGER_HPP__

#include <vector>
#include <string>

namespace UTIL {

	/**
	 * @brief LogLevel
	 */
	class LogLevel {
	public:
		static const int ERROR_LEVEL = 0;
		static const int WARN_LEVEL = 1;
		static const int INFO_LEVEL = 2;
		static const int DEBUG_LEVEL = 3;
		static const int VERBOSE_LEVEL = 4;

	private:
		int level;

	public:
		LogLevel();
		LogLevel(int level);
		virtual ~LogLevel();

		std::string toString() const;
		std::string toMinialString() const;
	};

	/**
	 * @brief LogMessage
	 */
	class LogMessage {
	private:
		LogLevel level;
		std::string message;
	public:
		LogMessage();
		LogMessage(int level, const std::string & message);
		virtual ~LogMessage();

		void setLogLevel(const LogLevel & level);
		void setMessage(const std::string & message);

		std::string toString() const;
	};


	/**
	 * @brief LogMessagePrinter
	 */
	class LogMessagePrinter {
	private:
	public:
		LogMessagePrinter();
		virtual ~LogMessagePrinter();

		virtual std::string print(const LogMessage & message);
	};

	/**
	 * @brief LogMessagePrinter Decorator
	 */
	class LogMessagePrinterDecorator : public LogMessagePrinter {
	private:
		LogMessagePrinter & printer;
	public:
		LogMessagePrinterDecorator(LogMessagePrinter & printer);
		virtual ~LogMessagePrinterDecorator();
	};


	/**
	 * @brief LogMessageWriter
	 */
	class LogMessageWriter {
	private:
	public:
		LogMessageWriter();
		virtual ~LogMessageWriter();

		virtual void write(const std::string & message) = 0;
	};

	/**
	 * @brief LogMessageWriter Decorator
	 */
	class LogMessageWriterDecorator : public LogMessageWriter {
	private:
		LogMessageWriter & writer;
	public:
		LogMessageWriterDecorator(LogMessageWriter & writer);
		virtual ~LogMessageWriterDecorator();
	};

	/**
	 * @brief Standard IO Log Message Writer (cout)
	 */
	class StandardIOLogMessageWriter : public LogMessageWriter {
	public:
		StandardIOLogMessageWriter();
		virtual ~StandardIOLogMessageWriter();
		virtual void write(const std::string & message);
	};

	/**
	 * @brief Logger
	 */
	class Logger {
	private:
		LogMessagePrinter * printer;
		std::vector<LogMessageWriter *> writers;
	public:
		Logger();
		virtual ~Logger();

		virtual void log(const LogMessage & msg) const;
		void log(int level, const std::string & msg) const;
		void loge(const std::string & msg) const;
		void logw(const std::string & msg) const;
		void logi(const std::string & msg) const;
		void logd(const std::string & msg) const;
		void logv(const std::string & msg) const;

		void writeMessage(const std::string & msg) const;

		void setPrinter(LogMessagePrinter * printer);

		void addWriter(LogMessageWriter * writer);
		void removeWriter(LogMessageWriter * writer);
	};

	/**
	 * @brief Logger Factory
	 */
	class LoggerFactory {
	private:
	private:
		LoggerFactory();
	public:
		virtual ~LoggerFactory();

		static const Logger & getDefaultLogger();
	};
}

#endif