#include "stdafx.h"

#include "..\..\liboslayer\os.hpp"
#include "..\..\liboslayer\Date.hpp"

using namespace System;
using namespace System::Text;
using namespace System::Collections::Generic;
using namespace Microsoft::VisualStudio::TestTools::UnitTesting;

namespace TestProject1
{
	[TestClass]
	public ref class UnitTest1
	{
	private:
		TestContext^ testContextInstance;

	public: 
		/// <summary>
		///현재 테스트 실행에 대한 정보 및 기능을
		///제공하는 테스트 컨텍스트를 가져오거나 설정합니다.
		///</summary>
		property Microsoft::VisualStudio::TestTools::UnitTesting::TestContext^ TestContext
		{
			Microsoft::VisualStudio::TestTools::UnitTesting::TestContext^ get()
			{
				return testContextInstance;
			}
			System::Void set(Microsoft::VisualStudio::TestTools::UnitTesting::TestContext^ value)
			{
				testContextInstance = value;
			}
		};

		#pragma region Additional test attributes
		//
		//테스트를 작성할 때 다음 추가 특성을 사용할 수 있습니다.
		//
		//ClassInitialize를 사용하여 클래스의 첫 번째 테스트를 실행하기 전에 코드를 실행합니다.
		//[ClassInitialize()]
		//static void MyClassInitialize(TestContext^ testContext) {};
		//
		//ClassCleanup을 사용하여 클래스의 테스트를 모두 실행한 후에 코드를 실행합니다.
		//[ClassCleanup()]
		//static void MyClassCleanup() {};
		//
		//TestInitialize를 사용하여 각 테스트를 실행하기 전에 코드를 실행합니다.
		//[TestInitialize()]
		//void MyTestInitialize() {};
		//
		//TestCleanup을 사용하여 각 테스트를 실행하기 전에 코드를 실행합니다.
		//[TestCleanup()]
		//void MyTestCleanup() {};
		//
		#pragma endregion 

		[TestMethod]
		void TestMethod1()
		{
			unsigned long _t = OS::tick_milli();
			OS::idle(1000);
			Assert::AreEqual(true, (OS::tick_milli() - _t >= 1000 ? true : false));
		};

		[TestMethod]
		void TestDate()
		{
			OS::Date d;
			d.setYear(2017);
			d.setMonth(3 - 1);
			d.setDay(22);
			d.setHour(23);
			d.setMinute(33);
			d.setSecond(52);
			d.setGmtOffset(9 * 60);

			Assert::AreEqual(true, std::string("Wed, 22 Mar 2017 14:33:52 GMT") == OS::Date::formatRfc1123(d));
			Assert::AreEqual(true, std::string("Wednesday, 22-Mar-17 14:33:52 GMT") == OS::Date::formatRfc1036(d));
		};
	};
}
