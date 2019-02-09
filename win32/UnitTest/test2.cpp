#include "stdafx.h"
#include "CppUnitTest.h"
#include <liboslayer/TestSuite.hpp>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

using namespace osl;

namespace UnitTest2
{
	TEST_CLASS(UnitTest2)
	{
	public:

		TEST_METHOD(TestMethod2)
		{
			// TODO: 테스트 코드를 여기에 입력합니다.

			ASSERT('a', == , 'a');
		}

	};
}