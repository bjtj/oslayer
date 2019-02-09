#include "stdafx.h"
#include "CppUnitTest.h"
#include <liboslayer/TestSuite.hpp>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace UnitTest
{		
	TEST_CLASS(UnitTest1)
	{
	public:
		
		TEST_METHOD(TestMethod1)
		{
			// TODO: 테스트 코드를 여기에 입력합니다.
			ASSERT('a', == , 'a');
		}

	};
}