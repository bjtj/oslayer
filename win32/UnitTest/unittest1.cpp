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
			// TODO: �׽�Ʈ �ڵ带 ���⿡ �Է��մϴ�.
			ASSERT('a', == , 'a');
		}

	};
}