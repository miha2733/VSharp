using System;
using NUnit.Framework;
using VSharp.Test.Tests.Methods;

namespace VSharp.Test.Tests
{
    public class A
    {
        private int x, y;

        public A(int _x, int _y)
        {
            x = _x;
            y = _y;
        }
    }

    public struct B
    {
        private int x, y;

        public B(int _x, int _y)
        {
            x = _x;
            y = _y;
        }
    }

    [TestSvmFixture]
    public class InternalCalls<T, U>
        where U : struct
    {
        [TestSvm]
        public static Type RetType1(A a)
        {
            return a?.GetType();
        }

        [TestSvm]
        public static Type RetType2(B b)
        {
            return b.GetType();
        }

        [TestSvm]
        public static Type RetType3(T t)
        {
            if (t == null)
                return null;
            return t.GetType();
        }

        [TestSvm]
        public static Type RetType4(U u)
        {
            return u.GetType();
        }
    }
}