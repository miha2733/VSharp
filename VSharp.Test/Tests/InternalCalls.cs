using System;
using NUnit.Framework;
using VSharp.Test.Tests.Methods;

namespace VSharp.Test.Tests
{
    public class C {}
    public class A {}

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
    public static class InternalCalls<T, U, W>
        where U : struct
        where T : class
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

        [TestSvm]
        public static bool CompareTypesClass(A a, T b)
        {
            if (a == null || b == null) return false;
            Type t1 = a.GetType();
            Type t2 = b.GetType();
            return t1 == t2;
        }

        [TestSvm]
        public static bool CompareTypes(A a, W b)
        {
            if (a == null || b == null) return false;
            Type t1 = a.GetType();
            Type t2 = b.GetType();
            return t1 == t2;
        }

        [TestSvm]
        public static bool CompareTypesSymbolic(Type t, T b)
        {
            if (b == null || t == null) return false;
            Type t2 = b.GetType();
            return t == t2;
        }
    }

    [TestSvmFixture]
    public static class InternalCallTests
    {
        [TestSvm]
        public static bool CompareTypesClass_Test()
        {
            A a = new A();
            return InternalCalls<A, B, A>.CompareTypesClass(a, a);
        }

        [TestSvm]
        public static bool CompareTypesClass_Test2()
        {
            A a = new A();
            C c = new C();
            return InternalCalls<C, B, A>.CompareTypesClass(a, c);
        }

        [TestSvm]
        public static bool CompareTypesSymbolic_Test1()
        {
            Type t = typeof(A);
            C c = new C();
            return InternalCalls<C, B, A>.CompareTypesSymbolic(t, c);
        }

        [TestSvm]
        public static bool CompareTypesSymbolic_Test2()
        {
            Type t = typeof(C);
            C c = new C();
            return InternalCalls<C, B, A>.CompareTypesSymbolic(t, c);
        }

        [TestSvm]
        public static bool TypeIsA(Type t)
        {
            if (t == null) return false;
            Type t2 = typeof(A);
            return t == t2;
        }

        [TestSvm]
        public static bool AIsA()
        {
            return TypeIsA(typeof(A));
        }

        [TestSvm]
        public static bool CIsNotA()
        {
            return TypeIsA(typeof(C));
        }
    }
}