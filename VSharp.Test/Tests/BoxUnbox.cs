using NUnit.Framework;
using VSharp.Test.Tests.Methods;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class UnboxAny
    {
        public class A
        {
            private int x;

            public A()
            {
                x = 123;
            }
        }

        public struct B
        {
            private double x;

            public B(double z)
            {
                x = z;
            }
        }

        private static bool IsCast<T>(object o)
        {
            return o is T;
        }

        private static T AsCast<T> (object o) where T : class
        {
            return o as T;
        }

        private static T Cast<T> (object o)
        {
            return (T) o;
        }

        // [TestSvm]
        // [Ignore("primitive cast: unreachable")]
        public static object UnboxAny1() // TODO: check #do
        {
            var b = new B(5);
            return Cast<B>(b);
        }

        [Ignore("Exceptions handling")]
        public static object UnboxAny2()
        {
            var b = new B(5);
            return Cast<A>(b);
        }

        [Ignore("Exceptions handling")]
        public static object UnboxAny3()
        {
            var a = new A();
            return Cast<B>(a);
        }

        [Ignore("primitive cast: unreachable")]
        public static object UnboxAny4()
        {
            var a = new A();
            return Cast<A>(a);
        }

        [Ignore("primitive cast: unreachable")]
        public static object UnboxAny5()
        {
            var a = new int[] {1, 2, 3};
            return Cast<uint[]>(a);
        }

        [Ignore("primitive cast: unreachable")]
        public static object UnboxAny6()
        {
            var a = new uint[] {1, 2, 3};
            return Cast<int[]>(a);
        }
    }

    [TestSvmFixture]
    public class BoxUnbox
    {
        [TestSvm]
        public static object BoxInteger(int x)
        {
            object obj = x;
            return obj;
        }

        [TestSvm]
        public static object TrickyBox(int x)
        {
            if (x == 5)
            {
                return x;
            }
            return 42;
        }

        // [TestSvm]
        // [Ignore("expected reference, but got System.Nullable")]
        public static object Box7() // TODO: check #do
        {
            int? x = 7;
            return x;
        }

        [Ignore("expected reference, but got System.Nullable")]
        public static object BoxNullable(int? x)
        {
            return x;
        }

        [Ignore("expected reference, but got System.Nullable")]
        public static bool AlwaysNull()
        {
            return BoxNullable(null) == null;
        }

        private static bool AlwaysTrueForNullable(int? x)
        {
            object obj = x;
            int? y = (int?) obj;
            return x == y;
        }

        [Ignore("expected reference, but got System.Nullable")]
        public static bool True1()
        {
            return AlwaysTrueForNullable(null);
        }

        [Ignore("expected reference, but got System.Nullable")]
        public static bool True2()
        {
            int? x = 55;

            return AlwaysTrueForNullable(x);
        }

        [TestSvm]
        public static bool True3()
        {
            int x = 42;
            object obj = x;
            int y = (int) obj;
            return x == y;
        }
    }

    [TestSvmFixture]
    public class UnboxGeneric<T>
    {
        // [TestSvm]
        // [Ignore("Unbox.any for generics is not implemented: no way to do `Nullable.GetUnderlyingType` for generic type")]
        public static T Cast(object o) // TODO: check #do
        {
            return (T) o;
        }
    }

    [TestSvmFixture]
    public class BoxUnboxWithGeneric<G, T, U, V>
        where G : IVirtual
        where T : class, IVirtual
        where U : struct, IVirtual
    {
        [Ignore("Insufficient information")]
        public static object BoxValueOrReference(G t)
        {
            object o = t;
            return o;
        }

        [TestSvm]
        public static object BoxReference(T t)
        {
            object o = t;
            return o;
        }

        // [TestSvm]
        // [Ignore("Subtype analysis is not so smart (it doesn't check IVirtual constraint)")]
        public static object BoxValue(U t) // TODO: check #do
        {
            object o = t;
            return o;
        }

        [Ignore("Insufficient information")]
        public static object BoxValue(V t)
        {
            object o = t;
            return o;
        }
    }
}
