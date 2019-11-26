using System;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public sealed class Arithmetics
    {
        // 7 + n
        [Ignore("Reinterpretation is not implemented")]
        public static int ArithmeticsMethod1(int n, int m)
        {
            return -((n - m) + (m - n) + (1 + m + 2 + 0 - m + 4 + m) - (m + n)) + 14 + (n * (5 - 4) + (5 - 7 + m / m) * n) / m;
        }

        // 0
        [TestSvm]
        public static int ArithmeticsMethod2(int a, int b)
        {
            return a + b - a - b;
        }

        // c - 11
        [TestSvm]
        public static int ArithmeticsMethod3(int a, int b, int c)
        {
            return (a + b + c) - 1 * (a + b + 8) - 3;
        }

        // 6*n - 126826
        [Ignore("Reinterpretation is not implemented")]
        public static int ArithmeticsMethod4(int n, int m)
        {
            return (n + n + n + n + n + n - 2312) + m * m * m / (2 * n - n + 3 * n - 4 * n + m * m * m) - 124515;
        }

        // Expecting true
        [TestSvm]
        public static bool IncrementsWorkCorrect(int x)
        {
            int xorig = x;
            x = x++;
            int x1 = x;
            x++;
            int x2 = x;
            x = ++x;
            int x3 = x;
            ++x;
            int x4 = x;
            return x1 == xorig & x2 == xorig + 1 & x3 == xorig + 2 & x4 == xorig + 3;
        }

        // Expecting true
        [TestSvm]
        public static bool Decreasing(int x)
        {
            int x1 = x + 1;
            int x2 = x + 2;
            return x2 - x1 == 1;
        }

        [TestSvm]
        public static int CheckedUnchecked(int x0, int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8, int x9)
        {
            return checked(x0 + unchecked(x1 + checked(x2 + x3 + x4)) + unchecked(x5 - x6 * x7));
        }

        private static int CheckOverflow0(int x0, int x1)
        {
            return checked (2147483620 + x0 + 2147483620) + x1;
        }

        // Expecting overflow error
        [Ignore("Reinterpretation is not implemented")]
        public static int CheckOverflow1(int x1)
        {
            return CheckOverflow0(2147483620, 2147483620 + x1);
        }

        // Expecting overflow error
        [Ignore("Reinterpretation is not implemented")]
        public static int CheckOverflow2(int x1)
        {
            int x = 1000 * 1000 * 1000;
            int y = x;
            return checked ((x + y) * 2);
        }

        // Expecting Infinity + x1
        [TestSvm]
        public static double CheckOverflow3(double x1)
        {
            double x = 1.5E+308;
            return checked ((x / 0.01) + x1);
        }

        // Expecting devide by zero error
        [Ignore("Reinterpretation is not implemented")]
        public static int CheckDivideByZeroException0(int x1)
        {
            int x = 255;
            int y = 0;
            return (x / y + x1);
        }

        // Expecting 2000000000 + x1 + 2000000000
        [TestSvm]
        public static int CheckOrder(int x1)
        {
            int x = 2000000000;
            int y = x;
            return checked (x + x1 + y);
        }

        // Expecting a
        [TestSvm]
        public static int ShiftLeftOnZero(int a)
        {
            return (a << 0) >> 0;
        }

        // Expecting 0.0
        [TestSvm]
        public static double ZeroShift(int a)
        {
            return 0 << a >> a;
        }

        // Expecting a << b
        [TestSvm]
        public static int DefaultShift(int a, int b)
        {
            return a << b;
        }

        // Expecting a << 32
        [TestSvm]
        public static Int64 SumShifts(Int64 a)
        {
            return (a << 31) + (a << 31);
        }

        // Expecting 0
        [TestSvm]
        public static Int32 ShiftSum(Int32 a)
        {
            return (a + a) << 31;
        }

        // Expecting a << 19
        [TestSvm]
        public static Int32 MultiplyOnShift1(Int16 a)
        {
            return (a << 17) * 4;
        }

        // Expecting a << 16
        [TestSvm]
        public static Int32 MultiplyOnShift2(Int16 a)
        {
            return (a << 14) * 4;
        }

        // Expecting 0
        [TestSvm]
        public static Int32 ShiftMultiplication(Int16 a)
        {
            return (a * 512) << 23;
        }

        // Expecting (a >> 20) / 1024
        [TestSvm]
        public static int ShiftDevision1(byte a)
        {
            return (a >> 20) / 1024;
        }

        // Expecting (a / 512) >> 12
        [TestSvm]
        public static int ShiftDevision2(int a)
        {
            return (a / 512) >> 12;
        }

        // Expecting 0
        [TestSvm]
        public static uint ShiftDevision3(uint a)
        {
            return (a >> 22) / 1024;
        }

        // Expecting a >> 41
        [TestSvm]
        public static ulong ShiftDevision4(ulong a)
        {
            return (a >> 31) / 1024;
        }

        // Expecting 0
        [TestSvm]
        public static int ShiftSumOfShifts1(int a)
        {
            return ((a << 30) + (a << 30)) << 2;
        }

        // Expecting a << 34
        [TestSvm]
        public static long ShiftSumOfShifts2(long a)
        {
            return ((a << 31) + (a << 31)) << 2;
        }

        // Expecting -2147483648
        [TestSvm]
        public static int ConcreteShift()
        {
            return 2 << 30;
        }

        // Expecting 0
        [TestSvm]
        public static int MultiplyShifts1(int a, int c)
        {
            return ((a + a) << 14) * (c << 17);
        }

        // Expecting (a * c) << 29
        [TestSvm]
        public static int MultiplyShifts2(int a, int c)
        {
            return ((a + a) << 11) * (c << 17);
        }

        // Expecting (a << 6) / 4
        [TestSvm]
        public static int ShiftWithDivAndMul(int a)
        {
            return ((a * 16) << 2) / 4;
        }

        // Expecting (int64)(a >> 15 >> 18)
        [TestSvm]
        public static Int64 DoubleShiftRight(int a)
        {
            return (a >> 15) >> 18;
        }

        // log(x)
        [TestSvm]
        public static double LogMethod1(double x)
        {
            return Math.Log(x);
        }

        // log(x + y)
        [TestSvm]
        public static double LogMethod2(double x, double y)
        {
            return Math.Log(x + y);
        }

        // 0
        [TestSvm]
        public static double LogMethod3()
        {
            return Math.Log(1);
        }

        // log(1 + log(x))
        [TestSvm]
        public static double LogMethod4(double x)
        {
            return Math.Log(1 + Math.Log(x));
        }

        // -Infinity
        [TestSvm]
        public static double LogMethod5()
        {
            return Math.Log(0);
        }

        // NaN
        [TestSvm]
        public static double LogMethod6()
        {
            return Math.Log(-1);
        }

        [TestSvm]
        public static double LogMethod7(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Log(y);
        }

        // sqrt(x)
        [TestSvm]
        public static double SqrtMethod1(double x)
        {
            return Math.Sqrt(x);
        }

        // 2
        [TestSvm]
        public static double SqrtMethod2()
        {
            return Math.Sqrt(4);
        }

        [TestSvm]
        public static double SqrtMethod3(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Sqrt(y);
        }

        // NaN
        [TestSvm]
        public static double SqrtMethod4()
        {
            return Math.Sqrt(-1);
        }

        // 1
        [TestSvm]
        public static double ExpMethod1()
        {
            return Math.Exp(0);
        }

        // exp(x)
        [TestSvm]
        public static double ExpMethod2(double x)
        {
            return Math.Exp(x);
        }

        // 1
        [TestSvm]
        public static double PowMethod1(double x)
        {
            return Math.Pow(x, Math.Log(1));
        }

        // 1
        [TestSvm]
        public static double PowMethod2(double x)
        {
            return Math.Pow(1, x);
        }

        // 25
        [TestSvm]
        public static double PowMethod3()
        {
            return Math.Pow(5, 2);
        }

        //pow(x, y)
        [TestSvm]
        public static double PowMethod4(double x, double y)
        {
            return Math.Pow(x, y);
        }

        [TestSvm]
        public static double PowMethod5(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Pow(y, 2);
        }

        [TestSvm]
        public static double PowMethod6(double x)
        {
            double y;
            double z;
            if (x >= 0) y = x;
            else y = -x;
            if (x >= 8) z = y;
            else z = x;
            return Math.Pow(y, z);
        }

        // x + y
        [TestSvm]
        public static double PowMethod7(double x, double y)
        {
            return Math.Pow(x + y, 1);
        }

        // pow(2, x)
        [TestSvm]
        public static double PowMethod8(double x)
        {
            return Math.Pow(2, x);
        }

        // NaN
        [TestSvm]
        public static double PowMethod9(double x)
        {
            return Math.Pow(Double.NaN, x);
        }

        // NaN
        [TestSvm]
        public static double PowMethod10(double x, double y)
        {
            return Math.Pow(x, Math.Log(-1));
        }

        // 0
        [TestSvm]
        public static double PowMethod11(double x)
        {
            return Math.Pow(0, x);
        }

        // 1
        [TestSvm]
        public static double PowMethod12()
        {
            return Math.Pow(Double.PositiveInfinity, 0);
        }

        // 0
        [TestSvm]
        public static double PowMethod13()
        {
            return Math.Pow(0, Double.PositiveInfinity);
        }

        // Infinity
        [TestSvm]
        public static double PowMethod14(double x)
        {
            return Math.Pow(Double.PositiveInfinity, 5);
        }

        [TestSvm]
        public static double PowMethod15(double x, double y)
        {
            return Math.Pow(Double.PositiveInfinity, x + y);
        }

        [TestSvm]
        public static double PowMethod16(double x)
        {
            return Math.Pow(x, Double.NegativeInfinity);
        }

        // -Infinity
        [TestSvm]
        public static double PowMethod17()
        {
            return Math.Pow(Double.NegativeInfinity, 9);
        }

        // 0
        [TestSvm]
        public static double AcosMethod()
        {
            return Math.Acos(1);
        }

        // arcsin(x)
        [TestSvm]
        public static double AsinMethod(double x)
        {
            return Math.Asin(x);
        }

        // arctan(x - y)
        [TestSvm]
        public static double AtanMethod(double x, double y)
        {
            return Math.Atan(x - y);
        }

        // ceiling(x)
        [TestSvm]
        public static double CeilingMethod(double x)
        {
            return Math.Ceiling(x);
        }

        // NaN
        [TestSvm]
        public static double CosMethod()
        {
            return Math.Cos(Double.NaN);
        }

        // NaN
        [TestSvm]
        public static double CoshMethod()
        {
            return Math.Cosh(Math.Log(0));
        }

        // floor(x)
        [TestSvm]
        public static double FloorMethod(double x)
        {
            return Math.Floor(x);
        }

        // NaN
        [TestSvm]
        public static double SinMethod()
        {
            return Math.Sin(Double.PositiveInfinity);
        }

        // NaN
        [TestSvm]
        public static double TanMethod()
        {
            return Math.Tan(Double.NegativeInfinity);
        }

        [TestSvm]
        public static double SinhMethod(double x)
        {
            double y;
            if (x > 0)
            {
                y = x;
            }
            else
            {
                y = -x;
            }

            return Math.Sinh(y);
        }

        // tanh(x)
        [TestSvm]
        public static double TanhMethod(double x)
        {
            return Math.Tanh(x);
        }

        // 7
        [TestSvm]
        public static double RoundMethod()
        {
            return Math.Round(6.7);
        }

        // abs(x)
        [TestSvm]
        public static double AbsMethod(double x) // TODO: static Object[] disappears #do
        {
            return Math.Abs(x);
        }

        // 5.9
        [Ignore("hasKey indexes randomly changes from run to run")]
        public static float AbsSingleMethod()
        {
            return Math.Abs(Convert.ToSingle(-5.9));
        }

        // NaN
        [TestSvm]
        public static double Atan2Method1(double x)
        {
            return Math.Atan2(x, Double.NaN);
        }

        // NaN
        [TestSvm]
        public static double Atan2Method2(double x)
        {
            return Math.Atan2(Double.PositiveInfinity, x);
        }

        // 0
        [TestSvm]
        public static double Atan2Method3()
        {
            return Math.Atan2(1, Double.PositiveInfinity);
        }
    }
}
