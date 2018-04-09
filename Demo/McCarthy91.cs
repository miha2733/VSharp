using System;

namespace Demo
{
    public static class McCarthy91
    {
        public static int McCarthy(int n)
        {
            return n > 100 ? n - 10 : McCarthy(McCarthy(n + 11));
        }

        public static void CheckMc91(int x)
        {
            if (x <= 96 && McCarthy(x + 5) != 91)
            {
                throw new Exception();
            }
        }
    }
}
