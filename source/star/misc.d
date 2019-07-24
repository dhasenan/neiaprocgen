module star.misc;

T min(T)(T a, T b)
{
    if (a < b) return a;
    return b;
}

T max(T)(T a, T b)
{
    if (a > b) return a;
    return b;
}

enum parsecToLightyear = 3.26156;

