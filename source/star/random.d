module star.random;

import std.algorithm;
import std.random;
import std.math;
import star.point;

Mt19937_64 rndGen;
alias myRandom = rndGen;

void seed(TRng)(ref TRng rng, real x, real y, real z, ushort layer, ulong seed)
{
    // Manipulate our inputs to a sequence of ulongs
    union R
    {
        real r;
        ubyte[10] b;
        ulong[2] u;
    }

    R xr = {r: x};
    R yr = {r: y};
    R zr = {r: z};

    ulong[5] s;
    s[0] = xr.u[0];
    s[1] = yr.u[0];
    s[2] = zr.u[0];
    s[3] = (xr.u[1] << 48) | (yr.u[1] << 32) | (zr.u[1] << 16) | layer;
    s[4] = seed;

    // An infinite range that provides seed data based on our parameters
    struct SeedRange(ulong len)
    {
        ulong[len] s;
        ulong i;

        ulong front()
        {
            return s[i];
        }

        void popFront()
        {
            i++;
            if (i >= s.length)
            {
                i = 0;
                foreach (ref t; s)
                {
                    t = t << 11 | t >> 53;
                    t = t * 53;
                }
            }
        }

        enum empty = false;
    }

    rng.seed(SeedRange!(s.length)(s, 0));
}

real gaussRandom()(real mean, real standardDeviation)
{
    return gaussRandom(mean, standardDeviation, myRandom);
}

real gaussRandom(TRnd)(real mean, real standardDeviation, ref TRnd rnd)
{
    real u = 0, v = 0, r = 0;
    while (r == 0 || r >= 1)
    {
        u = (uniform01!real(rnd) * 2) - 1;
        v = (uniform01!real(rnd) * 2) - 1;
        r = u * u + v * v;
    }
    auto gauss = u * (-2 * log(r) / r) ^^ 0.5;
    return mean + gauss * standardDeviation;
}

real[] normalizeAverage(real[] v)
{
    auto avg = v.sum / v.length;
    v[] /= avg;
    return v;
}

real[] normalizeSum(real[] v, real total)
{
    auto avg = total / v.sum;
    v[] *= avg;
    return v;
}

real clampNegative(real d)
{
    if (d >= 0) return d;
    return 0;
}

Point randomPointInShell(TRng)(real inner, real outer, ref TRng rng)
{
    auto p = randomPointOnSphere(1, rng);
    auto radius = uniform(inner^^2, outer^^2, rng)^^0.5;
    return p * radius;
}
Point randomPointOnSphere(TRng)(real radius, ref TRng rng)
{
    // Marsaglia 1972 method
    // Pick x1 and x2 such that x1**2 + x2^^2 < 1
    real x1 = uniform(cast(real)-1, cast(real)1);
    real f = abs(1 - x1 ^^ 2) ^^ 0.5;
    real x2 = uniform(nextUp(-f), f);
    return Point(
            2 * x1 * sqrt(1 - x1^^2 - x2^^2),
            2 * x2 * sqrt(1 - x1^^2 - x2^^2),
            1 - 2 * (x1^^2 + x2^^2));
}

unittest
{
    randomPointInShell(1, 12.5, myRandom);
}
