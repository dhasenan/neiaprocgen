module star.point;

import std.math;
import star.misc;

struct Point
{
    real x = 0, y = 0, z = 0;

    real dist(Point other)
    {
        return ((x-other.x)^^2 + (y - other.y)^^2)^^0.5;
    }

    real magnitude()
    {
        return dist(Point(0, 0, 0));
    }

    Point opBinary(string op)(Point other) if (op == "+" || op == "-")
    {
        return Point(
                mixin("x", op, "other.x"),
                mixin("y", op, "other.y"),
                mixin("z", op, "other.z"),
                );
    }

    Point opBinary(string op)(real r) if (op == "*" || op == "/")
    {
        return Point(
                mixin("x", op, "r"),
                mixin("y", op, "r"),
                mixin("z", op, "r"),
                );
    }

    /** Is this point in the given axis-aligned bounding box?
      * Assumes it's left-inclusive, right-exclusive
      */
    bool inVolume(Point a, Point b)
    {
        return a.x <= x && a.y <= y && a.z <= z &&
            b.x > x && b.y > y && b.z > z;
    }

    enum Point invalid = Point(real.nan, real.nan, real.nan);
    enum Point origin = Point(0, 0, 0);

    string toString() const
    {
        import std.format : format;
        return "(%s, %s, %s)".format(x, y, z);
    }
}

struct Volume
{
    Point start, end;

    static Volume radius(real r)
    {
        return radius(Point(0, 0, 0), r);
    }
    static Volume radius(Point p, real r)
    {
        r = abs(r);
        return Volume(
                Point(p.x - r, p.y - r, p.z - r),
                Point(p.x + r, p.y + r, p.z + r));
    }

    this(Point a, Point b)
    {
        start.x = min(a.x, b.x);
        start.y = min(a.y, b.y);
        start.z = min(a.z, b.z);
        end.x = max(a.x, b.x);
        end.y = max(a.y, b.y);
        end.z = max(a.z, b.z);
    }

    bool intersects(Volume other)
    {
        // Two AABB volumes intersect if each axis-projection intersects.
        return
            intersectAxis(start.x, end.x, other.start.x, other.end.x) &&
            intersectAxis(start.y, end.y, other.start.y, other.end.y) &&
            intersectAxis(start.z, end.z, other.start.z, other.end.z);
    }

    Point midpoint()
    {
        return lerp(start, end, 0.5);
    }

    bool contains(Point p)
    {
        return
            start.x <= p.x &&
            start.y <= p.y &&
            start.z <= p.z &&
            end.x > p.x &&
            end.y > p.y &&
            end.z > p.z;
    }

    alias opIn_r = contains;

    real totalVolume()
    {
        return abs((start.x - end.x) * (start.y - end.y) * (start.z - end.z));
    }
}

real volume(Point a, Point b)
{
    return abs(a.x - b.x) * abs(a.y - b.y) * abs(a.z - b.z);
}

private bool intersectAxis(real a, real b, real c, real d)
{
    if (a > b)
    {
        auto tmp = a;
        a = b;
        b = tmp;
    }
    if (c > d)
    {
        auto tmp = c;
        c = d;
        d = tmp;
    }
    return c < b && d > a;
}

unittest
{
    Point a = {-1000, -1000, -1000};
    Point b = {1000, 1000, 1000};
    Point c = {4, 4, 4};
    assert(c.inVolume(a, b));
    assert(!c.inVolume(b, a));
    assert(!b.inVolume(a, c));
}

unittest
{
    Point a = {100, 200, 300};
    a = a * 5;
    assert(a == Point(500, 1000, 1500));
    a = a / 25;
    assert(a == Point(20, 40, 60));
}

unittest
{
    Point a = {1, 2, 3};
    Point b = {6, -2, 12};
    auto c = a + b;
    assert(c == Point(7, 0, 15));
    auto d = a - b;
    assert(d == Point(-5, 4, -9), c.toString);
}

/** Project the point to the xy plane. Find the angle with a ray from the origin toward (1, 0). */
real projectedAngleFromOrigin(Point p)
{
    if (p.y == 0)
    {
        return 0;
    }
    return acos(p.x / p.y);
}

Point lerp(Point a, Point b, real progress)
{
    return ((b - a) * progress) + a;
}

unittest
{
    auto a = Point(0, 0, 0);
    auto b = Point(10, 10, 10);
    auto c = Point(20, 100, 12);
    assert(lerp(a, b, 0.5) == Point(5, 5, 5), lerp(a, b, 0.5).toString);
    assert(lerp(b, a, 0.5) == Point(5, 5, 5));
    assert(lerp(b, c, 0.5) == Point(15, 55, 11), lerp(b, c, 0.5).toString);
}

@("Volume.contains")
unittest
{
    auto v = Volume(Point(5, 212, -1), Point(15, 223, 9));
    assert(v.contains(Point(10, 217, 4)));
    assert(v.contains(Point(14.9, 222.9, 8.9)));
    assert(v.contains(Point(5.01, 212.01, -0.9)));

    assert(!v.contains(Point(14.9, 222.9, 9.01)));
    assert(!v.contains(Point(14.9, 223.01, 8.9)));
    assert(!v.contains(Point(15.01, 222, 8.9)));
    assert(!v.contains(Point(5.01, 222, -1.01)));
    assert(!v.contains(Point(5.01, 211.9, 1.01)));
    assert(!v.contains(Point(4.99, 212.9, 1.01)));
}
