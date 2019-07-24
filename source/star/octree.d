module star.octree;

import star.point;
import std.exception;
import std.experimental.logger;
import std.typecons;

version (unittest) shared static this()
{
    sharedLog.logLevel = LogLevel.trace;
    import etc.linux.memoryerror;
    registerMemoryErrorHandler;
}

__gshared static Logger octlog;
shared static this()
{
    import std.stdio;
    octlog = new FileLogger(stderr, LogLevel.warning);
}

struct OctreeRange(T)
{
    Octree!T tree, top, last;
    ulong index = 0;
    Volume volume;

    this(Octree!T tree)
    {
        this.tree = tree;
        if (tree !is null)
        {
            this.top = tree.parent;
            this.volume = tree.volume;
            findFirstNode;
        }
    }

    this(Octree!T tree, Volume volume)
    {
        this.tree = tree;
        this.top = tree is null ? null : tree.parent;
        this.volume = volume;
        findFirstNode;
        if (tree !is null)
        {
            this.top = tree.parent;
            findFirstNode;
        }
    }

    private void findFirstNode()
    {
        while (tree && !tree.leaf)
        {
            foreach (i, child; tree.children)
            {
                if (child !is null)
                {
                    octlog.tracef("descending to child %s node %s", i, child.id);
                    tree = child;
                    break;
                }
            }
        }
        if (tree && tree.contents.length == 0)
        {
            octlog.tracef("first leaf of %s is empty, popping", tree.id);
            popFront;
        }
        if (tree !is null)
            octlog.tracef("resting at node %s with %s contents, started at position %s", tree.id,
                    tree.contents.length, index);
    }

    typeof(this) save()
    {
        return this;
    }

    Tuple!(Point, T) front()
    {
        enforce(!empty);
        octlog.tracef("node %s grabbing item %s of %s", tree.id, index, tree.contents.length);
        return tree.contents[index];
    }

    bool empty()
    {
        return tree is null || tree is top;
    }

    void popFront()
    {
        index++;
        while (tree !is null && tree.contents.length > index)
        {
            if (volume.contains(tree.contents[index][0]))
            {
                break;
            }
            index++;
        }
        if (tree.contents.length > index)
        {
            octlog.tracef("tree %s content %s of %s", tree.id, index, tree.contents.length);
            return;
        }

        index = 0;
        do
        {
            // First, we try going up the tree until we find a node we haven't exhausted.
            auto last = tree;
            auto t = tree.parent;
            while (t !is null && t !is top)
            {
                typeof(t) next = null;
                size_t nextI = 0;
                foreach_reverse (i, c; t.children)
                {
                    if (c is null) continue;
                    if (c is last)
                    {
                        // We've found where we iterated up from.
                        if (next is null)
                        {
                            nextI = i;
                            last = t;
                            t = t.parent;
                            if (t is top)
                            {
                                tree = t;
                                octlog.tracef("reached the top; finished iteration");
                                return;
                            }
                        }
                        break;
                    }
                    next = c;
                }
                if (next is null)
                {
                    octlog.tracef("exhausted subtree %s, ascending to %s", t.id,
                            t.parent is null ? -1 : t.parent.id);
                    last = t;
                    tree = t = t.parent;
                    continue;
                }
                octlog.tracef("taking sibling %s (%s)", nextI, next.id);
                last = t;
                t = next;
                while (!t.leaf)
                {
                    foreach (i, child; t.children)
                    {
                        if (child !is null)
                        {
                            octlog.tracef("descending to child %s (%s)", i, child.id);
                            last = t;
                            t = child;
                            break;
                        }
                    }
                }
                break;
            }
            tree = t;
        }
        while (tree !is null && tree !is top && tree.contents.length == 0);
        octlog.tracef("top? %s null? %s contents: %s",
                tree is top, tree is null, tree is null ? 0 : tree.contents.length);
    }
}

class Octree(T)
{
    static long maxId;

    long id;
    real stride;
    Point mid;
    Volume volume;

    this(Volume volume, real stride, Octree!T parent = null)
    {
        this.volume = volume;
        this.stride = stride;
        this.parent = parent;
        this.mid = volume.midpoint;
        this.id = maxId++;
    }

    Octree!T parent;
    Octree!T[8] children;
    Tuple!(Point, T)[] contents;

    T opIndex(Point p)
    {
        if (leaf)
        {
            foreach (t; contents)
            {
                if (t[0] == p)
                {
                    return t[1];
                }
            }
            return T.init;
        }
        foreach (child; children)
        {
            if (child !is null && p in child.volume)
            {
                return child[p];
            }
        }
        return T.init;
    }

    void remove(Point p)
    {
        if (leaf)
        {
            foreach (i, t; contents)
            {
                if (t[0] == p)
                {
                    contents[i] = contents[$-1];
                    contents.length--;
                }
            }
            return;
        }
        if (p in volume)
        {
            foreach (child; children) if (child) child.remove(p);
        }
    }

    T opIndexAssign(T value, Point p)
    {
        insert(p, value);
        return value;
    }

    void insert(Point p, T value)
    {
        import std.format : format;
        assert(p in volume, format("tried inserting %s to volume %s".format(p, volume)));
        if (leaf)
        {
            contents ~= tuple(p, value);
            return;
        }
        uint c;
        Point a, b;
        if (p.x < mid.x)
        {
            a.x = volume.start.x;
            b.x = mid.x;
        }
        else
        {
            a.x = mid.x;
            b.x = volume.end.x;
            c |= 0b100;
        }

        if (p.y < mid.y)
        {
            a.y = volume.start.y;
            b.y = mid.y;
        }
        else
        {
            a.y = mid.y;
            b.y = volume.end.y;
            c |= 0b010;
        }

        if (p.z < mid.z)
        {
            a.z = volume.start.z;
            b.z = mid.z;
        }
        else
        {
            a.z = mid.z;
            b.z = volume.end.z;
            c |= 0b001;
        }

        assert(p.inVolume(a, b));
        auto kid = children[c];
        if (kid is null)
        {
            kid = children[c] = new Octree!T(Volume(a, b), stride, this);
        }
        kid.insert(p, value);
    }

    bool leaf()
    {
        return volume.end.x - volume.start.x <= stride;
    }

    auto range(Volume v)
    {
        if (leaf)
        {
            return OctreeRange!T(this, v);
        }
        auto count = 0;
        Octree!T c;
        foreach (child; children)
        {
            if (child !is null && v.intersects(child.volume))
            {
                c = child;
                count++;
            }
        }
        if (count == 0)
        {
            return OctreeRange!T(null, v);
        }
        if (count == 1)
        {
            return OctreeRange!T(c, v);
        }
        return OctreeRange!T(this, v);
    }

    OctreeRange!T range()
    {
        return OctreeRange!T(this);
    }

    void visit(Volume v, void delegate(Point, T) dg)
    {
        // Is it in bounds?
        if (!v.intersects(volume)) return;
        if (leaf)
        {
            foreach (t; contents)
            {
                if (t[0] in v)
                {
                    dg(t[0], t[1]);
                }
            }
        }
        else
        {
            foreach (child; children)
            {
                if (child)
                {
                    child.visit(v, dg);
                }
            }
        }
    }

    void print(size_t depth)
    {
        import std.stdio;
        char[] c = new char[depth];
        c[] = ' ';
        writefln("%soctree %s", c, volume);
        foreach (child; children)
        {
            if (child !is null) child.print(depth + 1);
        }
        foreach (v; contents)
        {
            writefln("%s item %s -> %s", c, v[0], v[1]);
        }
    }
}

/*
@("Octree constrained range")
unittest
{
    import std.range;
    import std.format;
    auto t = new Octree!int(Volume(Point(-1000, -1000, -1000), Point(1000, 1000, 1000)), 1);
    Point a = {4,4,4};
    Point b = {5,5,5};
    Point c = {7,7,7};
    t.insert(a, 10);
    t.insert(b, 11);
    t.insert(c, 12);
    foreach (p, v; t.range(Volume(Point(0, 0, 0), Point(4.5, 4.5, 4.5))))
    {
        infof("%s -> %s", p, v);
    }
    auto len = t.range(Volume(Point(0, 0, 0), Point(4.5, 4.5, 4.5))).walkLength;
    assert(len == 1, "expected 1 item, got %s".format(len));
    len = t.range(Volume(Point(0, 0, 0), Point(6.5, 6.5, 6.5))).walkLength;
    assert(len == 2, "expected 2 items, got %s".format(len));
    len = t.range(Volume(Point(0, 0, 0), Point(6.5, 6.5, 6.5))).walkLength;
    assert(len == 3, "expected 3 items, got %s".format(len));
}

@("Octree misc")
unittest
{
    import std.format;
    auto t = new Octree!int(Volume(Point(-1000, -1000, -1000), Point(1000, 1000, 1000)), 200);
    Point a = {4,4,4};
    Point b = {5,4,4};
    Point c = {7,4,4};
    t.insert(a, 10);
    t.insert(b, 11);
    t.insert(c, 12);

    uint count = 0;
    t.visit(Volume(Point(0, 0, 0), Point(5.1, 5, 5)), (p, v)
        {
            count++;
            if (p == a) assert(v == 10, "value for a: expected %s, got %s".format(10, v));
            else if (p == b) assert(v == 11, "value for b: expected %s, got %s".format(11, v));
            else assert(false, "unexpected point: %s -> %s".format(p, v));
        });
    assert(count == 2);

    assert(t[a] == 10);
    assert(t[b] == 11);
    assert(t[c] == 12);
    assert(t[Point(12, 999, -12.4)] == 0);

    Point[] points = [a, b, c];
    bool[] found = new bool[points.length];
    import std.stdio;
    foreach (pair; t.range)
    {
        writeln(pair);
        foreach (i, p; points)
        {
            if (pair[0] == p)
            {
                found[i] = true;
            }
        }
    }
    writeln(found);
    foreach (i, f; found)
    {
        assert(f, format("failed to find point %s", points[i]));
    }
}
*/


//version(DummyPoissonSample)
Octree!T poissonSample(T, alias factory, TRng)(
        ref TRng rng,
        Octree!T obstacles,
        Octree!T output,
        Volume volume,
        real distance,
        uint attempts = 5)
{
    enforce(output.volume.contains(volume.start));
    enforce(output.volume.contains(volume.end));

    ulong toPlace = cast(ulong)(volume.totalVolume / ((distance * 1.5) ^^ 3));
    foreach (i; 0..toPlace)
    {
        import std.random;
        import star.random;
        Point p;
        p.x = uniform(volume.start.x, volume.end.x, rng);
        p.y = uniform(volume.start.y, volume.end.y, rng);
        p.z = uniform(volume.start.z, volume.end.z, rng);
        output[p] = factory(p);
        if ((i + 1) % 1000 == 0) infof("placed %s stars", i + 1);
    }
    return output;
}
version(none)
Octree!T poissonSample(T, alias factory, TRng)(
        ref TRng rng,
        Octree!T obstacles,
        Octree!T output,
        Volume volume,
        real distance,
        uint attempts = 30)
{
    import std.exception;
    import std.random;
    import star.random;

    enforce(output.volume.contains(volume.start));
    enforce(output.volume.contains(volume.end));

    ulong toPlace = cast(ulong)(volume.totalVolume / ((distance * 1.5) ^^ 3));
    ulong count = 0;
    Point[] active;

    import std.stdio;
    writefln("poisson sampling about %s items in volume %s, min distance %s", toPlace, volume,
            distance);

    bool tryPlace()
    {
        auto nextIndex = uniform(0, active.length, rng);
        // TODO fix this to use fewer GC allocations
        auto next = active[nextIndex];
        foreach (i; 0..attempts)
        {
            import std.range, std.algorithm;
            auto delta = randomPointInShell(distance, distance*2, rng);
            auto p = next + delta;
            if (!volume.contains(p))
            {
                continue;
            }
            auto margin = Volume.radius(p, distance);
            octlog.infof("checking obstacles in %s for point %s", margin, p);
            auto found = chain(obstacles.range(margin), output.range(margin))
                .filter!(x => x[0].dist(p) <= distance);
            if (found.empty)
            {
                auto tmp = factory(p);
                output[p] = tmp;
                active ~= p;
                count++;
                if (count % 1_000 == 0)
                {
                    import std.stdio;
                    octlog.infof("placed star %s of %s", count, toPlace);
                    if (count > toPlace)
                    {
                        octlog.warningf("went over placement limit, stopping");
                        return output;
                    }
                }
                return true;
            }
        }
        active[nextIndex] = active[$-1];
        active.length--;
        version(none)infof("removed active item %s at %s, %s elements remaining, %s placed",
                next, nextIndex, active.length, count);
        return false;
    }

    foreach (i; 0..attempts)
    {
        auto origin = Point(
                uniform(volume.start.x, volume.end.x, rng),
                uniform(volume.start.y, volume.end.y, rng),
                uniform(volume.start.z, volume.end.z, rng));
        active ~= origin;
        if (tryPlace()) break;
        version(none)infof("failed to place initial point (attempt %s)", i);
    }

    while (active.length && count < toPlace)
    {
        tryPlace;
        //infof("tried to place, have %s active points", active.length);
    }

    return output;
}

/+
@("poisson sample only one point")
unittest
{
    import star.random;
    import std.range;

    // No obstacles.
    auto obstacles = new Octree!int(Volume.radius(100), 1);
    auto output = new Octree!int(Volume.radius(100), 1);

    // Radius of 1 => can only insert the initial point
    poissonSample!(int, x => 10)(myRandom, obstacles, output, Volume.radius(0.5), 1);
    assert(output.range.walkLength == 1);
}

@("poisson sample obstacles block everything")
unittest
{
    import star.random;
    import std.range;

    // Obstacles at the edges, can't insert anything
    auto obstacles = new Octree!int(Volume.radius(100), 1);
    obstacles[Point(0.6, 0, 0)] = 1;
    obstacles[Point(-0.6, 0, 0)] = 1;
    obstacles[Point(0, 0.6, 0)] = 1;
    obstacles[Point(0, -0.6, 0)] = 1;
    obstacles[Point(0, 0, 0.6)] = 1;
    obstacles[Point(0, 0, -0.6)] = 1;
    auto output = new Octree!int(Volume.radius(100), 1);

    poissonSample!(int, x => 10)(myRandom, obstacles, output, Volume.radius(0.5), 1);
    import std.format;
    assert(output.range.walkLength == 0,
            "expected empty output but got %s items".format(output.range.walkLength));
}

@("poisson sample larger volume")
unittest
{
    import star.random;
    import std.range;

    // No obstacles.
    auto obstacles = new Octree!int(Volume.radius(100), 1);
    auto output = new Octree!int(Volume.radius(100), 1);

    // Radius of 10 => can insert about 10×10×10 points, or 1000
    // Should be able to insert at least 5×5×5?
    auto result = poissonSample!(int, x => 10)(myRandom, obstacles, output, Volume.radius(5), 1);
    assert(result is output, "output octree was wrong");
    auto resultCount = output.range.walkLength;
    import std.format;
    assert(resultCount <= 1000 && resultCount >= 125,
            "expected result count between 125 and 1000, got %s".format(resultCount));
}
+/

version(none)
@("octree poisson sample result check")
unittest
{
    import std.algorithm, std.range;
    Volume bound = Volume(Point(264.469, 23267.3, 377.453), Point(824.94, 23827.7, 937.924));
    Point[] points = [
        // The failure case requires all four of these points.
        Point(319.825, 23651.4, 657.356),
        Point(266.056, 23666, 657.06),
        Point(330.391, 23684, 643.476),
        Point(320.707, 23713.8, 657.424),
        Point(341.877, 23678, 636.371),
    ];

// poisson sampling about 13241 items in volume Volume((264.469, 23267.3, 377.453), (824.94, 23827.7, 937.924)), min distance 15.7938
    auto tree = new Octree!int(Volume(Point(0, 22000, 0), Point(1200, 25000, 1200)), 10);
    enum minDist = 15.7939;
    uint failureCount = 0;
    foreach (i, a; points)
    {
        import std.stdio;
        if (!bound.contains(a))
        {
            infof("not contained: %s", a);
        }
        if (i == 0)
        {
            tree[a] = 1;
            continue;
        }
        auto matches = tree
            .range(Volume.radius(a, minDist))
            .map!(x => x[0])
            .array;
        auto shouldHave = points[0..i-1]
            .filter!(p => p.dist(a) <= minDist)
            .array;
        import std.format;
        assert(
                matches.length == shouldHave.length,
                format("point %s: expected %s, got %s", a, shouldHave.length, matches.length));
    }
    assert(failureCount == 0);
}
